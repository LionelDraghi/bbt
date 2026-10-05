-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Settings;
with BBT.Created_File_List;             use BBT.Created_File_List;
with BBT.Writers;                       use BBT.Writers;
with BBT.Tests.Actions.File_Operations; use BBT.Tests.Actions.File_Operations;

with Ada.Calendar;
with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Spawn.Environments,
     Spawn.Processes,
     Spawn.Process_Listeners,
     Spawn.Processes.Monitor_Loop,
     Spawn.String_Vectors;

with GNAT.OS_Lib;

use Ada, BBT;

package body BBT.Tests.Actions is

   -- -----------------------------------------------------------------------
   procedure Put_Debug_Line
     (Item      : String;
      Location  : IO.Location_Type    := IO.No_Location;
      Verbosity : IO.Verbosity_Levels := IO.Debug;
      Topic     : IO.Extended_Topics  := IO.Step_Actions)
      renames IO.Put_Line;
   pragma Warnings (Off, Put_Debug_Line);

   -- --------------------------------------------------------------------------
   function Is_Success (I : Integer) return Boolean is
     (I = Integer (Command_Line.Success));

   function Entry_Exists (File_Name : String) return Boolean is
     (File_Name /= "" and then Exists (File_Name));

   use type BBT.Tests.Actions.File_Operations.File_Kind;
   function File_Exists (File_Name : String) return Boolean is
     (File_Name /= ""
      and then Exists (File_Name)
      and then Kind (File_Name) = Ordinary_File);

   function Dir_Exists (File_Name : String) return Boolean is
     (File_Name /= ""
      and then Exists (File_Name)
      and then Kind (File_Name) = Directory);

   -- --------------------------------------------------------------------------
   function Get_Expected (Step : Step_Type'Class) return Text is
      use type Text;
   begin
      if Step.Data.File_Content /= Empty_Text then
         -- File content provided in code fenced lines
         Put_Debug_Line ("======= Get_Expected returning Text" & Step.Data.File_Content'Image);
         return Step.Data.File_Content;

      elsif Step.Data.Object_File_Name /= Null_Unbounded_String
        and then File_Exists (+Step.Data.Object_File_Name)
      then
         -- The string denotes a file
         declare
            T : constant Text := Get_Text (+Step.Data.Object_File_Name);
         begin
            Put_Debug_Line ("======= Get_Expected returning content of file " & Step.Data.Object_File_Name'Image);
            -- Put_Text (Item => T);
            return T;
         end;

      elsif Step.Data.Object_String /= Null_Unbounded_String then
         -- The string is the content
         Put_Debug_Line ("======= Get_Expected returning string content" & Step.Data.Object_String'Image);
         return [1 => +Step.Data.Object_String];

      else
         -- Either the provided file content was null (two consecutive code
         -- fence marks), or there is an error somewhere in the scenario.
         -- But scenario errors are supposed to be caught during scenario
         -- analysis, and the run stopped before reaching this point,
         -- unless run with "--keep_going".
         -- In both cases, returning an Empty_Text seems to be the right
         -- things to do.
         Put_Debug_Line ("======= Get_Expected returning empty Text");
         return Empty_Text;
      end if;
   end Get_Expected;

   -- --------------------------------------------------------------------------
   --  Asynchronous execution of the commands, based on the Spawn library.
   --  The standard output and standard error of the command are appended
   --  to files by the listener, so that the output checks steps read the
   --  same files as before.
   --  When a scenario sends text to the command (Type_Text / Enter_Text
   --  steps), the command runs across steps, and each input step resets
   --  the output baseline: the output checks following an input step apply
   --  only to the output produced after it.

   Running       : Boolean := False;
   --  the last command is still running
   Process_Error : Integer := 0;
   --  set by the listener if the command could not be run

   Cmd_Output_Name : Unbounded_String;
   Cmd_Err_Name    : Unbounded_String;
   Merged          : Boolean := True;
   --  the standard error goes to the output file unless an error output
   --  file was requested

   Output_Bytes : Natural := 0;
   Err_Bytes    : Natural := 0;
   --  bytes written so far, used to detect when the command is quiet

   Output_Lines : Natural := 0;
   Err_Lines    : Natural := 0;
   --  complete lines written so far

   Output_Line_Offset : Natural := 0;
   Err_Line_Offset    : Natural := 0;
   --  lines produced before the last input step

   Input_Bytes : Natural := 0;
   --  bytes produced when the last input was sent, or at the start of
   --  the command if no input was sent yet: the response to check is
   --  the output produced after that point

   Last_Return_Code : Integer := 0;
   --  Set by the listener when the command terminates, read by the exit
   --  code checks of the following steps (each step is run by a separate
   --  Runner.Run_Step call).

   function Last_Exit_Code return Integer is (Last_Return_Code);

   -- ------------------------------------------------------------------------
   procedure Append_Data (File_Name : String;
                          Data      : Ada.Streams.Stream_Element_Array)
   is
      F : Ada.Streams.Stream_IO.File_Type;
   begin
      --  The file is opened and closed at each call, so that output checks
      --  can read it between two callbacks.
      if Ada.Directories.Exists (File_Name) then
         Ada.Streams.Stream_IO.Open
           (F, Ada.Streams.Stream_IO.Append_File, File_Name);
      else
         Ada.Streams.Stream_IO.Create
           (F, Ada.Streams.Stream_IO.Out_File, File_Name);
      end if;
      Ada.Streams.Stream_IO.Write (F, Data);
      Ada.Streams.Stream_IO.Close (F);
   end Append_Data;

   -- ------------------------------------------------------------------------
   procedure Write_Output (Data : Ada.Streams.Stream_Element_Array) is
      use Ada.Streams;
   begin
      Append_Data (To_String (Cmd_Output_Name), Data);
      Output_Bytes := @ + Natural (Data'Length);
      for C of Data loop
         if C = Character'Pos (Ada.Characters.Latin_1.LF) then
            Output_Lines := @ + 1;
         end if;
      end loop;
   end Write_Output;

   procedure Write_Error (Data : Ada.Streams.Stream_Element_Array) is
      use Ada.Streams;
   begin
      --  When the standard error is merged, it goes to the output file
      if Merged then
         Append_Data (To_String (Cmd_Output_Name), Data);
      else
         Append_Data (To_String (Cmd_Err_Name), Data);
      end if;
      Err_Bytes := @ + Natural (Data'Length);
      for C of Data loop
         if C = Character'Pos (Ada.Characters.Latin_1.LF) then
            Err_Lines := @ + 1;
         end if;
      end loop;
   end Write_Error;

   -- ------------------------------------------------------------------------
   type Process_Access is access all Spawn.Processes.Process;

   The_Process : Process_Access;
   --  The process run by the last Run_Cmd. A new process object is
   --  created at each Run_Cmd: reusing the same object for several
   --  starts works on POSIX but is broken on Windows (the Spawn
   --  monitor fails with ERROR_INVALID_HANDLE on the second start).
   --  It survives between steps only when interactive input is expected.

   package Retired_Processes is new Ada.Containers.Vectors
     (Positive, Process_Access);
   Retired : Retired_Processes.Vector;
   --  The process objects are never freed: the Spawn monitor keeps a
   --  pid to process map, with no removal on termination, so a freed
   --  object leaves a dangling pointer in the monitor. On macOS, where
   --  pids are quickly reused, waitpid then finds the stale map entry
   --  and writes the exit status into freed memory, corrupting the heap
   --  (erroneous memory access, bogus stack overflow, at random
   --  positions). The objects are small, and a run spawns at most a few
   --  hundreds of them: leaking them is the safe option, until the
   --  library cleans its map.

   Dead    : Boolean := False;
   --  the process has terminated, or could not be created at all

   Started_OK : Boolean := False;
   --  the process could be created (the Started callback was called);
   --  an error on a non created process leaves nothing to kill

   Monitor_Initialized : Boolean := False;
   --  at least one process was successfully started during this run;
   --  until then, the Spawn monitor has an empty process table, and
   --  Monitor_Loop crashes on Windows (null table dereference)



   package Listeners is
      type Listener is limited new
        Spawn.Process_Listeners.Process_Listener with null record;

      overriding procedure Started (Self : in out Listener);

      overriding procedure Standard_Output_Available
        (Self : in out Listener);

      overriding procedure Standard_Error_Available
        (Self : in out Listener);

      overriding procedure Finished
        (Self        : in out Listener;
         Exit_Status : Spawn.Processes.Process_Exit_Status;
         Exit_Code   : Spawn.Processes.Process_Exit_Code);

      overriding procedure Error_Occurred
        (Self          : in out Listener;
         Process_Error : Integer);

      overriding procedure Exception_Occurred
        (Self       : in out Listener;
         Occurrence : Ada.Exceptions.Exception_Occurrence);
   end Listeners;

   package body Listeners is
      overriding procedure Started (Self : in out Listener) is
         pragma Unreferenced (Self);
      begin
         Put_Debug_Line ("  command started");
         Started_OK         := True;
         Monitor_Initialized := True;
      end Started;

      overriding procedure Standard_Output_Available
        (Self : in out Listener)
      is
         pragma Unreferenced (Self);
         use type Ada.Streams.Stream_Element_Offset;
         Data : Ada.Streams.Stream_Element_Array (1 .. 2 ** 12);
         Last : Ada.Streams.Stream_Element_Offset;
         Ok   : Boolean := True;
      begin
         loop
            The_Process.Read_Standard_Output (Data, Last, Ok);
            exit when Last < Data'First;
            Write_Output (Data (1 .. Last));
         end loop;
      end Standard_Output_Available;

      overriding procedure Standard_Error_Available
        (Self : in out Listener)
      is
         pragma Unreferenced (Self);
         use type Ada.Streams.Stream_Element_Offset;
         Data : Ada.Streams.Stream_Element_Array (1 .. 2 ** 12);
         Last : Ada.Streams.Stream_Element_Offset;
         Ok   : Boolean := True;
      begin
         loop
            The_Process.Read_Standard_Error (Data, Last, Ok);
            exit when Last < Data'First;
            Write_Error (Data (1 .. Last));
         end loop;
      end Standard_Error_Available;

      overriding procedure Finished
        (Self        : in out Listener;
         Exit_Status : Spawn.Processes.Process_Exit_Status;
         Exit_Code   : Spawn.Processes.Process_Exit_Code)
      is
         pragma Unreferenced (Self);
         --  Fixme: a command terminated by a signal is not distinguished
         --  from a normal termination.
      begin
         Put_Debug_Line ("  command finished, exit code" & Exit_Code'Image
                         & ", status " & Exit_Status'Image);
         Running := False;
         Dead    := True;
         Last_Return_Code := Integer (Exit_Code);
      end Finished;

      overriding procedure Error_Occurred
        (Self          : in out Listener;
         Process_Error : Integer)
      is
         pragma Unreferenced (Self);
      begin
         Put_Debug_Line ("  command error" & Process_Error'Image);
         Running := False;
         BBT.Tests.Actions.Process_Error := Process_Error;
         if not Started_OK then
            --  the process could not be created: nothing remains
            --  to kill or reap, the object can be freed
            Dead := True;
         end if;
      end Error_Occurred;

      overriding procedure Exception_Occurred
        (Self       : in out Listener;
         Occurrence : Ada.Exceptions.Exception_Occurrence)
      is
         pragma Unreferenced (Self);
      begin
         Put_Debug_Line ("  exception in the listener: " &
                           Ada.Exceptions.Exception_Information
                             (Occurrence));
         Running := False;
         BBT.Tests.Actions.Process_Error := 1;
         if not Started_OK then
            Dead := True;
         end if;
      end Exception_Occurred;

   end Listeners;

   The_Listener : aliased Listeners.Listener;

   -- ------------------------------------------------------------------------
   Quiet_Step    : constant Duration := 0.02;
   Quiet_Window  : constant Duration := 0.10;
   Quiet_Timeout : constant Duration := 5.0;
   End_Timeout   : constant Duration := 10.0;

   procedure Safe_Monitor_Loop (Timeout : Duration) is
   begin
      if Monitor_Initialized then
         Spawn.Processes.Monitor_Loop (Timeout);
      else
         --  Until a process is successfully started, the Spawn monitor
         --  process table is not allocated, and Monitor_Loop crashes
         --  on Windows with a null table dereference. The command
         --  error, if any, has already been reported through the
         --  Error_Occurred callback before the crash point.
         begin
            Spawn.Processes.Monitor_Loop (Timeout);
         exception
            when Constraint_Error =>
               Put_Debug_Line ("  ignored monitor crash before the " &
                                 "first started command");
         end;
      end if;
   end Safe_Monitor_Loop;

   procedure Pump (Timeout : Duration);
   --  Process the pending events until the command is no more running,
   --  or until Timeout is elapsed. No timeout when Timeout = 0.0.

   procedure Pump (Timeout : Duration) is
      use type Ada.Calendar.Time;
      Deadline : constant Ada.Calendar.Time :=
                   Ada.Calendar.Clock + Timeout;
   begin
      while Running loop
         if Timeout > 0.0 and then Ada.Calendar.Clock >= Deadline then
            return;
         end if;
         Safe_Monitor_Loop (Quiet_Step);
      end loop;
   end Pump;

   -- ------------------------------------------------------------------------
   procedure Wait_Quiet is
      use type Ada.Calendar.Time;
      Deadline  : constant Ada.Calendar.Time :=
                    Ada.Calendar.Clock + Quiet_Timeout;
      Quiet     : Duration := 0.0;
      Last_Size : Natural;
   begin
      if not Running then
         Safe_Monitor_Loop (Quiet_Step);
         return;
      end if;
      --  The command output is considered complete when it stays quiet
      --  during a short window, or when it terminates. This is used before
      --  sending input to a command: whether its prompt is already there
      --  or it is still starting does not matter, as long as it is quiet.
      loop
         Last_Size := Output_Bytes + Err_Bytes;
         Safe_Monitor_Loop (Quiet_Step);
         if Output_Bytes + Err_Bytes = Last_Size then
            Quiet := @ + Quiet_Step;
         else
            Quiet := 0.0;
         end if;
         exit when Quiet >= Quiet_Window or else not Running
           or else Ada.Calendar.Clock >= Deadline;
      end loop;
   end Wait_Quiet;

   -- ------------------------------------------------------------------------
   procedure Wait_Response is
      use type Ada.Calendar.Time;
      Deadline   : constant Ada.Calendar.Time :=
                     Ada.Calendar.Clock + Quiet_Timeout;
      Quiet      : Duration := 0.0;
      Last_Size  : Natural;
   begin
      if not Running then
         Safe_Monitor_Loop (Quiet_Step);
         return;
      end if;
      --  The output of a running command is checked once the response to
      --  the last input, or the output produced since the command start
      --  when no input was sent yet, is complete: the command stays
      --  quiet AFTER having produced some output after that point, or
      --  terminates. Waiting for this output avoids considering the
      --  command quiet while it is still computing the consequence of
      --  the last input.
      --  Fixme: a command producing its output in bursts separated by
      --  more than the quiet window may be considered quiet too early.
      loop
         Last_Size := Output_Bytes + Err_Bytes;
         Safe_Monitor_Loop (Quiet_Step);
         if Output_Bytes + Err_Bytes = Last_Size then
            if Output_Bytes + Err_Bytes > Input_Bytes then
               Quiet := @ + Quiet_Step;
            end if;
         else
            Quiet := 0.0;
         end if;
         exit when not Running
           or else Quiet >= Quiet_Window
           or else Ada.Calendar.Clock >= Deadline;
      end loop;
   end Wait_Response;

   -- --------------------------------------------------------------------------
   procedure Dispose_Process is
      use type Ada.Calendar.Time;
      Deadline : constant Ada.Calendar.Time :=
                   Ada.Calendar.Clock + End_Timeout;
   begin
      if The_Process = null then
         return;
      end if;
      if not Dead then
         --  An error does not mean that the process died: kill it, and
         --  pump the monitor until it is reaped.
         Put_Debug_Line ("  disposing a still running command");
         The_Process.Kill_Process;
         while not Dead loop
            Safe_Monitor_Loop (Quiet_Step);
            exit when Ada.Calendar.Clock >= Deadline;
         end loop;
      end if;
      --  The object is kept in Retired for the whole run: the Spawn
      --  monitor never forgets a process (its pid map has no removal),
      --  so freeing the object would leave a dangling pointer there.
      Retired.Append (The_Process);
      The_Process := null;
   end Dispose_Process;

   -- --------------------------------------------------------------------------
   procedure Run_Cmd (Step                       :     Step_Type'Class;
                      Cmd                        :     String;
                      Output_Name                :     String;
                      Expected_Result            :     Run_Result;
                      Verbosity                  :     Verbosity_Levels;
                      Spawn_OK                   : out Boolean;
                      Return_Code                : out Integer;
                      Error_Output_Name          :     String := "";
                      Interactive_Input_Expected :     Boolean := False)
   is
      use GNAT.OS_Lib;
      -- Initial_Dir : constant String  := Current_Directory;
      Spawn_Arg      : constant Argument_List_Access
        := Argument_String_To_List (Cmd);
      Args           : Spawn.String_Vectors.UTF_8_String_Vector;

   begin
      Put_Debug_Line ("Run_Cmd " & Cmd & " in " & Settings.Exec_Dir &
                        ", output file = " & Output_Name &
                        (if Interactive_Input_Expected
                         then ", interactive input expected"
                         else ""));

      --  A previous command should have terminated before this one starts:
      --  all steps but the input ones wait for its end. If it is still
      --  running here, let's give it a chance, then kill it.
      if Running then
         Pump (End_Timeout);
         if Running then
            Put_Debug_Line ("  killing a still running command");
            The_Process.Kill_Process;
            Running := False;
         end if;
      end if;

      --  A new Spawn process object is used for each command: reusing
      --  the same object for several starts is broken on Windows.
      Dispose_Process;

      -- The first argument should be an executable (e.g. not a bash
      -- built-in)
      --
      -- If it is, replace it by the fully-qualified path name (spawn
      -- is implemented via execve, which on macOS doesn't
      -- understand PATH)
      Find_The_Executable_If_Any :
      declare
         Full_Path : GNAT.OS_Lib.String_Access;
      begin
         Full_Path := Locate_Exec_On_Path (Spawn_Arg.all (1).all);

         if Exists (Spawn_Arg.all (1).all) and then
           not Is_Executable_File (Spawn_Arg.all (1).all)
         then
            -- IO.Put_Line ("not exec", Verbosity => IO.Normal);
            Spawn_OK := False;
            Put_Step_Result
              (Step      => Step,
               Success   => Spawn_OK,
               Fail_Msg  => Spawn_Arg.all (1).all & " not executable",
               Loc       => Step.Location,
               Verbosity => Verbosity);
            return;

         elsif Full_Path = null then
            -- IO.Put_Line ("not found", Verbosity => IO.Normal);
            Spawn_OK := False;
            Put_Step_Result
              (Step      => Step,
               Success   => Spawn_OK,
               Fail_Msg  => Spawn_Arg.all (1).all & " not found",
               Loc       => Step.Location,
               Verbosity => Verbosity);
            return;

         else
            Put_Debug_Line
              ("Cmd " & Spawn_Arg.all (1).all & " = " & Full_Path.all);
            Free (Spawn_Arg.all (1));
            Spawn_Arg.all (1) := Full_Path;

         end if;
      end Find_The_Executable_If_Any;

      -- Removes quote on argument
      for I in 2 .. Spawn_Arg'Last loop
         -- Put_Debug_Line (">>>>>>>>>>" & Spawn_Arg.all (I).all & "<");
         declare
            Tmp : String := Spawn_Arg.all (I).all;
            I1  : constant Positive := (if Tmp (Tmp'First) = '"' then Tmp'First + 1
                                        else Tmp'First);
            I2  : constant Natural := (if Tmp (Tmp'Last) = '"' then Tmp'Last - 1
                                       else Tmp'Last);
            -- ugly and buggy
         begin
            Free (Spawn_Arg.all (I));
            Spawn_Arg.all (I) := new String'(Tmp (I1 .. I2));
         end;
         -- Put_Debug_Line ("===========" & Spawn_Arg.all (I).all & "<");
      end loop;

      --  Truncate the output files, and reset the state
      declare
         F : Ada.Streams.Stream_IO.File_Type;
      begin
         Ada.Streams.Stream_IO.Create
           (F, Ada.Streams.Stream_IO.Out_File, Output_Name);
         Ada.Streams.Stream_IO.Close (F);
      end;
      Cmd_Output_Name := To_Unbounded_String (Output_Name);
      Merged          := Error_Output_Name = "";
      if not Merged then
         declare
            F : Ada.Streams.Stream_IO.File_Type;
         begin
            Ada.Streams.Stream_IO.Create
              (F, Ada.Streams.Stream_IO.Out_File, Error_Output_Name);
            Ada.Streams.Stream_IO.Close (F);
         end;
         Cmd_Err_Name := To_Unbounded_String (Error_Output_Name);
      end if;
      Output_Bytes       := 0;
      Err_Bytes          := 0;
      Output_Lines       := 0;
      Err_Lines          := 0;
      Output_Line_Offset := 0;
      Err_Line_Offset    := 0;
      Input_Bytes        := 0;
      Process_Error      := 0;
      Running            := False;

      for I in 2 .. Spawn_Arg'Last loop
         Args.Append (Spawn_Arg.all (I).all);
      end loop;
      The_Process := new Spawn.Processes.Process;
      Dead        := False;
      Started_OK  := False;
      The_Process.Set_Program (Spawn_Arg.all (1).all);
      The_Process.Set_Arguments (Args);
      --  The command inherits the bbt environment, including the
      --  variables set or unset by the environment variable steps.
      --  The environment is rebuilt at each command: the Spawn library
      --  snapshot of the system environment is taken at elaboration
      --  time, and thus does not reflect those steps.
      declare
         Env : Spawn.Environments.Process_Environment;
         procedure Copy (Name, Value : String) is
         begin
            Env.Insert (Name, Value);
         end Copy;
      begin
         Ada.Environment_Variables.Iterate (Copy'Access);
         The_Process.Set_Environment (Env);
      end;
      The_Process.Set_Listener (The_Listener'Unchecked_Access);

      begin
         The_Process.Start;
         Running := True;
      exception
         when E : others =>
            Running := False;
            Spawn_OK := False;
            Put_Debug_Line ("  cannot start the command: " &
                              Ada.Exceptions.Exception_Information (E));
            Put_Step_Result
              (Step      => Step,
               Success   => False,
               Fail_Msg  => "Couldn't run " & Cmd,
               Loc       => Step.Location,
               Verbosity => Verbosity);
            return;
      end;

      --  Unless the command is to be fed interactively, the step waits
      --  for its termination, as a blocking Spawn would.
      if Interactive_Input_Expected then
         Wait_Quiet;
      else
         Pump (0.0);
      end if;

      Spawn_OK := Process_Error = 0;
      Return_Code := Last_Return_Code;

      Put_Debug_Line ("Spawn returns : Success = " & Spawn_OK'Image &
                        ", Return_Code = " & Return_Code'Image);

      --  Note: when interactive input is expected, the command may still
      --  be running here, so its return code is not checked at this step:
      --  it will be available to the following exit code checks steps,
      --  once the command has terminated.
      --  Fixme: thus, "successfully run" is not checked for interactive
      --  commands.
      if Spawn_OK and then not Interactive_Input_Expected
        and then Expected_Result = Success
      then
         Put_Step_Result (Step       => Step,
                           Success   => Is_Success (Return_Code),
                           Fail_Msg  => "Unsuccessfully run " &
                              Step.Data.Object_String'Image,
                           Loc       => Step.Location,
                           Verbosity => Verbosity);

      elsif Spawn_OK and then not Interactive_Input_Expected
        and then Expected_Result = Failure
      then
         Put_Step_Result (Step      => Step,
                          Success   => not Is_Success (Return_Code),
                          Fail_Msg  => "Successfully run " &
                            Step.Data.Object_String'Image &
                            " but expected to fail",
                          Loc       => Step.Location,
                          Verbosity => Verbosity);

      else
         -- If Expected_Result = Don't_Care, Success is only
         -- determined by Spawn_OK, not by the return code.
         Put_Step_Result (Step      => Step,
                          Success   => Spawn_OK,
                          Fail_Msg  => "Couldn't run " & Cmd,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      end if;

   end Run_Cmd;

   -- --------------------------------------------------------------------------
   function Interactive_Command_Running return Boolean is (Running);

   -- --------------------------------------------------------------------------
   procedure Wait_Command_End (Step      :     Step_Type'Class;
                               Verbosity :     Verbosity_Levels;
                               OK        : out Boolean) is
   begin
      if Running then
         Pump (End_Timeout);
      end if;
      OK := not Running;
      if not OK then
         Put_Step_Result (Step      => Step,
                          Success   => False,
                          Fail_Msg  => "the command is still running",
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      end if;
   end Wait_Command_End;

   -- --------------------------------------------------------------------------
   procedure Send_Input (Step           :     Step_Type'Class;
                         With_Newline   :     Boolean;
                         Verbosity      :     Verbosity_Levels;
                         OK             : out Boolean)
   is
      use Ada.Streams;
      Input : constant String :=
                To_String (Step.Data.Object_String)
                & (if With_Newline
                   then [1 => Ada.Characters.Latin_1.LF]
                   else "");
      Data      : Stream_Element_Array (1 .. Input'Length);
      Last      : Stream_Element_Offset;
      Write_OK  : Boolean := True;
   begin
      Put_Debug_Line ("Send_Input" & Input'Image);
      if not Running then
         Put_Step_Result (Step      => Step,
                          Success   => False,
                          Fail_Msg  => "no command is running when " &
                            "reaching this step",
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
         OK := False;
         return;
      end if;

      --  The command prompt should be complete before the input is sent,
      --  and the output baseline is set just before the input: the output
      --  checks following this step apply to the consequence of the input.
      Wait_Quiet;
      Output_Line_Offset := Output_Lines;
      Err_Line_Offset    := Err_Lines;
      Input_Bytes        := Output_Bytes + Err_Bytes;

      for J in Input'Range loop
         Data (Stream_Element_Offset (J)) := Character'Pos (Input (J));
      end loop;
      The_Process.Write_Standard_Input (Data, Last, Write_OK);
      OK := Write_OK and then Last = Data'Last;
      if not OK then
         Put_Step_Result (Step      => Step,
                          Success   => False,
                          Fail_Msg  => "cannot send input to the command",
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      end if;
   end Send_Input;

   -- --------------------------------------------------------------------------
   function Output_Since_Input (Output : Text) return Text is
      Result : Text := Empty_Text;
      Skip   : Natural := Output_Line_Offset;
   begin
      for Line of Output loop
         if Skip > 0 then
            Skip := @ - 1;
         else
            Result.Append (Line);
         end if;
      end loop;
      return Result;
   end Output_Since_Input;

   -- --------------------------------------------------------------------------
   function Stderr_Since_Input (Stderr : Text) return Text is
      Result : Text := Empty_Text;
      Skip   : Natural := Err_Line_Offset;
   begin
      for Line of Stderr loop
         if Skip > 0 then
            Skip := @ - 1;
         else
            Result.Append (Line);
         end if;
      end loop;
      return Result;
   end Stderr_Since_Input;

   -- --------------------------------------------------------------------------
   procedure Reset_Interactive_State is
   begin
      if Running then
         Pump (1.0);
         if Running then
            Put_Debug_Line ("  killing a command still running at the " &
                              "end of the scenario");
            The_Process.Kill_Process;
            Running := False;
         end if;
      end if;
      Output_Line_Offset := 0;
      Err_Line_Offset    := 0;
      Dispose_Process;
   end Reset_Interactive_State;

   -- --------------------------------------------------------------------------
   procedure Erase_And_Create (Step         : Step_Type'Class;
                               Verbosity    :     Verbosity_Levels) is
      File_Name  : constant String := To_String (Step.Data.Subject_String);
      Parent_Dir : constant String :=
                     Directories.Containing_Directory (File_Name);
   begin
      Put_Debug_Line ("Create_New " & File_Name);
      Created_File_List.Add (File_Name);
      -- Should be deleted at the end, even if pre-existing.

      case Step.Data.File_Type is
         when Ordinary_File =>
            if Exists (File_Name) then
               Put_Debug_Line (Item => "Deleting existing " & File_Name);
               Delete_File (File_Name);
            elsif not Exists (Parent_Dir) then
               Put_Debug_Line (Item => "Creating missing dir " & Parent_Dir);
               Directories.Create_Path (Parent_Dir);
               -- Create all missing intermediate directories
            end if;
            Create_File (File_Name    => File_Name,
                         With_Content => Get_Expected (Step),
                         Executable   => Step.Data.Executable_File);
            Put_Step_Result (Step      => Step,
                             Success   => File_Exists (File_Name),
                             Fail_Msg  => "File " & File_Name'Image &
                               " creation failed",
                             Loc       => Step.Location,
                             Verbosity => Verbosity);
         when Directory =>
            if not Exists (File_Name) then
               Directories.Create_Path (File_Name);
            end if;
            Put_Step_Result (Step      => Step,
                             Success   => Dir_Exists (File_Name),
                             Fail_Msg  => "Couldn't create directory " &
                               File_Name'Image,
                             Loc       => Step.Location,
                             Verbosity => Verbosity);
         when others =>
            -- don't mess around with special files!
            null;

      end case;
   end Erase_And_Create;

   -- --------------------------------------------------------------------------
   procedure Create_If_None (Step      : Step_Type'Class;
                             Verbosity : Verbosity_Levels) is
      File_Name : constant String := To_String (Step.Data.Subject_String);
   begin
      Put_Debug_Line ("Create_New " & File_Name);
      case Step.Data.File_Type is
         when Ordinary_File =>
            if not Exists (File_Name) then
               Created_File_List.Add (File_Name);
               -- should be deleted at the end only if created here
               Create_File (File_Name    => File_Name,
                            With_Content => Get_Expected (Step),
                            Executable   => Step.Data.Executable_File);

            end if;
            Put_Step_Result (Step     => Step,
                             Success  => File_Exists (File_Name),
                             Fail_Msg => "File " & File_Name'Image &
                               " creation failed",
                             Loc       => Step.Location,
                             Verbosity => Verbosity);
         when Directory =>
            if not Exists (File_Name) then
               Created_File_List.Add (File_Name);
               -- should be deleted at the end only if created here
               Directories.Create_Path (File_Name);
            end if;
            Put_Step_Result (Step     => Step,
                             Success  => Dir_Exists (File_Name),
                             Fail_Msg => "Couldn't create directory " &
                               File_Name'Image,
                             Loc       => Step.Location,
                             Verbosity => Verbosity);
         when others =>
            -- don't mess around with special files!
            null;
      end case;
   end Create_If_None;

   -- --------------------------------------------------------------------------
   procedure Return_Error (Last_Returned_Code : Integer;
                           Step               : Step_Type'Class;
                           Verbosity          : Verbosity_Levels) is
   begin
      Put_Debug_Line ("Return_Error " & Last_Returned_Code'Image);
      Put_Step_Result (Step     => Step,
                       Success  => not Is_Success (Last_Returned_Code),
                       Fail_Msg => "Expected error code, got no error",
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Return_Error;

   -- --------------------------------------------------------------------------
   procedure Return_No_Error (Last_Returned_Code : Integer;
                              Step               : Step_Type'Class;
                              Verbosity          : Verbosity_Levels) is
   begin
      Put_Debug_Line ("Return_No_Error " & Last_Returned_Code'Image);
      Put_Step_Result (Step     => Step,
                       Success  => Is_Success (Last_Returned_Code),
                       Fail_Msg => "No error expected, but got one (" &
                         Last_Returned_Code'Image & ")",
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Return_No_Error;

   -- --------------------------------------------------------------------------
   procedure Check_File_Existence (File_Name : String;
                                   Step      : Step_Type'Class;
                                   Verbosity : Verbosity_Levels) is
   begin
      Put_Debug_Line ("Check_File_Existence " & File_Name);
      if Entry_Exists (File_Name) then
         Put_Step_Result (Step     => Step,
                          Success  => Kind (File_Name) = Ordinary_File,
                          Fail_Msg => File_Name'Image &
                            " exists but its a dir and not a file as expected",
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         Put_Step_Result (Step     => Step,
                          Success  => False,
                          Fail_Msg => "Expected file " &
                            File_Name'Image & " doesn't exists",
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      end if;
   end Check_File_Existence;

   -- --------------------------------------------------------------------------
   procedure Check_Dir_Existence (Dir_Name : String;
                                  Step      : Step_Type'Class;
                                  Verbosity : Verbosity_Levels) is
   begin
      Put_Debug_Line ("Check_Dir_Existence " & Dir_Name);
      if Entry_Exists (Dir_Name) then
         Put_Step_Result (Step     => Step,
                          Success  => Kind (Dir_Name) = Directory,
                          Fail_Msg => "File " & Dir_Name'Image &
                            " exists but isn't a dir as expected",
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         Put_Step_Result (Step     => Step,
                          Success  => False,
                          Fail_Msg => "Expected dir " &
                            Dir_Name'Image & " doesn't exists in Exec_Dir "
                          & Settings.Exec_Dir,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      end if;
   end Check_Dir_Existence;

   -- --------------------------------------------------------------------------
   procedure Check_No_File (File_Name : String;
                            Step      : Step_Type'Class;
                            Verbosity : Verbosity_Levels) is
   begin
      Put_Debug_Line ("Check_No_File " & File_Name);
      Put_Step_Result (Step     => Step,
                       Success  => not File_Exists (File_Name),
                       Fail_Msg => "file " &
                         File_Name'Image & " shouldn't exists",
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Check_No_File;

   -- --------------------------------------------------------------------------
   procedure Check_No_Dir (Dir_Name : String;
                           Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels) is
   begin
      Put_Debug_Line ("Check_No_Dir " & Dir_Name);
      Put_Step_Result (Step     => Step,
                       Success  => not Dir_Exists (Dir_Name),
                       Fail_Msg => "dir " &
                         Dir_Name'Image & " shouldn't exists",
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Check_No_Dir;

   -- --------------------------------------------------------------------------
   procedure Check_No_Output (Output : Text;
                              Step      : Step_Type'Class;
                              Verbosity : Verbosity_Levels) is
      use Texts;
   begin
      Put_Step_Result (Step     => Step,
                       Success  => Output = Empty_Text,
                       Fail_Msg => String'("output not null : " & Output'Image),
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Check_No_Output;

   -- --------------------------------------------------------------------------
   procedure Setup_No_File (Step      : Step_Type'Class;
                            Verbosity : Verbosity_Levels) is
      File_Name : constant String :=
                    +Step.Data.Subject_String & (+Step.Data.Object_File_Name);
   begin
      Put_Debug_Line ("Setup_No_File " & File_Name);
      Delete_File (File_Name);
      Put_Step_Result (Step     => Step,
                       Success  => not File_Exists (File_Name),
                       Fail_Msg => "file " & File_Name'Image & " not deleted",
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Setup_No_File;

   -- --------------------------------------------------------------------------
   procedure Setup_No_Dir (Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels) is
      Dir_Name : constant String :=
                   +Step.Data.Subject_String & (+Step.Data.Object_File_Name);
   begin
      Put_Debug_Line ("Setup_No_Dir " & Dir_Name);
      Delete_Tree (Dir_Name);
      Put_Step_Result (Step     => Step,
                       Success  => not Dir_Exists (Dir_Name),
                       Fail_Msg => "dir " & Dir_Name'Image & " not deleted",
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Setup_No_Dir;

   -- --------------------------------------------------------------------------
   procedure Output_Is (Output    : Text;
                        Step      : Step_Type'Class;
                        Verbosity : Verbosity_Levels) is
      use Texts;
      T2 : constant Text := Get_Expected (Step);
   begin
      -- Put_Debug_Line ("++++++++++ Output_Is Step = " & Step'Image);
      -- Put_Debug_Line ("++++++++++ dir = " & Settings.Launch_Directory);
      Put_Debug_Line ("++++++++++ Output = ");
      -- Put_Text (Item => Output);
      Put_Debug_Line ("++++++++++ T2 = ");
      -- Put_Text (Item => T2);
      declare
         Success : constant Boolean :=
           Is_Equal (Output, T2,
                     Case_Insensitive   => Settings.Ignore_Casing,
                     Ignore_Blanks      => Settings.Ignore_Whitespaces,
                     Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                     Sort_Texts         => Step.Data.Ignore_Order);
         -- First line of Msg is the error message,
         -- then comes the side-by-side comparison of expected and actual.
         -- It is computed only on error.
         Msg : Text := (if Success then Empty_Text
                        else Side_By_Side (T2, Output,
                                           Case_Insensitive   => Settings.Ignore_Casing,
                                           Ignore_Whitespaces => Settings.Ignore_Whitespaces));
      begin
         if not Success then
            Msg.Prepend ("Output not equal to expected:");
         end if;
         Put_Step_Result (Step     => Step,
                          Success  => Success,
                          Fail_Msg => Msg,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      end;
   end Output_Is;

   -- --------------------------------------------------------------------------
   procedure Output_Contains (Output    : Text;
                              Step      : Step_Type'Class;
                              Verbosity : Verbosity_Levels) is
      T2  : constant Text := Get_Expected (Step);
   begin
      Put_Debug_Line ("++++++++++ Output_Contains Step = " & Step'Image);
      Put_Debug_Line ("++++++++++ dir = " & Settings.Launch_Directory);
      Put_Step_Result (Step     => Step,
                       Success  => Contains
                         (Output, T2,
                          Case_Insensitive   => Settings.Ignore_Casing,
                          Ignore_Whitespaces => Settings.Ignore_Whitespaces,
                          Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                          Sort_Texts         => Step.Data.Ignore_Order),
                       Fail_Msg => "Output:  " & Code_Fenced_Image (Output) &
                         "does not contain expected:  " &
                         Code_Fenced_Image (T2),
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Output_Contains;

   -- --------------------------------------------------------------------------
   procedure Output_Does_Not_Contain (Output : Text;
                                      Step      : Step_Type'Class;
                                      Verbosity : Verbosity_Levels) is
      T2  : constant Text := Get_Expected (Step);
   begin
      Put_Debug_Line ("Output_Does_Not_Contain ");
      declare
         Success : constant Boolean :=
           not Contains (Output, T2,
                         Case_Insensitive   => Settings.Ignore_Casing,
                         Ignore_Whitespaces => Settings.Ignore_Whitespaces,
                         Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                         Sort_Texts         => Step.Data.Ignore_Order);
         -- First line of Msg gives the position of the intruder,
         -- then come the intruder lines. It is computed only on error.
         Msg : Text := Empty_Text;
      begin
         if not Success then
            declare
               I : constant Line_Index'Base :=
                 Index_Of (Output, T2,
                           Case_Insensitive   => Settings.Ignore_Casing,
                           Ignore_Whitespaces => Settings.Ignore_Whitespaces);
               -- Last line of the intruder, limited by the end of Output
               Last : constant Line_Index :=
                 Line_Index'Min (I + Line_Index (T2.Length) - 1,
                                 Output.Last_Index);
            begin
               Msg.Append (String'("Output contains unexpected at line" &
                                   I'Image & ":"));
               for J in I .. Last loop
                  Msg.Append (Output (J));
               end loop;
            end;
         end if;
         Put_Step_Result (Step     => Step,
                          Success  => Success,
                          Fail_Msg => Msg,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      end;
   end Output_Does_Not_Contain;

   -- --------------------------------------------------------------------------
   procedure Output_Matches (Output : Text;
                             Step      : Step_Type'Class;
                             Verbosity : Verbosity_Levels) is
      Regexp : constant String := Get_Expected (Step) (1);
   begin
      Put_Debug_Line ("Output_Matches ");
      Put_Step_Result (Step     => Step,
                       Success  => Text_Utilities.Matches (Output, Regexp),
                       Fail_Msg => "Output:  " & Code_Fenced_Image (Output) &
                         "does not match expected:  " & Regexp,
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Output_Matches;

   -- --------------------------------------------------------------------------
   procedure Output_Does_Not_Match (Output : Text;
                                    Step      : Step_Type'Class;
                                    Verbosity : Verbosity_Levels) is
      Regexp : constant String := Get_Expected (Step) (1);
   begin
      Put_Debug_Line ("Output_Does_Not_Match ");
      Put_Step_Result (Step     => Step,
                       Success  => not Text_Utilities.Matches (Output, Regexp),
                       Fail_Msg => "Output:  " & Code_Fenced_Image (Output) &
                         "match unexpected:  " & Regexp,
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Output_Does_Not_Match;

   -- --------------------------------------------------------------------------
   procedure File_Matches (Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels) is
      Regexp    : constant String := Get_Expected (Step) (1);
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
   begin
      Put_Debug_Line ("File_Matches ");
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         Put_Step_Result (Step     => Step,
                          Success  => Text_Utilities.Matches (T1, Regexp),
                          Fail_Msg => "File:  " & File_Name &
                            " does not match expected:  " & Regexp,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end File_Matches;

   -- --------------------------------------------------------------------------
   procedure File_Does_Not_Match (Step      : Step_Type'Class;
                                  Verbosity : Verbosity_Levels) is
      Regexp    : constant String := Get_Expected (Step) (1);
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
   begin
      Put_Debug_Line ("File_Does_Not_Match ");
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         Put_Step_Result (Step     => Step,
                          Success  => not Text_Utilities.Matches (T1, Regexp),
                          Fail_Msg => "File:  " & File_Name &
                            " should not match:  " & Regexp,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end File_Does_Not_Match;

   -- --------------------------------------------------------------------------
   procedure Files_Is (Step      : Step_Type'Class;
                       Verbosity : Verbosity_Levels) is
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
      T2        : constant Text   := Get_Expected (Step);
   begin
      Put_Debug_Line ("Files_Is " & File_Name &
                        " T1 = " & T1'Image &
                        " T2 = " & T2'Image);
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         declare
            Success : constant Boolean :=
              Is_Equal (T1, T2,
                        Case_Insensitive   => Settings.Ignore_Casing,
                        Ignore_Blanks      => Settings.Ignore_Whitespaces,
                        Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                        Sort_Texts         => Step.Data.Ignore_Order);
            -- First line of Msg is the error message,
            -- then comes the side-by-side comparison of expected and actual.
            -- It is computed only on error.
            Msg : Text := (if Success then Empty_Text
                           else Side_By_Side (T2, T1,
                                              Case_Insensitive   => Settings.Ignore_Casing,
                                              Ignore_Whitespaces => Settings.Ignore_Whitespaces));
         begin
            if not Success then
               Msg.Prepend (File_Name & " not equal to expected:");
            end if;
            Put_Step_Result (Step     => Step,
                             Success  => Success,
                             Fail_Msg => Msg,
                             Loc       => Step.Location,
                             Verbosity => Verbosity);
         end;
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end Files_Is;

   -- --------------------------------------------------------------------------
   procedure Files_Is_Not (Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels) is
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
      T2        : constant Text   := Get_Expected (Step);
   begin
      Put_Debug_Line ("Files_Is_Not " & File_Name);
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         Put_Step_Result (Step     => Step,
                          Success  => not Is_Equal
                            (T1, T2,
                             Case_Insensitive   => Settings.Ignore_Casing,
                             Ignore_Blanks      => Settings.Ignore_Whitespaces,
                             Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                             Sort_Texts         => Step.Data.Ignore_Order),
                          Fail_Msg => File_Name & " expected to be different from " &
                            Code_Fenced_Image (T2),
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end Files_Is_Not;

   -- --------------------------------------------------------------------------
   procedure File_Contains (Step      : Step_Type'Class;
                            Verbosity : Verbosity_Levels) is
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
      T2        : constant Text   := Get_Expected (Step);
   begin
      Put_Debug_Line ("File_Contains " & File_Name &
                        " T1 = " & T1'Image &
                        " T2 = " & T2'Image);
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         Put_Step_Result (Step     => Step,
                          Success  => Contains
                            (T1, T2,
                             Case_Insensitive   => Settings.Ignore_Casing,
                             Ignore_Whitespaces => Settings.Ignore_Whitespaces,
                             Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                             Sort_Texts         => Step.Data.Ignore_Order),
                          Fail_Msg => File_Name &
                            " does not contain expected:  " &
                            Code_Fenced_Image (T2),
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end File_Contains;

   -- --------------------------------------------------------------------------
   procedure File_Does_Not_Contain (Step      : Step_Type'Class;
                                    Verbosity : Verbosity_Levels) is
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
      T2        : constant Text   := Get_Expected (Step);
   begin
      Put_Debug_Line ("File_Does_Not_Contain " & File_Name);
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         declare
            Success : constant Boolean :=
              not Contains (T1, T2,
                            Case_Insensitive   => Settings.Ignore_Casing,
                            Ignore_Whitespaces => Settings.Ignore_Whitespaces,
                            Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                            Sort_Texts         => Step.Data.Ignore_Order);
            -- First line of Msg gives the position of the intruder,
            -- then come the intruder lines. It is computed only on error.
            Msg : Text := Empty_Text;
         begin
            if not Success then
               declare
                  I : constant Line_Index'Base :=
                    Index_Of (T1, T2,
                              Case_Insensitive   => Settings.Ignore_Casing,
                              Ignore_Whitespaces => Settings.Ignore_Whitespaces);
                  -- Last line of the intruder, limited by the end of the file
                  Last : constant Line_Index :=
                    Line_Index'Min (I + Line_Index (T2.Length) - 1,
                                    T1.Last_Index);
               begin
                  Msg.Append (String'(File_Name &
                                      " contains unexpected at line" &
                                      I'Image & ":"));
                  for J in I .. Last loop
                     Msg.Append (T1 (J));
                  end loop;
               end;
            end if;
            Put_Step_Result (Step     => Step,
                             Success  => Success,
                             Fail_Msg => Msg,
                             Loc       => Step.Location,
                             Verbosity => Verbosity);
         end;
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end File_Does_Not_Contain;

   -- --------------------------------------------------------------------------
   procedure Exit_Code_Is (Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels) is
      Expected : constant Integer := Integer'Value (+Step.Data.Object_String);
      -- Checked in Validate_Step_State
   begin
      Put_Debug_Line ("Exit_Code_Is " & Expected'Image & ", last ="
                      & Last_Return_Code'Image);
      Put_Step_Result (Step      => Step,
                       Success   => Last_Return_Code = Expected,
                       Fail_Msg  => "Expected exit code" & Expected'Image
                                    & ", got" & Last_Return_Code'Image,
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Exit_Code_Is;

   -- --------------------------------------------------------------------------
   -- Environment variables set or unset by the steps of the current
   -- scenario, with the value (or the absence) they had before, so that
   -- Restore_Environment can give them back.
   type Saved_Variable (Name_Length, Value_Length : Natural) is record
      Name    : String (1 .. Name_Length);
      Present : Boolean;
      Value   : String (1 .. Value_Length);
   end record;

   package Saved_Variable_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Saved_Variable);
   Saved_Variables : Saved_Variable_Lists.Vector;

   procedure Remember (Name : String) is
   begin
      for V of Saved_Variables loop
         if V.Name = Name then
            return; -- keep the value from before the first change
         end if;
      end loop;
      if Environment_Variables.Exists (Name) then
         declare
            Old : constant String := Environment_Variables.Value (Name);
         begin
            Saved_Variables.Append
              (Saved_Variable'(Name_Length  => Name'Length,
                Value_Length => Old'Length,
                Name         => Name,
                Present      => True,
                Value        => Old));
         end;
      else
         Saved_Variables.Append
           (Saved_Variable'(Name_Length  => Name'Length,
             Value_Length => 0,
             Name         => Name,
             Present      => False,
             Value        => ""));
      end if;
   end Remember;

   -- --------------------------------------------------------------------------
   procedure Set_Env_Var (Step      : Step_Type'Class;
                          Verbosity : Verbosity_Levels) is
      Name  : constant String := +Step.Data.Subject_String;
      Value : constant String := +Step.Data.Object_String;
      OK    : Boolean := True;
   begin
      Put_Debug_Line ("Set_Env_Var " & Name & " = " & Value);
      Remember (Name);
      begin
         Environment_Variables.Set (Name, Value);
      exception
         when Constraint_Error | Program_Error =>
            -- Illegal name (empty or with '=') or rejected by the OS
            OK := False;
      end;
      Put_Step_Result (Step      => Step,
                       Success   => OK,
                       Fail_Msg  => "Unable to set environment variable "
                                    & Name'Image,
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Set_Env_Var;

   -- --------------------------------------------------------------------------
   procedure Unset_Env_Var (Step      : Step_Type'Class;
                            Verbosity : Verbosity_Levels) is
      Name : constant String := +Step.Data.Subject_String;
      OK   : Boolean := True;
   begin
      Put_Debug_Line ("Unset_Env_Var " & Name);
      Remember (Name);
      begin
         Environment_Variables.Clear (Name);
      exception
         when Constraint_Error | Program_Error =>
            OK := False;
      end;
      Put_Step_Result (Step      => Step,
                       Success   => OK,
                       Fail_Msg  => "Unable to unset environment variable "
                                    & Name'Image,
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Unset_Env_Var;

   -- --------------------------------------------------------------------------
   procedure Restore_Environment is
   begin
      for V of Saved_Variables loop
         if V.Present then
            Environment_Variables.Set (V.Name, V.Value);
         else
            Environment_Variables.Clear (V.Name);
         end if;
      end loop;
      Saved_Variables.Clear;
   end Restore_Environment;

end BBT.Tests.Actions;
