-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Settings;
with BBT.Writers;                       use BBT.Writers;
with BBT.Tests.Actions.File_Operations; use BBT.Tests.Actions.File_Operations;

with Ada.Calendar;
with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Spawn.Environments,
     Spawn.Processes,
     Spawn.Process_Listeners,
     Spawn.Processes.Monitor_Loop,
     Spawn.String_Vectors;

with GNAT.OS_Lib;

use Ada, BBT;

package body BBT.Tests.Actions.Commands is

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
   Spawn_Error : Integer := 0;
   --  set by the listener if the command could not be run

   Scenario_Deadline : Ada.Calendar.Time := Ada.Calendar.Clock;
   --  expiry time of the current scenario when the scenario timeout is
   --  armed; the bound applies only when Settings.Scenario_Timeout > 0
   --  (cf. the design discussion D6)

   use type Ada.Calendar.Time;

   function Timeout_Expired return Boolean is
     (Settings.Scenario_Timeout > 0.0
      and then Ada.Calendar.Clock >= Scenario_Deadline);

   procedure Set_Scenario_Deadline is
   begin
      if Settings.Scenario_Timeout > 0.0 then
         Scenario_Deadline := Ada.Calendar.Clock + Settings.Scenario_Timeout;
      end if;
   end Set_Scenario_Deadline;

   function Timeout_Image return String is
     (Ada.Strings.Fixed.Trim
        (Integer (Settings.Scenario_Timeout)'Image, Ada.Strings.Left));
   --  the timeout is always a whole number of seconds: the duration
   --  parser only sums integer amounts of seconds, minutes or hours

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

   --  A successfully run command that is still running at its step has
   --  its exit status check deferred to the next synchronization
   --  point, or to the end of the scenario (cf. the design
   --  discussion D2): the step result is emitted when the check is
   --  resolved, on the successfully run step line.
   type Step_Access is access all Step_Type'Class;
   Deferred_Step     : Step_Access;
   Deferred_Expected : Run_Result;
   Deferred_Pending  : Boolean := False;

   function Deferred_Exit_Check_Pending return Boolean is (Deferred_Pending);

   -- --------------------------------------------------------------------------
   procedure Defer_Exit_Check (Step     : Step_Type'Class;
                               Expected : Run_Result) is
   begin
      --  The step object is in the scenario step list, that lives for
      --  the whole run: keeping an access is safe, and cheaper than
      --  copying the step. The state is cleared at each scenario end.
      --  Unrestricted_Access is required, as the Step parameter is a
      --  constant view here; the step is never modified through it.
      Deferred_Step     := Step'Unrestricted_Access;
      Deferred_Expected := Expected;
      Deferred_Pending  := True;
      Put_Debug_Line ("  exit status check deferred, command still running");
   end Defer_Exit_Check;

   -- --------------------------------------------------------------------------
   procedure Check_Exit_Status (Step      : Step_Type'Class;
                                Expected  : Run_Result;
                                Verbosity : Verbosity_Levels) is
   --  Reports on the Step line the exit status check of the last
   --  terminated command: OK when its exit status matches Expected,
   --  a failure otherwise.
   begin
      if Expected = Success then
         Put_Step_Result (Step      => Step,
                          Success   => Is_Success (Last_Return_Code),
                          Fail_Msg  => "Unsuccessfully run " &
                            Step.Data.Object_String'Image,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         Put_Step_Result (Step      => Step,
                          Success   => not Is_Success (Last_Return_Code),
                          Fail_Msg  => "Successfully run " &
                            Step.Data.Object_String'Image &
                            " but expected to fail",
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      end if;
   end Check_Exit_Status;

   -- --------------------------------------------------------------------------
   procedure Resolve_Deferred_Exit_Check (Verbosity :     Verbosity_Levels;
                                           OK        : out Boolean) is
   begin
      OK := True;
      if not Deferred_Pending then
         return;
      end if;
      Deferred_Pending := False;
      OK := (if Deferred_Expected = Success
             then Is_Success (Last_Return_Code)
             else not Is_Success (Last_Return_Code));
      Check_Exit_Status (Step      => Deferred_Step.all,
                         Expected  => Deferred_Expected,
                         Verbosity => Verbosity);
      Deferred_Step := null;
   end Resolve_Deferred_Exit_Check;

   Output_Stream : Ada.Streams.Stream_IO.File_Type;
   Err_Stream    : Ada.Streams.Stream_IO.File_Type;
   --  Open for the whole command duration, and flushed at each chunk,
   --  so that output checks can read the files between two callbacks,
   --  without paying the file open cost at each chunk.

   -- ------------------------------------------------------------------------
   procedure Append_Data (File_Name : String;
                          Data      : Ada.Streams.Stream_Element_Array)
   is
      F : Ada.Streams.Stream_IO.File_Type;
   begin
      --  Fallback used when no stream is open: the file is opened and
      --  closed at each call, so that output checks can read it between
      --  two callbacks.
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
   procedure Write_Chunk (Stream    : Ada.Streams.Stream_IO.File_Type;
                          File_Name : String;
                          Data      : Ada.Streams.Stream_Element_Array) is
   begin
      --  When a stream is open, the chunk is written through it, and
      --  flushed, so that output checks can read the file between two
      --  callbacks without paying the file open cost at each chunk;
      --  otherwise the fallback opens and closes the file at each call.
      if Ada.Streams.Stream_IO.Is_Open (Stream) then
         Ada.Streams.Stream_IO.Write (Stream, Data);
         Ada.Streams.Stream_IO.Flush (Stream);
      else
         Append_Data (File_Name, Data);
      end if;
   end Write_Chunk;

   -- ------------------------------------------------------------------------
   procedure Close_Stream (Stream : in out Ada.Streams.Stream_IO.File_Type) is
   begin
      --  The streams are flushed at each chunk: this final flush is a
      --  safety net, e.g. for a command that could not start.
      if Ada.Streams.Stream_IO.Is_Open (Stream) then
         Ada.Streams.Stream_IO.Flush (Stream);
         Ada.Streams.Stream_IO.Close (Stream);
      end if;
   end Close_Stream;

   -- ------------------------------------------------------------------------
   procedure Write_Output (Data : Ada.Streams.Stream_Element_Array) is
      use Ada.Streams;
   begin
      Write_Chunk (Output_Stream, To_String (Cmd_Output_Name), Data);
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
         Write_Chunk (Output_Stream, To_String (Cmd_Output_Name), Data);
      else
         Write_Chunk (Err_Stream, To_String (Cmd_Err_Name), Data);
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
   --  library cleans its map
   --  (cf. https://github.com/AdaCore/spawn/issues/36).

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
         Spawn_Error := Process_Error;
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
         Spawn_Error := 1;
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
      Deadline : constant Ada.Calendar.Time :=
                   Ada.Calendar.Clock + Timeout;
   begin
      while Running loop
         if Timeout > 0.0 and then Ada.Calendar.Clock >= Deadline then
            return;
         end if;
         exit when Timeout_Expired;
         Safe_Monitor_Loop (Quiet_Step);
      end loop;
   end Pump;

   -- ------------------------------------------------------------------------
   procedure Kill_Running_Command is
   begin
      if Running and then The_Process /= null then
         Put_Debug_Line ("  killing the command on scenario timeout");
         The_Process.Kill_Process;
         --  Pump a moment, so that the listener reaps the process and
         --  updates the state.
         Pump (0.5);
      end if;
   end Kill_Running_Command;

   -- ------------------------------------------------------------------------
   procedure Check_Deadline (Step      :     Step_Type'Class;
                             Verbosity :     Verbosity_Levels;
                             OK        : out Boolean) is
   begin
      OK := not Timeout_Expired;
      if not OK then
         --  The scenario timeout expired: kill the still running
         --  command, so that no process is left behind, and report
         --  the failure on the hanging step line (cf. the design
         --  discussion D6).
         Kill_Running_Command;
         Put_Step_Result (Step      => Step,
                          Success   => False,
                          Fail_Msg  => "the scenario timeout of "
                            & Timeout_Image & "s expired",
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      end if;
   end Check_Deadline;

   -- ------------------------------------------------------------------------
   procedure Wait_Deferred_Exit_Check (Verbosity : Verbosity_Levels) is
      OK : Boolean;
   begin
      if Deferred_Pending then
         if Running then
            Put_Debug_Line ("  waiting for the command end, to resolve " &
                              "the deferred exit status check");
            Pump (0.0);
         end if;
         if Timeout_Expired then
            Put_Step_Result (Step      => Deferred_Step.all,
                             Success   => False,
                             Fail_Msg  => "the scenario timeout of "
                               & Timeout_Image
                               & "s expired before the command termination",
                             Loc       => Deferred_Step.all.Location,
                             Verbosity => Verbosity);
            Kill_Running_Command;
            Deferred_Pending := False;
            Deferred_Step    := null;
         else
            Resolve_Deferred_Exit_Check (Verbosity, OK);
         end if;
      end if;
   end Wait_Deferred_Exit_Check;

   -- ------------------------------------------------------------------------
   procedure Wait_Quiet is
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
           or else Ada.Calendar.Clock >= Deadline
           or else Timeout_Expired;
      end loop;
   end Wait_Quiet;

   -- ------------------------------------------------------------------------
   procedure Wait_Response is
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
           or else Ada.Calendar.Clock >= Deadline
           or else Timeout_Expired;
      end loop;
   end Wait_Response;

   -- --------------------------------------------------------------------------
   procedure Dispose_Process is
      Deadline : constant Ada.Calendar.Time :=
                   Ada.Calendar.Clock + End_Timeout;
   begin
      --  Close the output streams in any case: they were flushed at
      --  each chunk, so the files are already complete and readable.
      Close_Stream (Output_Stream);
      Close_Stream (Err_Stream);

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

      --  Truncate the output files, and reset the state.
      --
      --  Unless the command is to be fed interactively, the streams stay
      --  open for the whole command, and are flushed at each chunk:
      --  reopening a file at each chunk costs around 0.7 s on some
      --  Windows configurations, which made commands producing their
      --  output in many chunks, such as a nested bbt help grammar, take
      --  minutes instead of milliseconds. The streams are closed as
      --  soon as the command is terminated, or at the next command, so
      --  that the following checks can read the files.
      --
      --  For an interactive command, the output is instead appended by
      --  open-and-close at each chunk: the checks read the files
      --  between two chunks while the command is still running, which
      --  requires the file to be closed between the chunks.
      if Interactive_Input_Expected then
         declare
            F : Ada.Streams.Stream_IO.File_Type;
         begin
            Ada.Streams.Stream_IO.Create
              (F, Ada.Streams.Stream_IO.Out_File, Output_Name);
            Ada.Streams.Stream_IO.Close (F);
            if Error_Output_Name /= "" then
               Ada.Streams.Stream_IO.Create
                 (F, Ada.Streams.Stream_IO.Out_File, Error_Output_Name);
               Ada.Streams.Stream_IO.Close (F);
            end if;
         end;
      else
         Ada.Streams.Stream_IO.Create
           (Output_Stream, Ada.Streams.Stream_IO.Out_File, Output_Name);
         if Error_Output_Name /= "" then
            Ada.Streams.Stream_IO.Create
              (Err_Stream, Ada.Streams.Stream_IO.Out_File, Error_Output_Name);
         end if;
      end if;
      Cmd_Output_Name := To_Unbounded_String (Output_Name);
      Merged          := Error_Output_Name = "";
      if not Merged then
         Cmd_Err_Name := To_Unbounded_String (Error_Output_Name);
      end if;
      Output_Bytes       := 0;
      Err_Bytes          := 0;
      Output_Lines       := 0;
      Err_Lines          := 0;
      Output_Line_Offset := 0;
      Err_Line_Offset    := 0;
      Input_Bytes        := 0;
      Spawn_Error         := 0;
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
            --  The command could not start: close the streams, so that
            --  the following checks can read the empty output files.
            Close_Stream (Output_Stream);
            Close_Stream (Err_Stream);
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
         --  The command is terminated: close the streams now, so that
         --  the following checks can read the files.
         Close_Stream (Output_Stream);
         Close_Stream (Err_Stream);
      end if;

      Check_Deadline (Step      => Step,
                      Verbosity => Verbosity,
                      OK        => Spawn_OK);
      if not Spawn_OK then
         --  The scenario timeout expired while running the command:
         --  the failure has already been reported on the step line.
         Return_Code := Last_Return_Code;
         return;
      end if;

      Spawn_OK := Spawn_Error = 0;
      Return_Code := Last_Return_Code;

      Put_Debug_Line ("Spawn returns : Success = " & Spawn_OK'Image &
                        ", Return_Code = " & Return_Code'Image);

      --  Note: when interactive input is expected, the command may still
      --  be running here, and its exit status check is deferred to the
      --  next synchronization point, or to the end of the scenario
      --  (cf. the design discussion D2): the step result is emitted
      --  when the check is resolved, on the successfully run step line.
      if not Spawn_OK then
         Put_Step_Result (Step      => Step,
                          Success   => False,
                          Fail_Msg  => "Couldn't run " & Cmd,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);

      elsif Running and then Expected_Result /= Not_Specified then
         Defer_Exit_Check (Step     => Step,
                           Expected => Expected_Result);

      elsif Expected_Result /= Not_Specified then
         --  The command has terminated: the check is immediate
         Check_Exit_Status (Step      => Step,
                            Expected  => Expected_Result,
                            Verbosity => Verbosity);

      else
         -- If Expected_Result = Not_Specified, Success is only
         -- determined by Spawn_OK, not by the return code.
         Put_Step_Result (Step      => Step,
                          Success   => True,
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
      Check_Deadline (Step      => Step,
                      Verbosity => Verbosity,
                      OK        => OK);
      if OK then
         OK := not Running;
         if not OK then
            Put_Step_Result (Step      => Step,
                             Success   => False,
                             Fail_Msg  => "the command is still running",
                             Loc       => Step.Location,
                             Verbosity => Verbosity);
         end if;
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

      --  A deferred exit status check is resolved first (cf. the
      --  design discussion D2): if the command exited with an error
      --  since the previous step, the failure is reported on the
      --  successfully run step, and this step is not executed,
      --  instead of a misleading "no command is running" message.
      if Deferred_Pending then
         if Running then
            Wait_Quiet;
         end if;
         if not Running then
            Resolve_Deferred_Exit_Check (Verbosity, OK);
            if not OK then
               return;
            end if;
         end if;
      end if;

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
      Check_Deadline (Step      => Step,
                      Verbosity => Verbosity,
                      OK        => OK);
      if not OK then
         --  The scenario timeout expired: the failure has already
         --  been reported on the step line.
         return;
      end if;
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
      Deferred_Pending := False;
      Deferred_Step    := null;
      Dispose_Process;
   end Reset_Interactive_State;

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
   procedure Exit_Code_Is (Last_Returned_Code : Integer;
                           Step                : Step_Type'Class;
                           Verbosity          : Verbosity_Levels) is
      Expected : constant Integer := Integer'Value (+Step.Data.Object_String);
      -- Checked in Validate_Step_State
   begin
      Put_Debug_Line ("Exit_Code_Is " & Expected'Image & ", last ="
                      & Last_Returned_Code'Image);
      Put_Step_Result (Step      => Step,
                       Success   => Last_Returned_Code = Expected,
                       Fail_Msg  => "Expected exit code" & Expected'Image
                                    & ", got" & Last_Returned_Code'Image,
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Exit_Code_Is;

end BBT.Tests.Actions.Commands;
