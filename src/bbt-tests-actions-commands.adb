-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Settings;
with BBT.Writers;                       use BBT.Writers;
with BBT.Tests.Actions.File_Operations; use BBT.Tests.Actions.File_Operations;
with BBT.Tests.Actions.Commands.Spawn;

with Ada.Calendar;
with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Util.Strings.Vectors;

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

   use type Ada.Calendar.Time;

   -- --------------------------------------------------------------------------
   function Is_Success (I : Integer) return Boolean is
     (I = Integer (Command_Line.Success));
   -- --------------------------------------------------------------------------
   procedure Set_Scenario_Deadline is
   begin
      Spawn.Set_Deadline;
   end Set_Scenario_Deadline;

   function Timeout_Image return String is
     (Ada.Strings.Fixed.Trim
        (Integer (Settings.Scenario_Timeout)'Image, Ada.Strings.Left));
   --  the timeout is always a whole number of seconds: the duration
   --  parser only sums integer amounts of seconds, minutes or hours

   function Last_Exit_Code return Integer is (Spawn.Last_Exit_Code);

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
                          Success   => Is_Success (Spawn.Last_Exit_Code),
                          Fail_Msg  => "Unsuccessfully run " &
                            Step.Data.Object_String'Image,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         Put_Step_Result (Step      => Step,
                          Success   => not Is_Success (Spawn.Last_Exit_Code),
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
             then Is_Success (Spawn.Last_Exit_Code)
             else not Is_Success (Spawn.Last_Exit_Code));
      Check_Exit_Status (Step      => Deferred_Step.all,
                         Expected  => Deferred_Expected,
                         Verbosity => Verbosity);
      Deferred_Step := null;
   end Resolve_Deferred_Exit_Check;

   procedure Check_Deadline (Step      :     Step_Type'Class;
                             Verbosity :     Verbosity_Levels;
                             OK        : out Boolean) is
   begin
      OK := not Spawn.Timeout_Expired;
      if not OK then
         --  The scenario timeout expired: kill the still running
         --  command, so that no process is left behind, and report
         --  the failure on the hanging step line (cf. the design
         --  discussion D6).
         Spawn.Kill_And_Drain;
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
         if Spawn.Running then
            Put_Debug_Line ("  waiting for the command end, to resolve " &
                              "the deferred exit status check");
            Spawn.Pump (0.0);
         end if;
         if Spawn.Timeout_Expired then
            Put_Step_Result (Step      => Deferred_Step.all,
                             Success   => False,
                             Fail_Msg  => "the scenario timeout of "
                               & Timeout_Image
                               & "s expired before the command termination",
                             Loc       => Deferred_Step.all.Location,
                             Verbosity => Verbosity);
            Spawn.Kill_And_Drain;
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
                    Ada.Calendar.Clock + Spawn.Quiet_Timeout;
      Quiet     : Duration := 0.0;
      Last_Size : Natural;
   begin
      if not Spawn.Running then
         return;
      end if;
      --  The command output is considered complete when it stays quiet
      --  during a short window, or when it terminates. This is used before
      --  sending input to a command: whether its prompt is already there
      --  or it is still starting does not matter, as long as it is quiet.
      loop
         Last_Size := Spawn.Output_Bytes + Spawn.Err_Bytes;
         Spawn.Poll (Spawn.Quiet_Step);
         if Spawn.Output_Bytes + Spawn.Err_Bytes = Last_Size then
            Quiet := @ + Spawn.Quiet_Step;
         else
            Quiet := 0.0;
         end if;
         exit when Quiet >= Spawn.Quiet_Window or else not Spawn.Running
           or else Ada.Calendar.Clock >= Deadline
           or else Spawn.Timeout_Expired;
      end loop;
   end Wait_Quiet;

   -- ------------------------------------------------------------------------
   procedure Wait_Response is
      Deadline   : constant Ada.Calendar.Time :=
                     Ada.Calendar.Clock + Spawn.Quiet_Timeout;
      Quiet      : Duration := 0.0;
      Last_Size  : Natural;
   begin
      if not Spawn.Running then
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
         Last_Size := Spawn.Output_Bytes + Spawn.Err_Bytes;
         Spawn.Poll (Spawn.Quiet_Step);
         if Spawn.Output_Bytes + Spawn.Err_Bytes = Last_Size then
            if Spawn.Output_Bytes + Spawn.Err_Bytes > Spawn.Input_Bytes then
               Quiet := @ + Spawn.Quiet_Step;
            end if;
         else
            Quiet := 0.0;
         end if;
         exit when not Spawn.Running
           or else Quiet >= Spawn.Quiet_Window
           or else Ada.Calendar.Clock >= Deadline
           or else Spawn.Timeout_Expired;
      end loop;
   end Wait_Response;


   -- --------------------------------------------------------------------------
   function Quote_Windows_Arg (S : String) return String is
   --  Quote S for the Windows command line, per the Microsoft C runtime
   --  parsing rules: ada-util joins the process arguments with spaces
   --  without quoting them, so an argument containing a space must be
   --  quoted here, and its embedded double quotes escaped
      Result      : Unbounded_String := Null_Unbounded_String;
      Backslashes : Natural := 0;
   begin
      Append (Result, '"');
      for C of S loop
         if C = '\' then
            Backslashes := @ + 1;

         elsif C = '"' then
            --  A double quote is escaped, preceded by the doubling of
            --  the backslashes that precede it
            Append (Result, [1 .. 2 * Backslashes => '\']);
            Append (Result, "\""");
            Backslashes := 0;

         else
            Append (Result, [1 .. Backslashes => '\']);
            Backslashes := 0;
            Append (Result, C);
         end if;
      end loop;
      Append (Result, [1 .. Backslashes => '\']);
      Append (Result, '"');
      return To_String (Result);
   end Quote_Windows_Arg;

   -- ------------------------------------------------------------------------
   procedure Build_Spawn_Args (Cmd           :        String;
                               Through_Shell :        Boolean;
                               Step          :        Step_Type'Class;
                               Verbosity     :        Verbosity_Levels;
                               Args          : in out Util.Strings.Vectors.Vector;
                               OK            :    out Boolean)
   is
      use GNAT.OS_Lib;
      Spawn_Arg : Argument_List_Access := null;

   begin
      OK := False;

      if Through_Shell then
         --  The command string is passed as a single argument to the
         --  shell set in the settings: it is not split by bbt, and its
         --  pipes, redirections and command substitutions are
         --  interpreted by that shell (cf. docs/features/A310_Run_Shell.md)
         declare
            The_Shell : constant Settings.Shell_Kind := Settings.Shell;
            Full_Path : constant GNAT.OS_Lib.String_Access :=
              Locate_Exec_On_Path (Settings.Shell_Exe (The_Shell));
         begin
            if Full_Path = null then
               OK := False;
               Put_Step_Result
                 (Step      => Step,
                  Success   => OK,
                  Fail_Msg  => Settings.Shell_Exe (The_Shell) & " not found",
                  Loc       => Step.Location,
                  Verbosity => Verbosity);
               return;
            end if;
            Put_Debug_Line
              ("Shell " & Settings.Shell_Exe (The_Shell)
               & " = " & Full_Path.all);
            Args.Append (Full_Path.all);
            --  The invocation flag may be several arguments, e.g.
            --  -NoProfile -Command for the PowerShell shells: each is
            --  appended separately, as ada-util joins the arguments
            --  without quoting them
            declare
               Flag  : constant String := Settings.Shell_Flag (The_Shell);
               Start : Positive := Flag'First;
            begin
               for I in Flag'Range loop
                  if Flag (I) = ' ' then
                     Args.Append (Flag (Start .. I - 1));
                     Start := I + 1;
                  end if;
               end loop;
               Args.Append (Flag (Start .. Flag'Last));
            end;
            --  On Windows, the command string must be quoted: ada-util
            --  joins the arguments with spaces without quoting them,
            --  and the shell would otherwise receive the command
            --  truncated at its first space. On the other systems the
            --  arguments are passed as a real argv array, and the
            --  command string must be passed verbatim
            if Settings.On_Windows then
               Args.Append
                 (Quote_Windows_Arg
                    (Settings.Shell_Preamble (The_Shell) & Cmd));
            else
               Args.Append (Settings.Shell_Preamble (The_Shell) & Cmd);
            end if;
         end;

      else
         -- The first argument should be an executable (e.g. not a bash
         -- built-in)
         --
         -- If it is, replace it by the fully-qualified path name (spawn
         -- is implemented via execve, which on macOS doesn't
         -- understand PATH)
         Spawn_Arg := Argument_String_To_List (Cmd);
         Find_The_Executable_If_Any :
         declare
            Full_Path : GNAT.OS_Lib.String_Access;
         begin
            Full_Path := Locate_Exec_On_Path (Spawn_Arg.all (1).all);

            if Exists (Spawn_Arg.all (1).all) and then
              not Is_Executable_File (Spawn_Arg.all (1).all)
            then
               OK := False;
               Put_Step_Result
                 (Step      => Step,
                  Success   => OK,
                  Fail_Msg  => Spawn_Arg.all (1).all & " not executable",
                  Loc       => Step.Location,
                  Verbosity => Verbosity);
               return;

            elsif Full_Path = null then
               OK := False;
               Put_Step_Result
                 (Step      => Step,
                  Success   => OK,
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
         end loop;

         --  The first argument is the command, and the following ones
         --  are the already split and unquoted arguments: the process
         --  is created without shell
         Args.Append (Spawn_Arg.all (1).all);
         for I in 2 .. Spawn_Arg'Last loop
            Args.Append (Spawn_Arg.all (I).all);
         end loop;
      end if;

      OK := True;
   end Build_Spawn_Args;

   procedure Run_Cmd (Step                       :     Step_Type'Class;
                      Cmd                        :     String;
                      Output_Name                :     String;
                      Expected_Result            :     Run_Result;
                      Verbosity                  :     Verbosity_Levels;
                      Spawn_OK                   : out Boolean;
                      Return_Code                : out Integer;
                      Error_Output_Name          :     String := "";
                      Interactive_Input_Expected :     Boolean := False;
                      Through_Shell              :     Boolean := False)
   is
      Args : Util.Strings.Vectors.Vector;

   begin
      Put_Debug_Line ("Run_Cmd " & Cmd & " in " & Settings.Exec_Dir &
                        ", output file = " & Output_Name &
                        (if Interactive_Input_Expected
                         then ", interactive input expected"
                         else "") &
                        (if Through_Shell
                         then ", through " &
                           Settings.Shell_Name (Settings.Shell)
                         else ""));

      --  A previous command should have terminated before this one starts:
      --  all steps but the input ones wait for its end. If it is still
      --  running here, let's give it a chance, then kill it.
      if Spawn.Running then
         Spawn.Pump (Spawn.End_Timeout);
         if Spawn.Running then
            Put_Debug_Line ("  killing a still running command");
            Spawn.Kill;
         end if;
      end if;

      --  A new process object is used for each command.
      Spawn.Dispose;

      Build_Spawn_Args (Cmd           => Cmd,
                        Through_Shell => Through_Shell,
                        Step          => Step,
                        Verbosity     => Verbosity,
                        Args          => Args,
                        OK            => Spawn_OK);
      if not Spawn_OK then
         return;
      end if;

      Spawn.Start (Args        => Args,
                   Merged      => Error_Output_Name = "",
                   Interactive => Interactive_Input_Expected,
                   Out_Name    => Output_Name,
                   Err_Name    => Error_Output_Name,
                   OK          => Spawn_OK);
      if not Spawn_OK then
         --  The command could not start: the failure cause is debug
         --  logged by Spawn, the output files are closed so that the
         --  following checks can read the empty output files.
         Put_Step_Result
           (Step      => Step,
            Success   => False,
            Fail_Msg  => "Couldn't run " & Cmd,
            Loc       => Step.Location,
            Verbosity => Verbosity);
         return;
      end if;

      --  Unless the command is to be fed interactively, the step waits
      --  for its termination, as a blocking Spawn would.
      if Interactive_Input_Expected then
         Wait_Quiet;
      else
         Spawn.Pump (0.0);
         --  The command is terminated: close the streams now, so that
         --  the following checks can read the files.
         Spawn.Close_Output_Files;
      end if;

      Check_Deadline (Step      => Step,
                      Verbosity => Verbosity,
                      OK        => Spawn_OK);
      if not Spawn_OK then
         --  The scenario timeout expired while running the command:
         --  the failure has already been reported on the step line.
         Return_Code := Spawn.Last_Exit_Code;
         return;
      end if;

      Spawn_OK := True;
      Return_Code := Spawn.Last_Exit_Code;

      Put_Debug_Line ("Run_Cmd returns : Success = " & Spawn_OK'Image &
                        ", Return_Code = " & Return_Code'Image);

      --  Note: when interactive input is expected, the command may still
      --  be running here, and its exit status check is deferred to the
      --  next synchronization point, or to the end of the scenario
      --  (cf. the design discussion D2): the step result is emitted
      --  when the check is resolved, on the successfully run step line.
      --  Any run error has already been reported, and returned.
      if Spawn.Running and then Expected_Result /= Not_Specified then
         Defer_Exit_Check (Step     => Step,
                           Expected => Expected_Result);

      elsif Expected_Result /= Not_Specified then
         --  The command has terminated: the check is immediate
         Check_Exit_Status (Step      => Step,
                            Expected  => Expected_Result,
                            Verbosity => Verbosity);

      else
         -- If Expected_Result = Not_Specified, Success is only
         -- determined by the run, not by the return code.
         Put_Step_Result (Step      => Step,
                          Success   => True,
                          Fail_Msg  => "Couldn't run " & Cmd,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      end if;

   end Run_Cmd;

   -- --------------------------------------------------------------------------
   function Interactive_Command_Running return Boolean is (Spawn.Running);

   -- --------------------------------------------------------------------------
   procedure Wait_Command_End (Step      :     Step_Type'Class;
                               Verbosity :     Verbosity_Levels;
                               OK        : out Boolean) is
   begin
      if Spawn.Running then
         Spawn.Pump (Spawn.End_Timeout);
      end if;
      Check_Deadline (Step      => Step,
                      Verbosity => Verbosity,
                      OK        => OK);
      if OK then
         OK := not Spawn.Running;
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
      Input : constant String :=
                To_String (Step.Data.Object_String)
                & (if With_Newline
                   then [1 => Ada.Characters.Latin_1.LF]
                   else "");
   begin
      Put_Debug_Line ("Send_Input" & Input'Image);

      --  A deferred exit status check is resolved first (cf. the
      --  design discussion D2): if the command exited with an error
      --  since the previous step, the failure is reported on the
      --  successfully run step, and this step is not executed,
      --  instead of a misleading "no command is running" message.
      if Deferred_Pending then
         if Spawn.Running then
            Wait_Quiet;
         end if;
         if not Spawn.Running then
            Resolve_Deferred_Exit_Check (Verbosity, OK);
            if not OK then
               return;
            end if;
         end if;
      end if;

      if not Spawn.Running then
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
      Spawn.Set_Input_Mark;
      Spawn.Send (Input, OK);
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
     (Spawn.Output_Since_Input (Output));

   function Stderr_Since_Input (Stderr : Text) return Text is
     (Spawn.Stderr_Since_Input (Stderr));


   -- --------------------------------------------------------------------------
   procedure Reset_Interactive_State is
   begin
      Spawn.Reset;
      Deferred_Pending := False;
      Deferred_Step    := null;
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
                       Success   => (Last_Returned_Code = Expected)
                                      xor Step.Data.Negated,
                       Fail_Msg  => (if Step.Data.Negated
                                     then "Exit code should differ from"
                                            & Expected'Image
                                            & ", got" & Last_Returned_Code'Image
                                     else "Expected exit code" & Expected'Image
                                          & ", got" & Last_Returned_Code'Image),
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Exit_Code_Is;

end BBT.Tests.Actions.Commands;
