-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Util.Strings.Vectors;

package BBT.Tests.Actions.Commands.Spawn is

   --  The process machinery of the Commands action, extracted from the
   --  Commands body: spawn a command, pump its output into the output
   --  files, and reap it. This package holds no test semantics: no Step,
   --  no reporting, no exit status checks. The scenario deadline is part
   --  of it, as it only bounds waiting on the running command (cf. the
   --  design discussion D6).

   function Running return Boolean;
   --  The last spawned command is still running (not yet reaped)

   function Last_Exit_Code return Integer;
   --  Exit code of the last reaped command (0 before the first one)

   function Output_Bytes return Natural;
   function Err_Bytes return Natural;
   --  Bytes written so far to the output / error files, used to detect
   --  when the command is quiet

   function Input_Bytes return Natural;
   --  Bytes produced when the last input was sent, or at the start of
   --  the command if no input was sent yet: the response to check is
   --  the output produced after that point

   function Timeout_Expired return Boolean;
   --  Is the scenario timeout armed and expired? (cf. D6)

   procedure Set_Deadline;
   --  Arm the scenario deadline, when set in the settings

   procedure Start
     (Args        :     Util.Strings.Vectors.Vector;
      Merged      :     Boolean;
      Interactive :     Boolean;
      Out_Name    :     String;
      Err_Name    :     String;
      OK          : out Boolean);
   --  Truncate or create the output files, reset the output counters,
   --  and spawn the command described by Args (already built by the
   --  caller, shell prefix included), on a pseudo terminal when
   --  Interactive. The standard error goes to Err_Name, or to Out_Name
   --  when Merged. On failure, OK is False, the output files are closed
   --  so that the following checks can read them, and the failure cause
   --  is debug logged

   procedure Send (Data : String; OK : out Boolean);
   --  Write Data to the standard input of the running command.
   --  OK is False if the write failed

   procedure Poll (Timeout : Duration);
   --  Wait for command output during Timeout at most, read the available
   --  data and write it to the output files, then reap the command when
   --  both output streams have reached end of file

   procedure Pump (Timeout : Duration);
   --  Poll the command output until the command is no more running,
   --  or until Timeout is elapsed, or until the scenario deadline.
   --  No timeout when Timeout = 0.0, but the deadline still applies

   procedure Kill;
   --  Stop the running command (SIGKILL) and mark it not running,
   --  without draining nor reaping: this is used when the command output
   --  doesn't matter anymore, the process being disposed right after

   procedure Kill_And_Drain;
   --  Stop the running command (SIGKILL), then pump a moment, so that
   --  its output is drained into the output files and it is reaped

   procedure Dispose;
   --  Close the output files, kill and reap a still running command,
   --  and free the process object

   procedure Reset;
   --  Kill a command still running at the end of a scenario after a
   --  grace period, reset the output baselines, and dispose the process

   procedure Close_Output_Files;
   --  Flush and close the output files, so that the following checks
   --  can read them

   procedure Set_Input_Mark;
   --  Record the output baselines: the output checks that follow an
   --  input step apply only to the output produced after the mark

   function Output_Since_Input (Output : Text) return Text;
   --  The part of Output produced after the last input

   function Stderr_Since_Input (Output : Text) return Text;
   --  The part of Stderr produced after the last input

   Quiet_Step    : constant Duration := 0.02;
   Quiet_Window  : constant Duration := 0.10;
   Quiet_Timeout : constant Duration := 5.0;
   End_Timeout   : constant Duration := 10.0;

end BBT.Tests.Actions.Commands.Spawn;
