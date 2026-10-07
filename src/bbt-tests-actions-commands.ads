-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO,
     BBT.Model,
     BBT.Model.Steps;

use BBT.IO,
    BBT.Model,
    BBT.Model.Steps;

with Text_Utilities; use Text_Utilities;

package BBT.Tests.Actions.Commands is

   type Run_Result is (Not_Specified, Success, Failure);

   function Is_Success (I : Integer) return Boolean;
   function Is_Failure (I : Integer) return Boolean is
     (not Is_Success (I));

   procedure Run_Cmd (Step                       :     Step_Type'Class;
                      Cmd                        :     String;
                      Output_Name                :     String;
                      Expected_Result            :     Run_Result;
                      Verbosity                  :     Verbosity_Levels;
                      Spawn_OK                   : out Boolean;
                      Return_Code                : out Integer;
                      Error_Output_Name          :     String := "";
                      Interactive_Input_Expected :     Boolean := False);
   -- The command output (standard output and standard error) is written to
   -- Output_Name, unless Error_Output_Name is not empty: then the standard
   -- error is written to Error_Output_Name, and Output_Name receives the
   -- standard output only.
   -- The command runs asynchronously: unless Interactive_Input_Expected is
   -- set, Run_Cmd waits for its termination, and the command is finished
   -- when Run_Cmd returns. With Interactive_Input_Expected, Run_Cmd only
   -- waits for the command to become quiet, and the following steps send
   -- text to the still running command with Send_Input.

   procedure Send_Input (Step           :     Step_Type'Class;
                         With_Newline   :     Boolean;
                         Verbosity      :     Verbosity_Levels;
                         OK             : out Boolean);
   -- Send the step text to the standard input of the command started by
   -- the last Run_Cmd. Without trailing newline unless With_Newline.
   -- The step fails if no command is running.
   -- The output produced by the command after this input is the only one
   -- checked by the following output checks steps: Output_Since_Input
   -- gives that part of a Text read from an output file.

   function Interactive_Command_Running return Boolean;
   -- Is a command started by Run_Cmd, waiting for input, still running?

   procedure Wait_Quiet;
   -- Pump events until the running command, if any, has produced no more
   -- output during a short quiet window: its prompt is complete.

   procedure Wait_Response;
   -- Pump events until the running command, if any, has produced some
   -- output since this call, and then stayed quiet during a short window,
   -- or terminated: the consequence of the last input is complete.

   procedure Wait_Command_End (Step      :     Step_Type'Class;
                               Verbosity :     Verbosity_Levels;
                               OK        : out Boolean);
   -- Pump events until the running command terminates; the step fails
   -- if it is still running after a timeout.

   function Output_Since_Input (Output : Text) return Text;
   -- The part of Output produced after the last Send_Input

   function Stderr_Since_Input (Stderr : Text) return Text;
   -- The part of Stderr produced after the last Send_Input

   procedure Reset_Interactive_State;
   -- Terminate a command still running at the end of a scenario, and
   -- reset the output baselines. Called between scenarios.

   function Deferred_Exit_Check_Pending return Boolean;
   -- Is an exit status check, deferred from a successfully run step
   -- whose command was still running at its step, still pending?
   -- (cf. the design discussion D2)

   procedure Resolve_Deferred_Exit_Check (Verbosity :     Verbosity_Levels;
                                          OK        : out Boolean);
   -- Resolve the pending deferred exit status check, if any, and
   -- clear it: the check result is reported on the successfully run
   -- step line (cf. the design discussion D2). OK is False when the
   -- check fails, True when there was no pending check, or when the
   -- check passes. The command must have terminated.

   procedure Wait_Deferred_Exit_Check (Verbosity : Verbosity_Levels);
   -- Called at the end of a scenario: if an exit status check is
   -- deferred and its command is still running, wait for the command
   -- termination, then resolve the check. The wait is bounded by the
   -- scenario timeout (cf. the design discussion D6).

   procedure Set_Scenario_Deadline;
   -- Arm the scenario timeout, when set in the settings: called at
   -- the start of each scenario. The deadline covers the scenario
   -- steps, backgrounds included, not the cleanup (cf. the design
   -- discussion D6).

   procedure Check_Deadline (Step      :     Step_Type'Class;
                            Verbosity :     Verbosity_Levels;
                            OK        : out Boolean);
   -- If the scenario timeout is armed and expired: kill the still
   -- running command, if any, and report the failure on the Step
   -- line (cf. the design discussion D6). OK is False when the
   -- deadline expired.

   function Last_Exit_Code return Integer;
   -- Exit code of the last command run (0 before the first one).

   procedure Return_Error (Last_Returned_Code : Integer;
                           Step               : Step_Type'Class;
                           Verbosity          : Verbosity_Levels);
   procedure Return_No_Error (Last_Returned_Code : Integer;
                              Step               : Step_Type'Class;
                              Verbosity          : Verbosity_Levels);

   procedure Exit_Code_Is (Last_Returned_Code : Integer;
                           Step                : Step_Type'Class;
                           Verbosity          : Verbosity_Levels);

end BBT.Tests.Actions.Commands;
