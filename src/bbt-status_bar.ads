-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

private package BBT.Status_Bar is

   --  -------------------------------------------------------------------------
   type Scenario_Outcome is (OK, Failed, Skipped);

   --  -------------------------------------------------------------------------
   procedure Enable (Force : Boolean);
   --  Enable the bar if the standard output is a terminal, or if Force.
   --  On a redirected output (pipe, file, CI log), the bar would only
   --  fill the output with escape sequences.
   --  The terminal capabilities (Unicode level) are detected here.

   --  -------------------------------------------------------------------------
   procedure Initialize_Progress_Bar (Max_Event : Natural);
   --  Set the total number of scenarios to process.
   --  The counter replaces the activity label as soon as the total is known.

   --  -------------------------------------------------------------------------
   procedure Set_Current_File (File_Name : String);
   --  Set the name of the document currently run.

   --  -------------------------------------------------------------------------
   procedure Next_Scenario (Outcome : Scenario_Outcome);
   --  One more scenario has been processed: increment the counter and
   --  the failure count, and redraw. As soon as one scenario has
   --  failed, the bar turns red and stays red until the end of the
   --  run, so that the failure is never erased by the next ticks.

   --  -------------------------------------------------------------------------
   procedure Tick;
   --  Redraw the bar: the spinner is time based, so that the animation
   --  stays regular whatever the duration of the steps; this is called
   --  on each step to keep the bar alive.

   --  -------------------------------------------------------------------------
   procedure Put_Activity (S : String);
   --  Set the label of the activity in progress, and redraw.

   --  -------------------------------------------------------------------------
   procedure Clear;
   --  Erase the bar from the current line, if displayed.
   --  Called by BBT.IO before any output on the standard output.

   --  -------------------------------------------------------------------------
   procedure Draw;
   --  Redraw the bar at the current position, if the current line is clean.
   --  Called by BBT.IO after a completed line.

end BBT.Status_Bar;
