 -----------------------------------------------------------------------------
 -- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
 -- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
 -- SPDX-FileCopyrightText: 2024, Lionel Draghi
 -----------------------------------------------------------------------------

--  Web debugger client, based on the gnoga crate (its embedded
--  server + WebSocket, no native GUI dependency, UI rendered by the
--  user's browser).
--
--  This is a client of BBT.Execution_Control, and contains as little
--  execution logic as possible: it displays the scenario file around
--  the current step, and issues commands (Continue, Finish,
--  Pause_On_Error) in reaction to user actions. The test run itself
--  is started from a dedicated task, and its events are mirrored in
--  the debugger page.
--
--  Prototype limitations:
--  - one debugger connection at a time (limitation inherent to the
--    underlying single run);
--  - no Markdown syntax highlighting yet, and no step highlighting
--    other than the current line background;
--  - closing the debugger tab releases the halted execution in keep
--    going mode (the run completes);
--  - the terminal still displays some bbt output (verbose messages,
--    notes) in parallel of the debugger page.

private package BBT.GUI is

   procedure Run;
   --  Open the debugger page (starting the embedded gnoga server,
   --  and the user's browser on its address), start the test run of
   --  the loaded documents in a dedicated task, and wait for the run
   --  completion. Closing the tab releases a halted run.

end BBT.GUI;
