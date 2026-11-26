-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

--  Shared backend support library (crate bbt_backends): the services
--  every GUI backend needs and no GUI stack provides, per
--  docs/dev/design_discussions.md (D16). The harness deadline and the
--  polling (Eventually semantics), text normalization for accessible
--  names and titles, and, from the webapp backend on, the failure
--  diagnostics helpers.
--
--  What a backend needs from its GUI toolkit never lands here: those
--  helpers go down into the concerned backend crate.

package BBT_Backends is

   Harness_Deadline : constant Duration := 12.0;
   --  The default bounded time a GUI state change gets: window
   --  creation, accessibility subtree build, event propagation and
   --  widget state settling are asynchronous. The ToDoMVC witness
   --  notes report a fresh GTK 4 application registration on the
   --  accessibility registry taking whole seconds on a loaded
   --  session.

   Poll_Interval : constant Duration := 0.2;

end BBT_Backends;
