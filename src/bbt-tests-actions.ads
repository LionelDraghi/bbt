-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

private package BBT.Tests.Actions is

   --  The step actions are split in one child package per domain
   --  (cf. design discussion D5): Commands runs the commands and
   --  checks their exit status, Setup creates and checks files and
   --  directories, Output_Checks checks the outputs, Environment
   --  manages the environment variables, and File_Operations holds
   --  the low level file primitives.

end BBT.Tests.Actions;
