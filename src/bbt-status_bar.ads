-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

private package BBT.Status_Bar is

   --  -------------------------------------------------------------------------
   procedure Enable;
   procedure Disable;

   --  -------------------------------------------------------------------------
   procedure Initialize_Progress_Bar (Max_Event : Positive);

   --  -------------------------------------------------------------------------
   procedure Progress_Bar_Next_Step (File_Name : String);

   --  -------------------------------------------------------------------------
   procedure Put_Activity (S : String);

end BBT.Status_Bar;
