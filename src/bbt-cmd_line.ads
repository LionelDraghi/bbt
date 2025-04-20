-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Settings;

use BBT;

private package BBT.Cmd_Line is

   procedure Go_Next_Arg;
   procedure Go_Back_Previous_Arg;
   function On_Last_Arg return Boolean;
   function Current_Arg return String;
   function More_Args return Boolean;

   function Dash_To_Underscore (S : String) return String;
   -- Transform option like "--list-file" in "--list_file"
   procedure Set_Cmd (C : Settings.Command);
   procedure Analyze;

   procedure Put_Settings;
   procedure Put_Help (Topic : Settings.Help_Topic);
   procedure Put_Trace_Topics;
   procedure Create_Template; -- Fixme: shouldn't be here

end BBT.Cmd_Line;
