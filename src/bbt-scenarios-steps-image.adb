-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

separate (BBT.Scenarios.Steps)

function Image (T : Tokens) return String is
begin
   case T is
      when Given            => return "Given";
      when When_P           => return "When";
      when Then_P           => return "Then";
      when No_SA            => return "";
      when Executable       => return "executable";
      when New_SA           => return "new";
      when No_Subject       => return "";
      when Output_Subj      => return "output";
      when Subject_File     => return "`file`";
      when Dir_Subject      => return "`dir`";
      when Subject_Text     => return "`text`";
      when No_Verb          => return "";
      when Run              => return "run";
      when Fail             => return "fail";
      when Successful_Run   => return "successfully run";
      when Get              => return "get";
      when Get_No           => return "get no";
      when Does_Not_Contain => return "does not contain";
      when Contains         => return "contains";
      when Matches          => return "matches";
      when Does_Not_Match   => return "does not match";
      when Containing       => return "containing";
      when Is_V             => return "is";
      when Is_No            => return "is no";
      when No_Object        => return "";
      when Output_Obj       => return "output";
      when Obj_File_Name    => return "`file`";
      when Obj_Dir_Name     => return "`dir`";
      when Obj_Text         => return "`text`";
      when Command_List     => return "`cmd` [or `cmd`]*";
      when Error            => return "error";
      when Unordered        => return "unordered";
   end case;
end Image;