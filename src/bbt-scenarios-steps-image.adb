-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
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
      when Stderr_Subj      => return "stderr";
      when Exit_Subj        => return "exit";
      when Env_Var_Subj     => return "`var`";
      when Window_Subj      => return "window";
      when File_Subj        => return "`file`";
      when Dir_Subj         => return "`dir`";
      when Text_Subj        => return "`text`";
      when No_Verb          => return "";
      when Run              => return "run";
      when Fail             => return "fail";
      when Get              => return "get";
      when Contains         => return "contains";
      when Matches          => return "matches";
      when Containing       => return "containing";
      when Is_V             => return "is";
      when Type_Text        => return "type";
      when Enter_Text       => return "enter";
      when Appears          => return "appears";
      when Press            => return "press";
      when No_Object        => return "";
      when Obj_Output       => return "output";
      when Obj_Stderr       => return "error output";
      when Obj_File_Name    => return "`file`";
      when Obj_Dir_Name     => return "`dir`";
      when Obj_Text         => return "`text`";
      when Obj_Command_List => return "`cmd` [or `cmd`]*";
      when Obj_Error        => return "error";
      when Obj_Window_Name  => return "`window name`";
      when Obj_Widget_Name  => return "`widget name`";
      when Obj_Widget_State => return "enabled | disabled";
      when Unordered        => return "unordered";
   end case;
end Image;