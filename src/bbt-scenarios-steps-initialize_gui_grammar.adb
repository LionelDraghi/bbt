-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

separate (BBT.Scenarios.Steps)

--  GUI rule set initialization, cf. docs/proposed/gui_vocabulary.md.
--  GUI rules and keywords are registered here, after the CLI ones,
--  through the same registry (The_Rules) and the same keyword
--  registration mechanism (Lexer.Register_Keyword) as the CLI rule
--  set. Empty as long as the GUI vocabulary is not implemented.
procedure Initialize_GUI_Grammar is

   --  The GUI rule set keywords. Only registered here, like the rule
   --  set, so that they never perturb the CLI processing: the CLI
   --  sentences never contain a bare "window", "button"... keyword.
   procedure Register_Keywords is
   begin
      Lexer.Register_Keyword ("window");
      Lexer.Register_Keyword ("appears");
      Lexer.Register_Keyword ("button");
      Lexer.Register_Keyword ("textbox");
      Lexer.Register_Keyword ("dialog");
      Lexer.Register_Keyword ("press");
      Lexer.Register_Keyword ("enabled");
      Lexer.Register_Keyword ("disabled");
   end Register_Keywords;

begin
   Register_Keywords;
   --  GUI checks, Then steps
   Set ([Then_P], No_SA, Window_Subj, Appears, No_Object,
        GUI_Window_Appears, "a window appears / a `title` window appears");
   Set ([Then_P], No_SA, No_Subject, Is_V, Obj_Window_Name,
        GUI_Window_Appears, "there is a window named `title`");
   Set ([Then_P], No_SA, No_Subject, Is_V, Obj_Widget_Name,
        GUI_Widget_Exists, "there is a `role` named `name`");

   Set ([Then_P], No_SA, No_Subject, Is_V, Obj_Widget_State,
        GUI_Widget_State, "the `name` `role` is enabled / disabled");

   --  GUI actions & window checks
   Set ([Then_P], No_SA, Window_Subj, Contains, Obj_Text,
        GUI_Window_Contains, "the window contains `text`");
   Set ([When_P], No_SA, No_Subject, Press, Obj_Text,
        GUI_Press_Widget, "I press the `name` `role`");

end Initialize_GUI_Grammar;
