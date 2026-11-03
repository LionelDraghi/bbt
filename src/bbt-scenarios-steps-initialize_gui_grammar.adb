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
begin
   null;
end Initialize_GUI_Grammar;
