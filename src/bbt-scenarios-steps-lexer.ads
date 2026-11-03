-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Text_Utilities; use Text_Utilities;

private package BBT.Scenarios.Steps.Lexer is

   type Token_Type is (Keyword, Identifier, Code_Span, Empty);
   -- In Markdown, Code_Span denote a word or phrase enclosed in
   -- backticks (`).
   -- Refer to https://spec.commonmark.org/0.31.2/#code-spans
   -- for specification
   -- In BBT, backticks enclose the command to run, or a file name,
   -- or an expected output.

   procedure Initialize_Lexer;

   procedure Register_Keyword (K : String);
   --  Register one keyword spelling in the lexer keyword set. The set
   --  is no longer a flat hardcoded list: it is built by the rule set
   --  initializers (cf. Initialize_Grammar in BBT.Scenarios.Steps), so
   --  that a vocabulary extension registers its own keywords without
   --  altering the base lexer. K must be lower case: Is_A_Keyword
   --  lowercases the token before lookup.

   function Next_Token (Line     : access constant String;
                        First    :    out Positive; -- Index of first character in token
                        Last     :    out Natural;  -- Index of last character in token
                        Tok_Type :    out Token_Type) return String;

   function Next_Token (Line     : access constant String;
                        Tok_Type :    out Token_Type) return String;

   function More_Token return Boolean;

   procedure Put_Keywords;

end BBT.Scenarios.Steps.Lexer;
