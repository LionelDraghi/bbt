-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Text_Utilities; use Text_Utilities;

with Ada.Characters.Latin_1;
with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;                   use Ada.Directories;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Ada.Strings.Maps;

private package BBT.Scenarios.Step_Parser.Lexer is

   type Token_Type is (Keyword, Identifier, Code_Span, Empty);
   -- In Markdown, Code_Span denote a word or phrase enclosed in
   -- backticks (`).
   -- Refer to https://spec.commonmark.org/0.31.2/#code-spans
   -- for specification
   -- In BBT, backticks enclose the command to run, or a file name,
   -- or an expected output.

   --  type Token is record
   --     Tok_Type    : Token_Type;
   --     Token_Image : String;
   --  end record;
   --  package Token_List is new Ada.Containers.List (Element_Type => Token);

   procedure Initialize_Lexer;

   function Next_Token
     (Line    : access constant String;
     Tok_Type : out Token_Type) return String;

   function More_Token return Boolean;

   --  function Is_A_Keyword
   --    (S : access constant String; First : Positive; Last : Natural := 0)
   --     return Boolean;

   procedure Put_Keywords;

end BBT.Scenarios.Step_Parser.Lexer;
