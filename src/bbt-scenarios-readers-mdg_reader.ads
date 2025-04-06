-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

package BBT.Scenarios.Readers.MDG_Reader is
--  Minimal lexer to parse a subset of a [Markdown with Gherkin](https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md)
--  file.
--
--  Keywords actually processed are a subset of
--  [standard Cucumber](https://cucumber.io/docs/gherkin/reference/) :
--  ## Feature:
--  ## Example: (or Scenario)
--  ## Background:
--  - Given
--  - When
--  - Then
--  - And
--  - But
--
--  The level of the Heading (the number of '#') is indifferent, but the colon
--  is needed.
--
--  Step lines start with '-'.
--  Note that other [Markdown bullet list marker](https://spec.commonmark.org/0.31.2/#bullet-list-marker)
--  ('*' or '+') are not considered as step, so that users can more easily
--  put lists in scenario comments, without confusing the lexer.
--
--  This Lexer do not further analyse the steps, this is done in the Step_Parser
--  package.
--
--  As per https://spec.commonmark.org/0.31.2/#fenced-code-blocks
--  Fenced code block starts and ends with a code fence,
--  that is a sequence of at least three consecutive backtick characters (`)
--  or tildes (~)
--  Within BBT, only backtick fenced code are used, so that tildes fence code may be used freely in the file.
--  Background_Line code block contains text, and are used to specify
--  an expected output or an expected part of a file.

   -- --------------------------------------------------------------------------
   procedure Initialize;

private
   type MDG_Reader is new Abstract_Reader with null record;

   overriding function File_Pattern
     (Reader : MDG_Reader) return String;

   overriding function Format
     (Reader : MDG_Reader) return Valid_Input_Format is (MDG);

   overriding function Is_Of_The_Format
     (Reader    : MDG_Reader;
      File_Name : String) return Boolean;

   -- --------------------------------------------------------------------------
   function Remove_Emphasis (Reader    : MDG_Reader;
                             S         : String) return String;
   -- Removes potential emphasis on keyword
   -- * : bold (two *) and italic (one *)
   -- _ : underline
   -- [](https://spec.commonmark.org/0.31.2/#emphasis-and-strong-emphasis)

   -- --------------------------------------------------------------------------
   overriding function Find_Heading_Mark
     (Reader      : MDG_Reader;
      Line        : String;
      First       : out Natural;
      Last        : out Natural;
      Title_First : out Natural;
      Title_Last  : out Natural;
      Location    : Location_Type) return Boolean;
   -- ### **Header** : xyz
   -- First will point 'H'
   -- Last  will point 'r'
   -- Title_First will point 'x'
   -- Title_Last  will point 'z'
   --
   -- Refer to https://spec.commonmark.org/0.31.2/#atx-heading
   -- for specification

   -- -------------------------------------------------------------------------
   function Code_Fence_Line
     (Reader           : MDG_Reader;
      Line             : String;
      Look_For_Closing : Boolean) return Boolean;

end BBT.Scenarios.Readers.MDG_Reader;
