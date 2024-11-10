-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Strings.Text_Buffers;
with BBT.IO;                use BBT.IO;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package BBT.Scenarios.MDG_Lexer is
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

   type Line_Kind is (Feature_Line,
                      Scenario_Line,
                      Background_Line,
                      Step_Line,
                      Code_Fence,
                      Text_Line,
                      Empty_Line);

   type Line_Attributes (Kind : Line_Kind) is record
      case Kind is
         when Feature_Line | Scenario_Line | Background_Line =>
            -- The line is divided in
            --   ### Kind : Name
            -- For example, for
            --   # Feature: Staying alive
            -- Kind will be Feature_Line and Name "Staying alive"
            Name : Unbounded_String := Null_Unbounded_String;

         when Step_Line =>
            -- Step_Ln, contains the part after the list marker `-`
            -- Note that other "legal" Markdown list maker ('+' and '*') are
            -- intentionally ignored so that it will be possible to use it in
            -- comments without interfering with step line parsing.
            -- As a consequence, line starting with '+' and '*' will be
            -- considered here as Text_Line.
            Step_Ln : Unbounded_String := Null_Unbounded_String;

         when Text_Line =>
            -- Contains the full line, is considered as comment by the Lexer
            Line : Unbounded_String := Null_Unbounded_String;

         when Code_Fence | Empty_Line =>
            null;

      end case;
   end record with Put_Image => Put_Image;

   procedure Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      A      :        Line_Attributes);

   type Parsing_Context is limited private;

   -- --------------------------------------------------------------------------
   function Initialize_Context return Parsing_Context;
   function Parse_Line (Line    : access constant String;
                        Context : in out Parsing_Context;
                        Loc     : Location_Type)
                        return Line_Attributes;

private
   type Parsing_Context is record
      In_Code_Fence : Boolean;
      In_Scenario   : Boolean;
   end record;

end BBT.Scenarios.MDG_Lexer;
