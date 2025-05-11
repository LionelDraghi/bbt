-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Text_Buffers;

private package BBT.Scenarios is
-- Provide (in child packages) services related to the Scenario manipulation
-- that is Scenario file list management, and and scenario parsing.

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
   -- Fixme: To be moved as dispatching in Writers

end BBT.Scenarios;
