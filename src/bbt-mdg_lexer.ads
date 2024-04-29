with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package BBT.MDG_Lexer is

   -- Minimal lexer to parse a subset of a [Markdown with Gherkin](https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md)
   -- file.
   -- The magic of BBT is in the processing of steps lines, that are
   -- parsed in the Step_Lexer package

   type Line_Kind is (Feature_Line,
                      Scenario_Line,
                      Step_Line,
                      Code_Fence,
                      Text_Line,
                      Empty_Line);

   -- https://spec.commonmark.org/0.31.2/#fenced-code-blocks
   -- Fenced code block starts and ends with a code fence,
   -- that is a sequence of at least three consecutive backtick characters (`)
   -- or tildes (~)
   -- Within BBT, Fenced code block contains text, and are used to specify
   -- an expected output or an expected part of a file.

   type Line_Attributes (Kind : Line_Kind) is record
      case Kind is
         when Feature_Line | Scenario_Line =>
            -- For Feature or Scenario, the line is divided in
            --   ### Kind : Name
            -- For example, for
            --   # Feature: Staying alive
            -- Kind will be Feature_Line and Name "Staying alive"
            Name : Unbounded_String := Null_Unbounded_String;

         when Text_Line =>
            -- For Text_Line, Line contains the full line
            Line : Unbounded_String := Null_Unbounded_String;

         when Step_Line =>
            -- For Step_Line, contains the part after the list marker (* or -)
            Step_Ln : Unbounded_String := Null_Unbounded_String;

         when Code_Fence | Empty_Line =>
            null;

      end case;
   end record;

   -- --------------------------------------------------------------------------
   function Parse_Line (Line : access constant String)
                        return Line_Attributes;

end BBT.MDG_Lexer;
