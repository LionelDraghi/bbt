-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

package BBT.Scenarios.Readers.Adoc_Reader is
--  Minimal lexer to parse an [AsciiDoc](https://docs.asciidoctor.org/)
--  fiel with a Gherkin content similar to MDG.
--
--  Keywords actually processed are a subset of
--  [standard Cucumber](https://cucumber.io/docs/gherkin/reference/) :
--  == Feature:
--  == Example: (or Scenario)
--  == Background:
--  - Given
--  - When
--  - Then
--  - And
--  - But
--
--  The level of the Heading (the number of '=') is indifferent, but the colon
--  is needed.
--
--  Step lines start with '-'.
--  Note that the common AsciiDoc bullet list marker ('*')is  not considered as
--  step, so that users can more easily
--  put lists in scenario comments, without confusing the lexer.
--
--  This Lexer do not further analyse the steps, this is done in the Step_Parser
--  package.

   -- --------------------------------------------------------------------------
   procedure Initialize;

private
   type Adoc_Reader is new Abstract_Reader with null record;

   overriding function File_Pattern
     (Reader : Adoc_Reader) return String;

   overriding function Format
     (Reader : Adoc_Reader) return Valid_Input_Format is (Adoc);

   overriding function Is_Of_The_Format
     (Reader    : Adoc_Reader;
      File_Name : String) return Boolean;

   -- --------------------------------------------------------------------------
   function Remove_Emphasis (Reader    : Adoc_Reader;
                             S         : String) return String;
   -- Removes potential emphasis on keyword
   -- * : bold
   -- _ : italic
   -- ` : monospace
   -- [](https://asciidoc.org/#specifications)
   -- (there is no precise reference for now, as AsciiDoc specification is a
   -- work in progress)

   -- --------------------------------------------------------------------------
   overriding function Find_Heading_Mark
     (Reader      : Adoc_Reader;
      Line        : String;
      First       : out Natural;
      Last        : out Natural;
      Title_First : out Natural;
      Title_Last  : out Natural;
      Location    : Location_Type) return Boolean;
   -- === **Header** : xyz
   -- First will point 'H'
   -- Last  will point 'r'
   -- Title_First will point 'x'
   -- Title_Last  will point 'z'

   function Code_Fence_Line (Reader           : Adoc_Reader;
                             Line             : String;
                             Look_For_Closing : Boolean) return Boolean;

end BBT.Scenarios.Readers.Adoc_Reader;
