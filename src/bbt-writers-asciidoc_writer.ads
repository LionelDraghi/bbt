-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

package BBT.Writers.Asciidoc_Writer is

   -- --------------------------------------------------------------------------
   procedure Initialize;

private
   type Asciidoc_Writer is new Abstract_Writer with null record;

   -- --------------------------------------------------------------------------
   overriding function Default_Extension
     (Writer : Asciidoc_Writer) return String is (".adoc");

   overriding function File_Pattern (Writer : Asciidoc_Writer) return String;
   -- Regexp of files of this format

   overriding procedure Enable_Output
     (Writer : Asciidoc_Writer; File_Name : String := "");

   overriding function Is_Of_The_Format (Writer    : Asciidoc_Writer;
                                         File_Name : String) return Boolean;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Document_Start (Writer : Asciidoc_Writer;
                                            Doc    : Document_Type'Class);
   overriding procedure Put_Feature_Start (Writer  : Asciidoc_Writer;
                                           Feat    : Feature_Type'Class);
   overriding procedure Put_Scenario_Start (Writer : Asciidoc_Writer;
                                            Scen      : Scenario_Type'Class;
                                            Verbosity : Verbosity_Levels);
   overriding procedure Put_Step_Result (Writer   : Asciidoc_Writer;
                                         Step     : Step_Type'Class;
                                         Success  : Boolean;
                                         Fail_Msg : String;
                                         Loc       : IO.Location_Type;
                                         Verbosity : Verbosity_Levels);
   overriding procedure Put_Scenario_Result (Writer    : Asciidoc_Writer;
                                             Scen      : Scenario_Type'Class;
                                             Verbosity : Verbosity_Levels);
   overriding procedure Put_Summary (Writer : Asciidoc_Writer);
   overriding procedure Put_Detailed_Results (Writer : Asciidoc_Writer);

   -- --------------------------------------------------------------------------
   overriding procedure Put_Step
     (Writer : Asciidoc_Writer;
      Step   : Step_Type'Class);
   overriding procedure Explain
     (Writer : Asciidoc_Writer;
      Step   : Step_Type'Class);
   overriding procedure Put_Scenario_Title
     (Writer : Asciidoc_Writer; S : String);
   overriding procedure Put_Feature_Title
     (Writer : Asciidoc_Writer; S : String);
   overriding procedure Put_Document_Title
     (Writer : Asciidoc_Writer; S : String);

end BBT.Writers.Asciidoc_Writer;
