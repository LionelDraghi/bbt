-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

package BBT.Writers.Markdown_Writers is

   -- --------------------------------------------------------------------------
   procedure Initialize;

private
   type Markdown_Writer is new Abstract_Writer with null record;

   -- --------------------------------------------------------------------------
   overriding function Default_Extension
     (Writer : Markdown_Writer) return String is
     (".md");

   overriding function File_Pattern
     (Writer : Markdown_Writer) return String;
   -- Regexp of files of this format

   overriding procedure Enable_Output
     (Writer : Markdown_Writer; File_Name : String := "");

   overriding function Is_Of_The_Format (Writer    : Markdown_Writer;
                                         File_Name : String) return Boolean;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Document_Start (Writer : Markdown_Writer;
                                            Doc    : Document_Type'Class);
   overriding procedure Put_Feature_Start (Writer  : Markdown_Writer;
                                           Feat    : Feature_Type'Class);
   overriding procedure Put_Scenario_Start (Writer : Markdown_Writer;
                                            Scen   : Scenario_Type'Class);
   overriding procedure Put_Step_Result
     (Writer   : Markdown_Writer;
      Step     : Step_Type'Class;
      Success  : Boolean;
      Fail_Msg : String;
      Loc      : BBT.IO.Location_Type);
   overriding procedure Put_Scenario_Result (Writer : Markdown_Writer;
                                             Scen   : Scenario_Type'Class);
   overriding procedure Put_Summary
     (Writer    : Markdown_Writer);
   overriding procedure Put_Detailed_Results
     (Writer    : Markdown_Writer);

   -- --------------------------------------------------------------------------
   overriding procedure Put_Step (Writer : Markdown_Writer;
                                  Step   : Step_Type'Class);
   overriding procedure Put_Scenario_Title
     (Writer : Markdown_Writer; S : String);
   overriding procedure Put_Feature_Title
     (Writer : Markdown_Writer; S : String);
   overriding procedure Put_Document_Title
     (Writer : Markdown_Writer; S : String);

end BBT.Writers.Markdown_Writers;
