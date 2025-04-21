-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

package BBT.Writers.Text_Writer is

   -- --------------------------------------------------------------------------
   procedure Initialize;

private
   type Text_Writer is new Abstract_Writer with null record;

   -- --------------------------------------------------------------------------
   overriding function Default_Extension
     (Writer : Text_Writer) return String is
     (".txt");

   overriding function File_Pattern
     (Writer : Text_Writer) return String;
   -- Regexp of files of this format

   overriding procedure Enable_Output
     (Writer : Text_Writer; File_Name : String := "");

   overriding function Is_Of_The_Format (Writer    : Text_Writer;
                                         File_Name : String) return Boolean;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Document_Start (Writer : Text_Writer;
                                            Doc    : Document_Type'Class);
   overriding procedure Put_Feature_Start (Writer  : Text_Writer;
                                           Feat    : Feature_Type'Class);
   overriding procedure Put_Scenario_Start (Writer : Text_Writer;
                                            Scen   : Scenario_Type'Class);
   overriding procedure Put_Step_Result
     (Writer   : Text_Writer;
      Step     : Step_Type'Class;
      Success  : Boolean;
      Fail_Msg : String;
      Loc      : BBT.IO.Location_Type);
   overriding procedure Put_Scenario_Result (Writer : Text_Writer;
                                             Scen   : Scenario_Type'Class);
   overriding procedure Put_Summary
     (Writer    : Text_Writer);
   overriding procedure Put_Detailed_Results
     (Writer    : Text_Writer);

   -- --------------------------------------------------------------------------
   overriding procedure Put_Step (Writer : Text_Writer;
                                  Step   : Step_Type'Class);
   overriding procedure Put_Scenario_Title
     (Writer : Text_Writer; S : String);
   overriding procedure Put_Feature_Title
     (Writer : Text_Writer; S : String);
   overriding procedure Put_Document_Title
     (Writer : Text_Writer; S : String);

end BBT.Writers.Text_Writer;
