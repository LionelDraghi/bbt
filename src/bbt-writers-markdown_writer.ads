-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

package BBT.Writers.Markdown_Writer is

   -- --------------------------------------------------------------------------
   procedure Initialize;

private
   type Markdown_Writer is new Abstract_Writer with null record;

   -- --------------------------------------------------------------------------
   overriding function File_Extensions
     (Writer : Markdown_Writer) return String is ("*.md");

   -- --------------------------------------------------------------------------
   overriding function Default_Extension
     (Writer : Markdown_Writer) return String is (".md");

   overriding procedure Enable_Output (Writer : Markdown_Writer;
                                       File_Name : String := "");

   -- --------------------------------------------------------------------------
   overriding procedure Put_Summary (Writer : Markdown_Writer);
   overriding procedure Put_Step_Result (Writer : Markdown_Writer;
                                         Step      : BBT.Documents.Step_Type;
                                         Success   : Boolean;
                                         Fail_Msg  : String;
                                         Loc       : BBT.IO.Location_Type);
   overriding procedure Put_Overall_Results
     (Writer    : Markdown_Writer;
      Results   : BBT.Results.Test_Results_Count);

   -- --------------------------------------------------------------------------
   overriding procedure Put_Step (Writer : Markdown_Writer;
                                  Step   : Step_Type);
   overriding procedure Put_Scenario_Title (Writer : Markdown_Writer;
                                            S      : String);
   overriding procedure Put_Feature_Title (Writer : Markdown_Writer;
                                           S      : String);
   overriding procedure Put_Document_Title (Writer : Markdown_Writer;
                                            S      : String);

end BBT.Writers.Markdown_Writer;
