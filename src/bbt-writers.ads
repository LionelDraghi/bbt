-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Model.Documents,
     BBT.Model.Features,
     BBT.Model.Scenarios,
     BBT.Model.Steps,
     BBT.IO;

use BBT.Model,
    BBT.Model.Documents,
    BBT.Model.Features,
    BBT.Model.Scenarios,
    BBT.Model.Steps,
    BBT.IO;

private package BBT.Writers is
-- This package defines common services for all types of output, and
-- the abstract interface that each new writer (defined in child
-- packages) should implement.

   -- --------------------------------------------------------------------------
   type Output_Format is (Markdown, Badge, Txt, AsciiDoc, Terminal);
   -- WARNING : When calling Enable, there should be a registered Writer,
   --           otherwise an access check exception will be raised at run time.
   --           (cf. the child procedure Initialize)

   -- -------------------------------------------------------------------------
   function Is_Enabled (F : Output_Format) return Boolean;
   function No_Output_Format_Enabled return Boolean;

   function File_Pattern (For_Format : Output_Format) return String;
   -- Returns the regexp that will be used to identify sources files
   -- of this format.

   function Default_Extension (For_Format : Output_Format) return String;
   -- Preferred extension when creating a file of this format

   procedure File_Format (File_Name    :     String;
                          Found        : out Boolean;
                          Found_Format : out Output_Format);

   procedure Enable_Output (For_Format : Output_Format;
                            File_Name  : String := "");
   -- Will find and initialize the writer for this format

   -- -------------------------------------------------------------------------
   -- Runner Events
   -- During test run, each event result in a call on those procedure, that
   -- will be dispatch on each enabled Formatter
   -- procedure Put_Summary;
   procedure Put_Document_Start
     (Doc : Document_Type'Class);
   procedure Put_Feature_Start
     (Feat : Feature_Type'Class);
   procedure Put_Scenario_Start
     (Scen      : Scenario_Type'Class;
      Verbosity : Verbosity_Levels := Normal);
   procedure Put_Step_Result
     (Step      : Step_Type'Class;
      Success   : Boolean;
      Fail_Msg  : String;
      Loc       : IO.Location_Type;
      Verbosity : Verbosity_Levels := Normal);
   procedure Put_Scenario_Result
     (Scen      : Scenario_Type'Class;
      Verbosity : Verbosity_Levels := Normal);
   procedure Put_Overall_Results;


   -- -------------------------------------------------------------------------
   -- Output of the scenario as stored by bbt
   procedure Put_Document_List (Doc_List : Documents.List);

   -- -------------------------------------------------------------------------
   -- Output of the scenario as understood by bbt
   procedure Explain (Doc_List : Documents.List);

private
   -- -------------------------------------------------------------------------
   type Abstract_Writer is abstract tagged limited null record;

   function Default_Extension
     (Writer : Abstract_Writer) return String is abstract;
   -- Preferred extension when creating a file of this format

   function File_Pattern
     (Writer : Abstract_Writer) return String is abstract;
   -- Regexp of files of this format

   function Is_Of_The_Format (Writer    : Abstract_Writer;
                              File_Name : String) return Boolean is abstract;
   -- Returns True if the given file is processed by this writer.

   procedure Enable_Output (Writer    : Abstract_Writer;
                            File_Name : String := "") is abstract;
   -- Enable the Writer

   -- -------------------------------------------------------------------------
   procedure Put_Document_Start (Writer    : Abstract_Writer;
                                 Doc       : Document_Type'Class) is abstract;
   procedure Put_Feature_Start (Writer    : Abstract_Writer;
                                Feat      : Feature_Type'Class) is abstract;
   procedure Put_Scenario_Start (Writer    : Abstract_Writer;
                                 Scen      : Scenario_Type'Class) is abstract;
   procedure Put_Step_Result (Writer    : Abstract_Writer;
                              Step      : Step_Type'Class;
                              Success   : Boolean;
                              Fail_Msg  : String;
                              Loc       : IO.Location_Type) is abstract;
   procedure Put_Scenario_Result (Writer : Abstract_Writer;
                                  Scen   : Scenario_Type'Class) is abstract;
   procedure Put_Summary (Writer : Abstract_Writer) is abstract;
   procedure Put_Detailed_Results (Writer : Abstract_Writer) is abstract;



   -- -------------------------------------------------------------------------
   -- Output of the scenario as understood and stored by bbt
   procedure Put_Step (Writer : Abstract_Writer;
                       Step   : Step_Type'Class) is abstract;
   procedure Explain (Writer : Abstract_Writer;
                      Step   : Step_Type'Class) is abstract;
   procedure Put_Scenario_Title (Writer : Abstract_Writer;
                                 S      : String) is abstract;
   procedure Put_Feature_Title (Writer : Abstract_Writer;
                                S      : String) is abstract;
   procedure Put_Document_Title (Writer : Abstract_Writer;
                                 S      : String) is abstract;

   -- -------------------------------------------------------------------------
   type Interface_Access is access all Abstract_Writer'Class;
   procedure Register (Writer     : Interface_Access;
                       For_Format : Output_Format);
   function Get_Writer (For_Format : Output_Format) return Interface_Access;

end BBT.Writers;
