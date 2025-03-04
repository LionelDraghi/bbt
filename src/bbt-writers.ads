-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Documents; use BBT.Documents;
with BBT.IO;
with BBT.Results;

private package BBT.Writers is
-- This package defines common services for all types of output, and
-- the abstract interface that each new writer (defined in child
-- packages) should implement.

   -- --------------------------------------------------------------------------
   type Format is (MD, Badge, Txt, AsciiDoc, Terminal);
   -- WARNING : When calling Enable, there should be a registered Writer,
   --           otherwise an access check exception will be raised at run time.
   --           (cf. the child procedure Initialize)

   -- -------------------------------------------------------------------------
   function Is_Enabled (F : Format) return Boolean;

   function File_Extensions (For_Format : Format) return String;
   -- Returns the regexp that will be used to identify sources files
   -- of this format.

   function Default_Extension (For_Format : Format) return String;
   -- Preferred extension when creating a file of this format

   -- asciidoc : *.(adoc|asciidoc)

   procedure Enable_Output (For_Format : Format;
                            File_Name  : String := "");
   -- Will find and initialize the writer for this format.

   -- -------------------------------------------------------------------------
   -- Runner Events
   -- During test run, each event result in a call on those procedure, that
   -- will be dispatch on each enabled Formatter
   procedure Put_Summary;
   procedure Put_Step_Result (Step     : BBT.Documents.Step_Type;
                              Success  : Boolean;
                              Fail_Msg : String;
                              Loc      : BBT.IO.Location_Type);
   procedure Put_Overall_Results (Results : BBT.Results.Test_Results_Count);


   -- -------------------------------------------------------------------------
   -- Output of the scenario as understood and stored by bbt
   procedure Put_Document_List (Doc_List : Documents.Documents_Lists.Vector);

private
   -- -------------------------------------------------------------------------
   type Abstract_Writer is abstract tagged limited null record;

   function File_Extensions
     (Writer : Abstract_Writer) return String is abstract;
   -- Returns the regexp that will be used to identify sources files
   -- processed by this writer.
   function Default_Extension
     (Writer : Abstract_Writer) return String is abstract;
   -- Preferred extension when creating a file of this format

   procedure Enable_Output (Writer    : Abstract_Writer;
                            File_Name : String := "") is abstract;
   -- Enable the Writer

   -- -------------------------------------------------------------------------
   procedure Put_Summary (Writer : Abstract_Writer) is abstract;
   procedure Put_Step_Result (Writer    : Abstract_Writer;
                              Step      : BBT.Documents.Step_Type;
                              Success   : Boolean;
                              Fail_Msg  : String;
                              Loc       : BBT.IO.Location_Type) is abstract;
   procedure Put_Overall_Results
     (Writer    : Abstract_Writer;
      Results   : BBT.Results.Test_Results_Count) is abstract;

   -- -------------------------------------------------------------------------
   -- Output of the scenario as understood and stored by bbt
   procedure Put_Step (Writer : Abstract_Writer;
                       Step   : Step_Type) is abstract;
   procedure Put_Scenario_Title (Writer : Abstract_Writer;
                                 S      : String) is abstract;
   procedure Put_Feature_Title (Writer : Abstract_Writer;
                                S      : String) is abstract;
   procedure Put_Document_Title (Writer : Abstract_Writer;
                                 S      : String) is abstract;

   -- -------------------------------------------------------------------------
   type Interface_Access is access all Abstract_Writer'class;
   procedure Register (Writer     : Interface_Access;
                       For_Format : Format);

end BBT.Writers;
