-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO,
     BBT.Tests.Results;

use BBT,
    BBT.IO;

with GNAT.Regexp;

package body BBT.Writers.Asciidoc_Writer is

   -- -----------------------------------------------------------------------
   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := IO.Adoc_Writer)
                             renames BBT.IO.Put_Line;
   pragma Warnings (Off, Put_Debug_Line);

   -- --------------------------------------------------------------------------
   Processor : aliased Asciidoc_Writer;
   Regexp    : constant String := ("*.adoc");

   Compiled_Regexp : constant GNAT.Regexp.Regexp
     := GNAT.Regexp.Compile (Pattern        => Regexp,
                             Glob           => True,
                             Case_Sensitive => False);

   -- --------------------------------------------------------------------------
   procedure Initialize is
   begin
      Register (Writer     => Processor'Access,
                For_Format => AsciiDoc);
   end Initialize;

   -- --------------------------------------------------------------------------
   overriding function File_Pattern
     (Writer : Asciidoc_Writer) return String is (Regexp);

   -- --------------------------------------------------------------------------
   overriding procedure Enable_Output (Writer    : Asciidoc_Writer;
                                       File_Name : String := "") is
   begin
      null;
   end Enable_Output;

   -- --------------------------------------------------------------------------
   overriding function Is_Of_The_Format (Writer    : Asciidoc_Writer;
                                         File_Name : String) return Boolean is
   begin
      return GNAT.Regexp.Match (S => File_Name,
                                R => Compiled_Regexp);
   end Is_Of_The_Format;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Summary (Writer    : Asciidoc_Writer) is
      use Tests.Results;
   begin
      if Success then
         Put_Line ("## Summary : **Success**,"
                   & Count (Successful)'Image & " scenarios OK"
                   & (if Count (Empty) = 0
                     then ""
                     else "," & Count (Empty)'Image & " empty scenarios"),
                   Verbosity => Quiet);
      else
         Put_Line ("## Summary : **Fail**",
                   Verbosity => Quiet);
      end if;
   end Put_Summary;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Detailed_Results (Writer    : Asciidoc_Writer) is
      use Tests.Results;
      Verbosity_Level : constant Verbosity_Levels
        := (if Success then Verbose else Quiet);
   begin
      New_Line (Verbosity => Verbosity_Level);
      Put_Line ("|============", Verbosity => Verbosity_Level);
      Put_Line ("| Result     | Count |", Verbosity => Verbosity_Level);
      Put_Line ("|------------|-------|", Verbosity => Verbosity_Level);
      Put_Line ("| Failed     |" & Count_String_Image (Failed) & "|", Verbosity => Verbosity_Level);
      Put_Line ("| Successful |" & Count_String_Image (Successful) & "|", Verbosity => Verbosity_Level);
      Put_Line ("| Empty      |" & Count_String_Image (Empty) & "|", Verbosity => Verbosity_Level);
      if Count (Failed) /= 0 then
         Put_Line ("| Skipped    |" & Count_String_Image (Skipped) & "|", Verbosity => Verbosity_Level);
      end if;
      Put_Line ("|============", Verbosity => Verbosity_Level);
      New_Line (Verbosity => Verbosity_Level);
   end Put_Detailed_Results;

   -- --------------------------------------------------------------------------
   Pref : constant array (Boolean) of String (1 .. 10) :=
            [True  => "    OK  : ",
             False => "*** NOK : "];

   -- --------------------------------------------------------------------------
   overriding procedure Put_Document_Start (Writer : Asciidoc_Writer;
                                            Doc    : Document_Type'Class) is
   begin
      Put_Document_Start (Get_Writer (For_Format => Markdown).all,
                          Doc);
   end Put_Document_Start;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Feature_Start (Writer  : Asciidoc_Writer;
                                           Feat    : Feature_Type'Class) is
   begin
      Put_Feature_Start (Get_Writer (For_Format => Markdown).all,
                         Feat);
   end Put_Feature_Start;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Scenario_Start (Writer : Asciidoc_Writer;
                                            Scen   : Scenario_Type'Class) is
   begin
      Put_Scenario_Start (Get_Writer (For_Format => Markdown).all,
                          Scen);
   end Put_Scenario_Start;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Step_Result (Writer   : Asciidoc_Writer;
                                         Step     : BBT.Documents.Step_Type'Class;
                                         Success  : Boolean;
                                         Fail_Msg : String;
                                         Loc      : BBT.IO.Location_Type)
   is
      Pre  : constant String := Pref (Success);
   begin
      Documents.Add_Result (Success, Step.Parent_Scenario.all);
      if Success then
         IO.Pause_Tee;
         IO.Put_Line (Item      => Pre & (+Step.Step_String) & "  ",
                      Verbosity => IO.Verbose);
         IO.Restore_Tee;
      else
         IO.Put_Line (Pre & (+Step.Step_String) & " (" & IO.Image (Loc) & ")  ",
                      Verbosity => IO.Normal);
      end if;
   end Put_Step_Result;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Scenario_Result (Writer : Asciidoc_Writer;
                                             Scen   : Scenario_Type'Class) is
   begin
      Put_Scenario_Result (Get_Writer (For_Format => Markdown).all,
                           Scen);
   end Put_Scenario_Result;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Step (Writer : Asciidoc_Writer;
                                  Step   : Step_Type'Class) is
   begin
      Put_Line (Line (Step.Location)'Image & ": Step """ &
                (+Step.Step_String) & """");
      Put_Line (Step'Image);
   end Put_Step;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Scenario_Title (Writer : Asciidoc_Writer;
                                            S      : String) is
   begin
      Put_Line ("=== " & S);
   end Put_Scenario_Title;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Feature_Title (Writer : Asciidoc_Writer;
                                           S      : String) is
   begin
      Put_Line ("== " & S);
   end Put_Feature_Title;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Document_Title (Writer : Asciidoc_Writer;
                                            S      : String) is
   begin
      Put_Line ("= " & S);
   end Put_Document_Title;

end BBT.Writers.Asciidoc_Writer;
