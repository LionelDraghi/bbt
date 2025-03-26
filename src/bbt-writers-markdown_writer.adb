-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO; use BBT.IO;

with Ada.Strings.Fixed;

with GNAT.Regexp;

package body BBT.Writers.Markdown_Writer is

   -- -----------------------------------------------------------------------
   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := IO.MD_Writer)
                             renames BBT.IO.Put_Line;
   pragma Warnings (Off, Put_Debug_Line);

   -- --------------------------------------------------------------------------
   Processor       : aliased Markdown_Writer;
   Regexp          : constant String := ("*.md");
   Compiled_Regexp : constant GNAT.Regexp.Regexp
     := GNAT.Regexp.Compile (Pattern        => Regexp,
                             Glob           => True,
                             Case_Sensitive => False);

   -- --------------------------------------------------------------------------
   procedure Initialize is
   begin
      Register (Writer     => Processor'Access,
                For_Format => Markdown);
   end Initialize;

   -- --------------------------------------------------------------------------
   function File_Pattern
     (Writer : Markdown_Writer) return String is (Regexp);

   -- --------------------------------------------------------------------------
   procedure Enable_Output (Writer    : Markdown_Writer;
                            File_Name : String := "") is
   begin
      null;
   end Enable_Output;

   -- --------------------------------------------------------------------------
   function Is_Of_The_Format (Writer    : Markdown_Writer;
                              File_Name : String) return Boolean is
   begin
      return GNAT.Regexp.Match (S => File_Name,
                                R => Compiled_Regexp);
   end Is_Of_The_Format;

   -- --------------------------------------------------------------------------
   procedure Put_Summary (Writer : Markdown_Writer) is
   begin
      null;
   end Put_Summary;

   Pref : constant array (Boolean) of String (1 .. 10) :=
            [True  => "    OK  : ",
             False => "*** NOK : "];

   -- --------------------------------------------------------------------------
   procedure Put_Step_Result (Writer    : Markdown_Writer;
                              Step      : BBT.Documents.Step_Type;
                              Success   : Boolean;
                              Fail_Msg  : String;
                              Loc       : BBT.IO.Location_Type) is
      Pre  : constant String := Pref (Success);
   begin
      Put_Debug_Line ("Put_Step_Result = " & Step'Image);
      Put_Debug_Line ("Step.Parent     = " & Step.Parent_Scenario'Image);
      Documents.Add_Result (Success, Step.Parent_Scenario.all);
      if Success then
         IO.Pause_Tee; -- We don't want this level of detail in the
         --               generated test index.
         IO.Put_Line (Item      => Pre & (+Step.Step_String) & "  ",
                      Verbosity => IO.Verbose);
         IO.Restore_Tee;
      else
         IO.Put_Line (Pre & (+Step.Step_String) & " (" & IO.Image (Loc) & ")  ",
                      Verbosity => IO.Normal);
         IO.Put_Error (Fail_Msg & "  ", Loc);
      end if;
   end Put_Step_Result;

   -- --------------------------------------------------------------------------
   procedure Put_Overall_Results
     (Writer    : Markdown_Writer;
      Results   : BBT.Tests.Results.Test_Results_Count)
   is
      use BBT.Tests.Results;
      subtype Count_String is String (1 .. 7);
      Blank_Image : constant Count_String := [others => ' '];
      function Count (Test : Test_Result) return Count_String is
      begin
         return Ada.Strings.Fixed.Overwrite (Source   => Blank_Image,
                                             Position => 1,
                                             New_Item => Results (Test)'Image);
      end Count;
   begin -- Fixme : should be factorized in the parent package
      New_Line (Verbosity => Quiet);
      if Results (Failed) = 0 and Results (Empty) = 0
      then Put_Line ("## Summary : **Success**", Verbosity => Quiet);
      else Put_Line ("## Summary : **Fail**", Verbosity => Quiet);
      end if;
      New_Line (Verbosity => Quiet);
      Put_Line ("| Status     | Count |", Verbosity => Quiet);
      Put_Line ("|------------|-------|", Verbosity => Quiet);
      Put_Line ("| Failed     |" & Count (Failed) & "|", Verbosity => Quiet);
      Put_Line ("| Successful |" & Count (Successful) & "|", Verbosity => Quiet);
      Put_Line ("| Empty      |" & Count (Empty) & "|", Verbosity => Quiet);
      if Results (Failed) /= 0 then
         Put_Line ("| Skipped    |" & Count (Skipped) & "|", Verbosity => Quiet);
      end if;
      New_Line (Verbosity => Quiet);
   end Put_Overall_Results;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Step (Writer : Markdown_Writer;
                                  Step   : Step_Type) is
   begin
      Put_Line (Line (Step.Location)'Image & ": Step """ &
                (+Step.Step_String) & """");
      -- Put_Line (Step'Image);
   end Put_Step;

   overriding procedure Put_Scenario_Title (Writer : Markdown_Writer;
                                            S      : String) is
   begin
      Put_Line ("#### " & S);
   end Put_Scenario_Title;

   overriding procedure Put_Feature_Title (Writer : Markdown_Writer;
                                           S      : String) is
   begin
      Put_Line ("### " & S);
   end Put_Feature_Title;

   overriding procedure Put_Document_Title (Writer : Markdown_Writer;
                                            S      : String) is
   begin
      Put_Line ("## " & S);
   end Put_Document_Title;

end BBT.Writers.Markdown_Writer;
