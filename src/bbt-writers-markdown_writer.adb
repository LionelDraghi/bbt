-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO; use BBT.IO;

with Ada.Strings.Fixed;

package body BBT.Writers.Markdown_Writer is

   -- --------------------------------------------------------------------------
   Processor : aliased Markdown_Writer;

   -- --------------------------------------------------------------------------
   procedure Initialize is
   begin
      Register (Writer     => Processor'Access,
                For_Format => MD);
   end Initialize;

   -- --------------------------------------------------------------------------
   procedure Enable_Output (Writer : Markdown_Writer;
                            File_Name : String := "") is
   begin
      null;
   end Enable_Output;

   -- --------------------------------------------------------------------------
   procedure Put_Summary (Writer : Markdown_Writer) is
   begin
      null;
   end Put_Summary;

   Pref : constant array (Boolean) of String (1 .. 10) :=
            [True  => "    OK  : ",
             False => "*** NOK : "];

   -- --------------------------------------------------------------------------
   procedure Put_Step_Result (Writer : Markdown_Writer;
                              Step      : BBT.Documents.Step_Type;
                              Success   : Boolean;
                              Fail_Msg  : String;
                              Loc       : BBT.IO.Location_Type) is
      Pre  : constant String := Pref (Success);
   begin
      --  Put_Line ("Put_Step_Result = " & Step'Image);
      --  Put_Line ("Step.Parent     = " & Step.Parent_Scenario'Image);
      Documents.Add_Result (Success, Step.Parent_Scenario.all);
      if Success then
         IO.Pause_Tee; --  We don't want this level of detail in the
         --  generated test index.
         IO.Put_Line (Item      => Pre & (+Step.Step_String) & "  ",
                      Verbosity => IO.Verbose);
         IO.Restore_Tee;
      else
         IO.Put_Line (Pre & (+Step.Step_String) & " (" & IO.Image (Loc) & ")  ",
                      Verbosity => IO.Normal);
         IO.Put_Error (Fail_Msg & "  ", Loc);
      end if;
   end Put_Step_Result;

   procedure Put_Overall_Results
     (Writer    : Markdown_Writer;
      Results   : BBT.Results.Test_Results_Count)
   is
      use BBT.Results;
      subtype Count_String is String (1 .. 7);
      Blank_Image : constant Count_String := [others => ' '];
      function Count (Test : Test_Result) return Count_String is
      begin
         return Ada.Strings.Fixed.Overwrite (Source   => Blank_Image,
                                             Position => 1,
                                             New_Item => Results (Test)'Image);
      end Count;
   begin
      New_Line (Verbosity => Quiet);
      if Results (Failed) = 0
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
      Put_Line (Step'Image);
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
