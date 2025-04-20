-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO,
     BBT.Settings,
     BBT.Tests.Results;

use BBT.IO,
    BBT.Settings;

with File_Utilities;

with GNAT.Regexp;

package body BBT.Writers.Text_Writer is

   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := IO.Text_Writer)
                             renames BBT.IO.Put_Line;
   pragma Warnings (Off, Put_Debug_Line);

   -- --------------------------------------------------------------------------
   Processor       : aliased Text_Writer;
   Regexp          : constant String := ("*.txt");
   Compiled_Regexp : constant GNAT.Regexp.Regexp
     := GNAT.Regexp.Compile (Pattern        => Regexp,
                             Glob           => True,
                             Case_Sensitive => False);

   -- --------------------------------------------------------------------------
   procedure Initialize is
   begin
      Register (Writer     => Processor'Access,
                For_Format => Txt);
   end Initialize;

   -- --------------------------------------------------------------------------
   overriding function File_Pattern
     (Writer : Text_Writer) return String is (Regexp);

   -- --------------------------------------------------------------------------
   overriding procedure Enable_Output (Writer    : Text_Writer;
                                       File_Name : String := "") is
   begin
      null;
   end Enable_Output;

   -- --------------------------------------------------------------------------
   overriding function Is_Of_The_Format (Writer    : Text_Writer;
                                         File_Name : String) return Boolean is
   begin
      return GNAT.Regexp.Match (S => File_Name,
                                R => Compiled_Regexp);
   end Is_Of_The_Format;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Summary (Writer : Text_Writer) is
      use all type Tests.Results.Test_Result;
   begin
      New_Line (Quiet);
      if Tests.Results.Success then
         Put_Line ("Success,"
                   & Count (Successful)'Image & " scenarios OK"
                   & (if Count (Empty) = 0
                     then ""
                     else "," & Count (Empty)'Image & " empty scenarios"),
                   Verbosity => Quiet);
      else
         Put_Line ("**Fail**",
                   Verbosity => Quiet);
      end if;
   end Put_Summary;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Detailed_Results (Writer : Text_Writer) is
      use Tests.Results;
      Verbosity_Level : constant Verbosity_Levels
        := (if Success then Verbose else Quiet);
   begin
      New_Line (Verbosity_Level);
      Put_Line ("| Status     | Count |", Verbosity => Verbosity_Level);
      Put_Line ("|------------|-------|", Verbosity => Verbosity_Level);
      Put_Line ("| Failed     |" & Count_String_Image (Failed) & "|",
                Verbosity => Verbosity_Level);
      Put_Line ("| Successful |" & Count_String_Image (Successful) & "|",
                Verbosity => Verbosity_Level);
      Put_Line ("| Empty      |" & Count_String_Image (Empty) & "|",
                Verbosity => Verbosity_Level);
      if Count (Failed) /= 0 then
         Put_Line ("| Skipped    |" & Count_String_Image (Skipped) & "|",
                   Verbosity => Verbosity_Level);
      end if;
      New_Line (Verbosity_Level);
   end Put_Detailed_Results;

   Pref : constant array (Boolean) of String (1 .. 10) :=
            [True  => "    OK  : ",
             False => "*** NOK : "];

   -- --------------------------------------------------------------------------
   overriding procedure Put_Document_Start (Writer : Text_Writer;
                                            Doc    : Document_Type'Class) is
   begin
      Put_Document_Start (Get_Writer (For_Format => Markdown).all,
                          Doc);
   end Put_Document_Start;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Feature_Start (Writer  : Text_Writer;
                                           Feat    : Feature_Type'Class) is
   begin
      IO.Put_Line ("  ### Feature: " & (+Feat.Name) & "  ");
   end Put_Feature_Start;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Scenario_Start (Writer : Text_Writer;
                                            Scen   : Scenario_Type'Class) is
   begin
      null;
      -- IO.Put_Line ("  - Scen start: " & (+Scen.Name) & "  ");
   end Put_Scenario_Start;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Step_Result (Writer    : Text_Writer;
                                         Step      : BBT.Documents.Step_Type'Class;
                                         Success   : Boolean;
                                         Fail_Msg  : String;
                                         Loc       : BBT.IO.Location_Type) is
      Pre  : constant String := Pref (Success);
   begin
      Put_Debug_Line ("Put_Step_Result = " & Step'Image);
      Put_Debug_Line ("Step.Parent     = " & Step.Parent'Image);
      Documents.Add_Result (Success, Parent (Step).all);
      if Success then
         IO.Pause_Tee; --  We don't want this level of detail in the
         --  generated test index.
         IO.Put_Line (Item      => Pre & (+Step.Data.Src_Code) & "  ",
                      Verbosity => IO.Verbose);
         IO.Restore_Tee;
      else
         IO.Put_Line (Pre & (+Step.Data.Src_Code) & " (" & IO.Image (Loc) & ")  ",
                      Verbosity => IO.Normal);
         IO.Put_Error (Fail_Msg & "  ", Loc);
      end if;
   end Put_Step_Result;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Scenario_Result (Writer : Text_Writer;
                                             Scen   : Scenario_Type'Class)
   is
      Path_To_Scen  : constant String
        := File_Utilities.Short_Path (From_Dir => Settings.Result_Dir,
                                      To_File  => (+Parent_Doc (Scen).Name));
      Link_Image    : constant String
        := ("[" & (+Scen.Name) & "](" & Path_To_Scen & ")");
      use Tests.Results;
   begin
      case Tests.Results.Result (Scen) is
         when Empty =>
            Put_Line ("  - [ ] scenario " & Link_Image &
                        " is empty, nothing tested  ",
                      Verbosity => Normal);
         when Successful =>
            Put_Line ("  - [X] scenario " & Link_Image & " pass  ",
                      Verbosity => Normal);
         when Skipped | Failed =>
            Put_Line ("  - [ ] scenario " & Link_Image & " fails  ",
                      Verbosity => Quiet);
      end case;
   end Put_Scenario_Result;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Step (Writer : Text_Writer;
                                  Step   : Step_Type'Class) is
   begin
      Put_Line (Step.Location'Image & " Step """ &
                (+Step.Data.Src_Code) & """");
      -- Put_Line (Step'Image);
   end Put_Step;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Scenario_Title (Writer : Text_Writer;
                                            S      : String) is
   begin
      Put_Line (S);
   end Put_Scenario_Title;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Feature_Title (Writer : Text_Writer;
                                           S      : String) is
   begin
      Put_Line (S);
   end Put_Feature_Title;

   -- --------------------------------------------------------------------------
   overriding procedure Put_Document_Title (Writer : Text_Writer;
                                            S      : String) is
   begin
      Put_Line (S);
   end Put_Document_Title;

end BBT.Writers.Text_Writer;
