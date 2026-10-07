-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Settings;
with BBT.Writers;                       use BBT.Writers;
with BBT.Tests.Actions.File_Operations; use BBT.Tests.Actions.File_Operations;

package body BBT.Tests.Actions.Output_Checks is

   procedure Put_Debug_Line
     (Item      : String;
      Location  : IO.Location_Type    := IO.No_Location;
      Verbosity : IO.Verbosity_Levels := IO.Debug;
      Topic     : IO.Extended_Topics  := IO.Step_Actions)
      renames IO.Put_Line;
   pragma Warnings (Off, Put_Debug_Line);
   -- --------------------------------------------------------------------------
   procedure Check_No_Output (Output : Text;
                              Step      : Step_Type'Class;
                              Verbosity : Verbosity_Levels) is
      use Texts;
   begin
      Put_Step_Result (Step     => Step,
                       Success  => Output = Empty_Text,
                       Fail_Msg => String'("output not null : " & Output'Image),
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Check_No_Output;

   -- --------------------------------------------------------------------------
   procedure Output_Is (Output    : Text;
                        Step      : Step_Type'Class;
                        Verbosity : Verbosity_Levels) is
      use Texts;
      T2 : constant Text := Get_Expected (Step);
   begin
      -- Put_Debug_Line ("++++++++++ Output_Is Step = " & Step'Image);
      -- Put_Debug_Line ("++++++++++ dir = " & Settings.Launch_Directory);
      Put_Debug_Line ("++++++++++ Output = ");
      -- Put_Text (Item => Output);
      Put_Debug_Line ("++++++++++ T2 = ");
      -- Put_Text (Item => T2);
      declare
         Success : constant Boolean :=
           Is_Equal (Output, T2,
                     Case_Insensitive   => Settings.Ignore_Casing,
                     Ignore_Blanks      => Settings.Ignore_Whitespaces,
                     Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                     Sort_Texts         => Step.Data.Ignore_Order);
         -- First line of Msg is the error message,
         -- then comes the side-by-side comparison of expected and actual.
         -- It is computed only on error.
         Msg : Text := (if Success then Empty_Text
                        else Side_By_Side (T2, Output,
                                           Case_Insensitive   => Settings.Ignore_Casing,
                                           Ignore_Whitespaces => Settings.Ignore_Whitespaces));
      begin
         if not Success then
            Msg.Prepend ("Output not equal to expected:");
         end if;
         Put_Step_Result (Step     => Step,
                          Success  => Success,
                          Fail_Msg => Msg,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      end;
   end Output_Is;

   -- --------------------------------------------------------------------------
   procedure Output_Contains (Output    : Text;
                              Step      : Step_Type'Class;
                              Verbosity : Verbosity_Levels) is
      T2  : constant Text := Get_Expected (Step);
   begin
      Put_Debug_Line ("++++++++++ Output_Contains Step = " & Step'Image);
      Put_Debug_Line ("++++++++++ dir = " & Settings.Launch_Directory);
      Put_Step_Result (Step     => Step,
                       Success  => Contains
                         (Output, T2,
                          Case_Insensitive   => Settings.Ignore_Casing,
                          Ignore_Whitespaces => Settings.Ignore_Whitespaces,
                          Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                          Sort_Texts         => Step.Data.Ignore_Order),
                       Fail_Msg => "Output:  " & Code_Fenced_Image (Output) &
                         "does not contain expected:  " &
                         Code_Fenced_Image (T2),
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Output_Contains;

   -- --------------------------------------------------------------------------
   procedure Output_Does_Not_Contain (Output : Text;
                                      Step      : Step_Type'Class;
                                      Verbosity : Verbosity_Levels) is
      T2  : constant Text := Get_Expected (Step);
   begin
      Put_Debug_Line ("Output_Does_Not_Contain ");
      declare
         Success : constant Boolean :=
           not Contains (Output, T2,
                         Case_Insensitive   => Settings.Ignore_Casing,
                         Ignore_Whitespaces => Settings.Ignore_Whitespaces,
                         Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                         Sort_Texts         => Step.Data.Ignore_Order);
         -- First line of Msg gives the position of the intruder,
         -- then come the intruder lines. It is computed only on error.
         Msg : Text := Empty_Text;
      begin
         if not Success then
            declare
               I : constant Line_Index'Base :=
                 Index_Of (Output, T2,
                           Case_Insensitive   => Settings.Ignore_Casing,
                           Ignore_Whitespaces => Settings.Ignore_Whitespaces);
               -- Last line of the intruder, limited by the end of Output
               Last : constant Line_Index :=
                 Line_Index'Min (I + Line_Index (T2.Length) - 1,
                                 Output.Last_Index);
            begin
               Msg.Append (String'("Output contains unexpected at line" &
                                   I'Image & ":"));
               for J in I .. Last loop
                  Msg.Append (Output (J));
               end loop;
            end;
         end if;
         Put_Step_Result (Step     => Step,
                          Success  => Success,
                          Fail_Msg => Msg,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      end;
   end Output_Does_Not_Contain;

   -- --------------------------------------------------------------------------
   procedure Output_Matches (Output : Text;
                             Step      : Step_Type'Class;
                             Verbosity : Verbosity_Levels) is
      Regexp : constant String := Get_Expected (Step) (1);
   begin
      Put_Debug_Line ("Output_Matches ");
      Put_Step_Result (Step     => Step,
                       Success  => Text_Utilities.Matches (Output, Regexp),
                       Fail_Msg => "Output:  " & Code_Fenced_Image (Output) &
                         "does not match expected:  " & Regexp,
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Output_Matches;

   -- --------------------------------------------------------------------------
   procedure Output_Does_Not_Match (Output : Text;
                                    Step      : Step_Type'Class;
                                    Verbosity : Verbosity_Levels) is
      Regexp : constant String := Get_Expected (Step) (1);
   begin
      Put_Debug_Line ("Output_Does_Not_Match ");
      Put_Step_Result (Step     => Step,
                       Success  => not Text_Utilities.Matches (Output, Regexp),
                       Fail_Msg => "Output:  " & Code_Fenced_Image (Output) &
                         "match unexpected:  " & Regexp,
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Output_Does_Not_Match;

   -- --------------------------------------------------------------------------
   procedure File_Matches (Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels) is
      Regexp    : constant String := Get_Expected (Step) (1);
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
   begin
      Put_Debug_Line ("File_Matches ");
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         Put_Step_Result (Step     => Step,
                          Success  => Text_Utilities.Matches (T1, Regexp),
                          Fail_Msg => "File:  " & File_Name &
                            " does not match expected:  " & Regexp,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end File_Matches;

   -- --------------------------------------------------------------------------
   procedure File_Does_Not_Match (Step      : Step_Type'Class;
                                  Verbosity : Verbosity_Levels) is
      Regexp    : constant String := Get_Expected (Step) (1);
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
   begin
      Put_Debug_Line ("File_Does_Not_Match ");
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         Put_Step_Result (Step     => Step,
                          Success  => not Text_Utilities.Matches (T1, Regexp),
                          Fail_Msg => "File:  " & File_Name &
                            " should not match:  " & Regexp,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end File_Does_Not_Match;

   -- --------------------------------------------------------------------------
   procedure Files_Is (Step      : Step_Type'Class;
                       Verbosity : Verbosity_Levels) is
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
      T2        : constant Text   := Get_Expected (Step);
   begin
      Put_Debug_Line ("Files_Is " & File_Name &
                        " T1 = " & T1'Image &
                        " T2 = " & T2'Image);
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         declare
            Success : constant Boolean :=
              Is_Equal (T1, T2,
                        Case_Insensitive   => Settings.Ignore_Casing,
                        Ignore_Blanks      => Settings.Ignore_Whitespaces,
                        Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                        Sort_Texts         => Step.Data.Ignore_Order);
            -- First line of Msg is the error message,
            -- then comes the side-by-side comparison of expected and actual.
            -- It is computed only on error.
            Msg : Text := (if Success then Empty_Text
                           else Side_By_Side (T2, T1,
                                              Case_Insensitive   => Settings.Ignore_Casing,
                                              Ignore_Whitespaces => Settings.Ignore_Whitespaces));
         begin
            if not Success then
               Msg.Prepend (File_Name & " not equal to expected:");
            end if;
            Put_Step_Result (Step     => Step,
                             Success  => Success,
                             Fail_Msg => Msg,
                             Loc       => Step.Location,
                             Verbosity => Verbosity);
         end;
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end Files_Is;

   -- --------------------------------------------------------------------------
   procedure Files_Is_Not (Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels) is
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
      T2        : constant Text   := Get_Expected (Step);
   begin
      Put_Debug_Line ("Files_Is_Not " & File_Name);
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         Put_Step_Result (Step     => Step,
                          Success  => not Is_Equal
                            (T1, T2,
                             Case_Insensitive   => Settings.Ignore_Casing,
                             Ignore_Blanks      => Settings.Ignore_Whitespaces,
                             Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                             Sort_Texts         => Step.Data.Ignore_Order),
                          Fail_Msg => File_Name & " expected to be different from " &
                            Code_Fenced_Image (T2),
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end Files_Is_Not;

   -- --------------------------------------------------------------------------
   procedure File_Contains (Step      : Step_Type'Class;
                            Verbosity : Verbosity_Levels) is
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
      T2        : constant Text   := Get_Expected (Step);
   begin
      Put_Debug_Line ("File_Contains " & File_Name &
                        " T1 = " & T1'Image &
                        " T2 = " & T2'Image);
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         Put_Step_Result (Step     => Step,
                          Success  => Contains
                            (T1, T2,
                             Case_Insensitive   => Settings.Ignore_Casing,
                             Ignore_Whitespaces => Settings.Ignore_Whitespaces,
                             Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                             Sort_Texts         => Step.Data.Ignore_Order),
                          Fail_Msg => File_Name &
                            " does not contain expected:  " &
                            Code_Fenced_Image (T2),
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end File_Contains;

   -- --------------------------------------------------------------------------
   procedure File_Does_Not_Contain (Step      : Step_Type'Class;
                                    Verbosity : Verbosity_Levels) is
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
      T2        : constant Text   := Get_Expected (Step);
   begin
      Put_Debug_Line ("File_Does_Not_Contain " & File_Name);
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         declare
            Success : constant Boolean :=
              not Contains (T1, T2,
                            Case_Insensitive   => Settings.Ignore_Casing,
                            Ignore_Whitespaces => Settings.Ignore_Whitespaces,
                            Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                            Sort_Texts         => Step.Data.Ignore_Order);
            -- First line of Msg gives the position of the intruder,
            -- then come the intruder lines. It is computed only on error.
            Msg : Text := Empty_Text;
         begin
            if not Success then
               declare
                  I : constant Line_Index'Base :=
                    Index_Of (T1, T2,
                              Case_Insensitive   => Settings.Ignore_Casing,
                              Ignore_Whitespaces => Settings.Ignore_Whitespaces);
                  -- Last line of the intruder, limited by the end of the file
                  Last : constant Line_Index :=
                    Line_Index'Min (I + Line_Index (T2.Length) - 1,
                                    T1.Last_Index);
               begin
                  Msg.Append (String'(File_Name &
                                      " contains unexpected at line" &
                                      I'Image & ":"));
                  for J in I .. Last loop
                     Msg.Append (T1 (J));
                  end loop;
               end;
            end if;
            Put_Step_Result (Step     => Step,
                             Success  => Success,
                             Fail_Msg => Msg,
                             Loc       => Step.Location,
                             Verbosity => Verbosity);
         end;
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end File_Does_Not_Contain;

end BBT.Tests.Actions.Output_Checks;
