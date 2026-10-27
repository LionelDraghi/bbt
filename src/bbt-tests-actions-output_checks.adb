-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
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
   procedure Mask_Ignored_Parts (Actual   : in out Text;
                                 Expected : in out Text;
                                 Step     :        Step_Type'Class) is
   -- Mask the volatile parts of both sides of the comparison, as stated
   -- by the (ignoring ...) clause of the step, and warn when one of the
   -- patterns matches nothing on the actual side: the scenario probably
   -- declares volatile something the tested program does not print
      Counts : Pattern_Counts;
   begin
      if Step.Data.Ignore_Patterns = No_Ignore_Pattern
        and then Step.Data.Ignore_Regexps.Is_Empty
      then
         return;
      end if;
      Expected := Mask (Expected, Step.Data.Ignore_Patterns);
      Actual   := Mask (Actual, Step.Data.Ignore_Patterns, Counts);
      -- The matches-nothing warning is for the masking patterns only:
      -- a path syntax normalization that rewrites nothing is a no-op,
      -- not a sign that the scenario ignores something never printed
      for P in Ignore_Pattern range Time .. Location loop
         if Step.Data.Ignore_Patterns (P) and then Counts (P) = 0 then
            IO.Put_Warning ("the ignoring " & Image (P)
                              & " pattern matches nothing on the actual side",
                            Step.Location);
         end if;
      end loop;
      for R of Step.Data.Ignore_Regexps loop
         declare
            Count : Natural;
         begin
            Expected := Mask (Expected, R);
            Actual   := Mask (Actual, R, Count);
            if Count = 0 then
               IO.Put_Warning ("the ignoring """ & R
                                 & """ pattern matches nothing on the"
                                 & " actual side",
                               Step.Location);
            end if;
         end;
      end loop;
   end Mask_Ignored_Parts;

   -- --------------------------------------------------------------------------
   procedure Output_Not_Empty (Output    : Text;
                               Step      : Step_Type'Class;
                               Verbosity : Verbosity_Levels) is
   begin
      Put_Step_Result (Step     => Step,
                       Success  => (not Text_Utilities.Is_Empty (Output))
                                     xor Step.Data.Negated,
                       Fail_Msg => (if Step.Data.Negated
                                    then String'("output not null : "
                                                 & Output'Image)
                                    else String'("no output found")),
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Output_Not_Empty;

   -- --------------------------------------------------------------------------
   procedure Output_Is (Output    : Text;
                        Step      : Step_Type'Class;
                        Verbosity : Verbosity_Levels) is
      use Texts;
      T2     : Text := Get_Expected (Step);
      Actual : Text := Output;
   begin
      -- Put_Debug_Line ("++++++++++ Output_Is Step = " & Step'Image);
      -- Put_Debug_Line ("++++++++++ dir = " & Settings.Launch_Directory);
      Put_Debug_Line ("++++++++++ Output = ");
      -- Put_Text (Item => Output);
      Put_Debug_Line ("++++++++++ T2 = ");
      -- Put_Text (Item => T2);
      Mask_Ignored_Parts (Actual, T2, Step);
      declare
         Matched : constant Boolean :=
           Is_Equal (Actual, T2,
                     Case_Insensitive   => Settings.Ignore_Casing,
                     Ignore_Blanks      => Settings.Ignore_Whitespaces,
                     Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                     Sort_Texts         => Step.Data.Ignore_Order);
         Success : constant Boolean := Matched xor Step.Data.Negated;
         -- First line of Msg is the error message,
         -- then comes the side-by-side comparison of expected and actual.
         -- It is computed only on error.
         Msg : Text := (if Success then Empty_Text
                        else Side_By_Side (T2, Actual,
                                           Case_Insensitive   => Settings.Ignore_Casing,
                                           Ignore_Whitespaces => Settings.Ignore_Whitespaces));
      begin
         if not Success then
            Msg.Prepend (if Step.Data.Negated
                         then "Output equal to unexpected content:"
                         else "Output not equal to expected:");
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
      T2     : Text := Get_Expected (Step);
      Actual : Text := Output;
   begin
      Put_Debug_Line ("++++++++++ Output_Contains Step = " & Step'Image);
      Put_Debug_Line ("++++++++++ dir = " & Settings.Launch_Directory);
      Mask_Ignored_Parts (Actual, T2, Step);
      declare
         Matched : constant Boolean :=
           Contains
             (Actual, T2,
              Case_Insensitive   => Settings.Ignore_Casing,
              Ignore_Whitespaces => Settings.Ignore_Whitespaces,
              Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
              Sort_Texts         => Step.Data.Ignore_Order);
         Success : constant Boolean := Matched xor Step.Data.Negated;
         -- On failure, Msg is either the missing expected content, or,
         -- when the step is negated, the position and content of the
         -- intruder. It is computed only on error.
         Msg : Text := Empty_Text;
      begin
         if not Success then
            if Step.Data.Negated then
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
            else
               Msg.Append ("Output:  " & Code_Fenced_Image (Actual) &
                           "does not contain expected:  " &
                           Code_Fenced_Image (T2));
            end if;
         end if;
         Put_Step_Result (Step     => Step,
                          Success  => Success,
                          Fail_Msg => Msg,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      end;
   end Output_Contains;

   -- --------------------------------------------------------------------------
   procedure Output_Matches (Output : Text;
                             Step      : Step_Type'Class;
                             Verbosity : Verbosity_Levels) is
      Regexp : constant String := Get_Expected (Step) (1);
   begin
      Put_Debug_Line ("Output_Matches ");
      Put_Step_Result (Step     => Step,
                       Success  => Text_Utilities.Matches (Output, Regexp)
                                     xor Step.Data.Negated,
                       Fail_Msg => "Output:  " & Code_Fenced_Image (Output) &
                         (if Step.Data.Negated
                          then "match unexpected:  "
                          else "does not match expected:  ") & Regexp,
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Output_Matches;

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
                          Success  => Text_Utilities.Matches (T1, Regexp)
                                        xor Step.Data.Negated,
                          Fail_Msg => "File:  " & File_Name &
                            (if Step.Data.Negated
                             then " should not match:  "
                             else " does not match expected:  ") & Regexp,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end File_Matches;

   -- --------------------------------------------------------------------------
   procedure Files_Is (Step      : Step_Type'Class;
                       Verbosity : Verbosity_Levels) is
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
      T2        : Text := Get_Expected (Step);
   begin
      Put_Debug_Line ("Files_Is " & File_Name &
                        " T1 = " & T1'Image &
                        " T2 = " & T2'Image);
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         Mask_Ignored_Parts (T1, T2, Step);
         declare
            Matched : constant Boolean :=
              Is_Equal (T1, T2,
                        Case_Insensitive   => Settings.Ignore_Casing,
                        Ignore_Blanks      => Settings.Ignore_Whitespaces,
                        Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                        Sort_Texts         => Step.Data.Ignore_Order);
            Success : constant Boolean := Matched xor Step.Data.Negated;
            -- First line of Msg is the error message,
            -- then comes the side-by-side comparison of expected and actual.
            -- It is computed only on error.
            Msg : Text := Empty_Text;
         begin
            if not Success then
               if Step.Data.Negated then
                  Msg.Append (File_Name & " expected to be different from " &
                              Code_Fenced_Image (T2));
               else
                  Msg := Side_By_Side (T2, T1,
                                       Case_Insensitive   => Settings.Ignore_Casing,
                                       Ignore_Whitespaces => Settings.Ignore_Whitespaces);
                  Msg.Prepend (File_Name & " not equal to expected:");
               end if;
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
   procedure File_Contains (Step      : Step_Type'Class;
                            Verbosity : Verbosity_Levels) is
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
      T2        : Text := Get_Expected (Step);
   begin
      Put_Debug_Line ("File_Contains " & File_Name &
                        " T1 = " & T1'Image &
                        " T2 = " & T2'Image);
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         Mask_Ignored_Parts (T1, T2, Step);
         declare
            Matched : constant Boolean :=
              Contains
                (T1, T2,
                 Case_Insensitive   => Settings.Ignore_Casing,
                 Ignore_Whitespaces => Settings.Ignore_Whitespaces,
                 Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                 Sort_Texts         => Step.Data.Ignore_Order);
            Success : constant Boolean := Matched xor Step.Data.Negated;
            -- On failure, Msg is either the missing expected content, or,
            -- when the step is negated, the position and content of the
            -- intruder. It is computed only on error.
            Msg : Text := Empty_Text;
         begin
            if not Success then
               if Step.Data.Negated then
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
               else
                  Msg.Append (File_Name &
                              " does not contain expected:  " &
                              Code_Fenced_Image (T2));
               end if;
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
   end File_Contains;

end BBT.Tests.Actions.Output_Checks;
