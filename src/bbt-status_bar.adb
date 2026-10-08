-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with AnsiAda,
     Ada.Calendar,
     Ada.Characters.Latin_1,
     Ada.Strings.Bounded,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded,
     Ada.Text_IO,
     BBT.Status_Bar.Platform,
     Termicap.Capabilities,
     Termicap.Color,
     Termicap.TTY,
     Termicap.Unicode;

use AnsiAda,
    Ada.Text_IO;

package body BBT.Status_Bar is

   use type Termicap.Unicode.Unicode_Level;

   package SU renames Ada.Strings.Unbounded;

   --  The bar has sober colours: no background, a cyan spinner, gray
   --  text, and a green check mark / red cross on the last scenario
   --  outcome. As soon as a scenario fails, the whole bar turns red,
   --  and displays the failure count until the end of the run: the
   --  next ticks cannot erase the failure from the user's view.
   --
   --  The palette is selected at Enable according to the colour level
   --  detected by Termicap: no colours at all on a NO_COLOR terminal
   --  (Termicap honours NO_COLOR in its detection), the 16 standard
   --  colours on a basic terminal, and fine RGB otherwise.

   RGB_Spinner   : constant String := Foreground (0,   175, 175);
   RGB_Text      : constant String := Foreground (135, 135, 135);
   RGB_OK        : constant String := Foreground (0,   180,   0);
   RGB_Fail      : constant String := Foreground (220,   0,   0);
   RGB_Fail_Text : constant String := Foreground (200,  60,  60);

   Spinner_Colour   : SU.Unbounded_String;
   Text_Colour      : SU.Unbounded_String;
   OK_Colour        : SU.Unbounded_String;
   Fail_Colour      : SU.Unbounded_String;
   Fail_Text_Colour : SU.Unbounded_String;
   --  Empty strings when colours are disabled: nothing is emitted

   Check_Mark : constant String := "✓";
   Cross_Mark : constant String := "✗";

   package Fields is new Ada.Strings.Bounded.Generic_Bounded_Length (60);

   --  The spinner is braille on a terminal able to render it, and
   --  deliberately ASCII otherwise, to stay readable on any terminal
   ASCII_Spinner : constant array (Natural range 0 .. 3) of Character
     := ['|', '/', '-', '\'];
   Braille_Spinner : constant array (Natural range 0 .. 9) of String (1 .. 3)
     := ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];

   Is_Enabled    : Boolean := False;
   Bar_Displayed : Boolean := False;
   --  True when the bar is displayed at the current line, and the line
   --  holds nothing else.

   Total_Scenarios : Natural := 0;
   Done_Scenarios  : Natural := 0;
   Failed_Scenarios : Natural := 0;
   Current_File    : Fields.Bounded_String;
   Activity        : Fields.Bounded_String;
   Spinner_Start   : Ada.Calendar.Time := Ada.Calendar.Clock;
   --  The spinner is time based: the glyph is a function of the time
   --  elapsed since the bar was enabled, so that the animation stays
   --  regular, whatever the duration of the steps. A glyph advanced
   --  at each step instead gave a jerky animation, the steps having
   --  very uneven durations.
   Unicode_Level   : Termicap.Unicode.Unicode_Level := Termicap.Unicode.None;
   UTF8_OK         : Boolean := False;
   Last_Outcome    : Scenario_Outcome := Skipped;

   --  -------------------------------------------------------------------------
   function Image (N : Natural) return String is
     (Ada.Strings.Fixed.Trim (N'Image, Ada.Strings.Right));

   --  -------------------------------------------------------------------------
   function Glyphs_Enabled return Boolean is
     (Unicode_Level /= Termicap.Unicode.None and then UTF8_OK);

   --  -------------------------------------------------------------------------
   function Spinner_Image return String is
      use type Ada.Calendar.Time;
      --  Twelve frames per second
      Frame : constant Natural :=
        Natural ((Ada.Calendar.Clock - Spinner_Start) * 12.0);
   begin
      if Unicode_Level = Termicap.Unicode.Extended and then UTF8_OK then
         return Braille_Spinner (Frame mod 10);
      else
         return [1 => ASCII_Spinner (Frame mod 4)];
      end if;
   end Spinner_Image;

   --  -------------------------------------------------------------------------
   --  What precedes the spinner:
   --  - the failure count as soon as a scenario has failed, the whole
   --    bar being red in that case, until the end of the run
   --  - or the check mark of the last scenario, when it ran OK
   --  - nothing when glyphs are disabled and nothing failed
   function Lead_Image return String is
     (if Failed_Scenarios > 0
      then SU.To_String (Fail_Colour)
        & (if Glyphs_Enabled then Cross_Mark & " " else "")
        & Image (Failed_Scenarios) & " failed"
        & SU.To_String (Fail_Text_Colour) & " "
      elsif Glyphs_Enabled and then Last_Outcome = OK
      then SU.To_String (OK_Colour) & Check_Mark
        & SU.To_String (Text_Colour) & " "
      else "");

   --  -------------------------------------------------------------------------
   procedure Enable (Force : Boolean) is
      Caps : constant Termicap.Capabilities.Terminal_Capabilities
        := Termicap.Capabilities.Get (Termicap.TTY.Stdout);
   begin
      if Caps.TTY_Stdout or Force then
         Is_Enabled    := True;
         Unicode_Level := Caps.Unicode;
         UTF8_OK       := Platform.UTF8_Output;
         Spinner_Start := Ada.Calendar.Clock;

         case Caps.Color is
            when Termicap.Color.None =>
               --  NO_COLOR, or a terminal without colours: a plain bar
               null;
            when Termicap.Color.Basic_16 =>
               Spinner_Colour   := SU.To_Unbounded_String (Foreground (Cyan));
               Text_Colour      := SU.To_Unbounded_String (Foreground (Grey));
               OK_Colour        := SU.To_Unbounded_String (Foreground (Green));
               Fail_Colour      := SU.To_Unbounded_String (Foreground (Red));
               Fail_Text_Colour := SU.To_Unbounded_String (Foreground (Red));
            when others =>
               Spinner_Colour   := SU.To_Unbounded_String (RGB_Spinner);
               Text_Colour      := SU.To_Unbounded_String (RGB_Text);
               OK_Colour        := SU.To_Unbounded_String (RGB_OK);
               Fail_Colour      := SU.To_Unbounded_String (RGB_Fail);
               Fail_Text_Colour := SU.To_Unbounded_String (RGB_Fail_Text);
         end case;
      end if;
   end Enable;

   --  -------------------------------------------------------------------------
   procedure Initialize_Progress_Bar (Max_Event : Natural) is
   begin
      Total_Scenarios := Max_Event;
      Done_Scenarios  := 0;
      Failed_Scenarios := 0;
   end Initialize_Progress_Bar;

   --  -------------------------------------------------------------------------
   procedure Set_Current_File (File_Name : String) is
      use Ada.Strings;
   begin
      Current_File := Fields.To_Bounded_String (Source => File_Name,
                                                Drop   => Right);
      Draw;
   end Set_Current_File;

   --  -------------------------------------------------------------------------
   procedure Next_Scenario (Outcome : Scenario_Outcome) is
   begin
      Done_Scenarios := @ + 1;
      if Outcome = Failed then
         Failed_Scenarios := @ + 1;
      end if;
      Last_Outcome := Outcome;
      Tick;
   end Next_Scenario;

   --  -------------------------------------------------------------------------
   procedure Tick is
   begin
      --  No index to advance: the spinner is time based, this is
      --  just a redraw, to keep the bar alive between steps
      Draw;
   end Tick;

   --  -------------------------------------------------------------------------
   procedure Put_Activity (S : String) is
      use Ada.Strings;
   begin
      Activity := Fields.To_Bounded_String (Source => S,
                                            Drop   => Right);
      Draw;
   end Put_Activity;

   --  -------------------------------------------------------------------------
   procedure Clear is
      use Ada.Characters.Latin_1;
   begin
      if Is_Enabled and then Bar_Displayed then
         Put (CR & Clear_Line);
         --  CR brings the cursor back to column 1, and the whole line,
         --  that holds only the bar, is erased
         Bar_Displayed := False;
      end if;
   end Clear;

   --  -------------------------------------------------------------------------
   procedure Draw is
      --  When scenarios have failed, the whole bar is red, so that the
      --  failure cannot be missed, whatever happens next
      Running_Colour : constant String :=
        (if Failed_Scenarios > 0
         then SU.To_String (Fail_Text_Colour)
         else SU.To_String (Text_Colour));
      Bar : constant String :=
        (if Total_Scenarios > 0
         then Lead_Image
            & SU.To_String (Spinner_Colour) & Spinner_Image
            & Running_Colour & " "
            & Image (Done_Scenarios) & "/" & Image (Total_Scenarios)
            & " " & Fields.To_String (Current_File)
         else SU.To_String (Spinner_Colour) & Spinner_Image
            & Running_Colour & " " & Fields.To_String (Activity));

   begin
      if not Is_Enabled then
         return;
      end if;

      if Bar_Displayed then
         --  The line holds only the bar: erase it before redrawing
         Clear;
      elsif Col > 1 then
         --  The current line is not complete: drawing the bar here would
         --  mix it with real output, that Clear would then erase from
         --  the terminal at the next output
         return;
      end if;

      Put (Bar);
      Put (Default_Foreground & Default_Background);
      Bar_Displayed := True;
   end Draw;

end BBT.Status_Bar;
