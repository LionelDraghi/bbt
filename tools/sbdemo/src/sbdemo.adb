with AnsiAda,
     Ada.Calendar,
     Ada.Characters.Latin_1,
     Ada.Text_IO;

use AnsiAda,
    Ada.Text_IO;

procedure Sbdemo is

   use Ada.Characters.Latin_1;

   Braille : constant array (Natural range 0 .. 9) of String (1 .. 3)
     := ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];

   Counter   : constant String := "12/43";
   File_Name : constant String := "docs/features/B210_Status_Bar.md";

   --  Palettes
   Bar_FG        : constant String := Foreground (90,  50, 200);
   Bar_BG        : constant String := Background (140, 240, 250);
   Cyan          : constant String := Foreground (0, 175, 175);
   Gray           : constant String := Foreground (135, 135, 135);
   Green         : constant String := Foreground (0, 180,   0);
   Red           : constant String := Foreground (220,  0,   0);
   Soft_Red      : constant String := Foreground (200,  60,  60);
   Slate_BG      : constant String := Background (44,  48,  56);
   Light_Text    : constant String := Foreground (216, 218, 222);
   Fail_BG       : constant String := Background (124, 30,  30);
   --  AnsiAda.Reset (SGR 0) clears styles AND colours, background
   --  included; Style (Default) would only reset the foreground,
   --  and a style without background would keep the background
   --  of the previous one (the line erase fills with the
   --  current background!)

   function Spin (Frame : Natural) return String is
     (Braille (Frame mod 10));

   --  Animation d'une ligne pendant ~2 s
   procedure Play (Title  : String;
                   Render : access function (Frame : Natural) return String)
   is
      Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      use type Ada.Calendar.Time;
   begin
      Put (AnsiAda.Style (Bright) & Title & AnsiAda.Reset);
      New_Line;
      loop
         exit when Ada.Calendar.Clock - Start > 2.0;
         Put (CR & Clear_To_End_Of_Line);
         Put (Render
                (Natural ((Ada.Calendar.Clock - Start) * 12.0)));
         delay 0.083;
      end loop;
      --  Leave the last line displayed, with its style, so that the
      --  styles can be compared side by side after the run; the
      --  frame is fixed, and identical for all the panels
      Put (CR & Clear_To_End_Of_Line);
      Put (Render (7));
      New_Line;
      New_Line;
   end Play;

   --  Style 1 : actuel (violet sur cyan)
   function Current_Style (Frame : Natural) return String is
     (Bar_FG & Bar_BG & Style (Bright) & Spin (Frame)
      & " " & Counter & " " & File_Name & AnsiAda.Reset);

   --  Style 2 : cargo, recommande - pas de fond, texte atténue
   function Cargo_Style (Frame : Natural) return String is
     (Gray & Cyan & Spin (Frame)
      & Gray & " " & Counter & " " & File_Name & AnsiAda.Reset);

   --  Style 2 + dernier scenario OK
   function Cargo_OK (Frame : Natural) return String is
     (Green & "✓" & Gray & " " & Cyan & Spin (Frame)
      & Gray & " " & Counter & " " & File_Name & AnsiAda.Reset);

   --  Style 2 + echec persistant : la barre entiere passe en rouge
   --  atténue, la croix reste visible, avec le nombre d'echecs
   function Cargo_Failed (Frame : Natural) return String is
     (Red & "✗ 2" & Soft_Red & " " & Spin (Frame)
      & " " & Counter & " " & File_Name & AnsiAda.Reset);

   --  Style 3 : statusline a fond sombre
   function Statusline_Style (Frame : Natural) return String is
     (Slate_BG & Light_Text & Spin (Frame)
      & " " & Counter & " " & File_Name & AnsiAda.Reset);

   --  Style 3 + echec persistant : fond rouge sombre
   function Statusline_Failed (Frame : Natural) return String is
     (Fail_BG & Light_Text & "✗ 2 " & Spin (Frame)
      & " " & Counter & " " & File_Name & AnsiAda.Reset);

begin
   Put_Line ("Demo des styles de barre de statut bbt :"
             & " a lancer dans un vrai terminal.");
   New_Line;

   Play ("1. Style actuel : violet sur fond cyan", Current_Style'Access);
   Play ("2. Style cargo (recommande) : sans fond, texte attenue",
         Cargo_Style'Access);
   Play ("   2a. cargo, dernier scenario OK", Cargo_OK'Access);
   Play ("   2b. cargo, echec PERSISTANT (rouge, croix non effacee)",
         Cargo_Failed'Access);
   Play ("3. Style statusline : fond ardoise", Statusline_Style'Access);
   Play ("   3a. statusline, echec PERSISTANT (fond rouge)",
         Statusline_Failed'Access);

   Put_Line ("Fin de la demo.");
end Sbdemo;
