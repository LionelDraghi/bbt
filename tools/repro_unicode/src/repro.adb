with Ada.Text_IO;
with Termicap.Capabilities;
with Termicap.Unicode;
with Interfaces.C;

procedure Repro is
   use type Termicap.Unicode.Unicode_Level;
   Caps : constant Termicap.Capabilities.Terminal_Capabilities :=
     Termicap.Capabilities.Get;

   function GetConsoleOutputCP return Interfaces.C.unsigned;
   pragma Import (Stdcall, GetConsoleOutputCP, "GetConsoleOutputCP");

begin
   Ada.Text_IO.Put_Line ("TTY_Stdout    : " & Caps.TTY_Stdout'Image);
   Ada.Text_IO.Put_Line ("Unicode level : " & Caps.Unicode'Image);
   Ada.Text_IO.Put_Line
     ("Output CP     :"
      & Interfaces.C.unsigned'Image (GetConsoleOutputCP));
   Ada.Text_IO.Put ("   pass/fail glyph : ");
   if Caps.Unicode /= Termicap.Unicode.None then
      Ada.Text_IO.Put_Line ("[✓] OK");
   else
      Ada.Text_IO.Put_Line ("[X] OK  (ASCII fallback)");
   end if;
end Repro;
