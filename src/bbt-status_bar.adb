with AnsiAda,
     Ada.Strings.Fixed,
     Ada.Text_IO;

use AnsiAda,
    Ada.Text_IO;

package body BBT.Status_Bar is

   Foreground_Colour : constant String := Foreground (90,   50, 200);
   Background_Colour : constant String := Background (140, 240, 250);

   Status_Line : String (1 .. 70);
   --  This is the only line that is printed in that package
   --  Fields are part of that line.
   --  When a field is updated, the whole line is printed.

   Activity_First_Row : constant Positive := 1;
   Activity_Last_Row  : constant Positive := 30;

   File_Name_First_Row : constant Positive := 34;
   File_Name_Last_Row  : constant Positive := 70;

   --  File_Name_Field : String (1 .. 30);
   --  Activity_Column : constant Positive := 2;
   --
   --  File_Name_Field  : String (1 .. 40);
   --  File_Name_Column : constant Positive := Activity_Column +
   --                      Activity_Field'Length + 2;
   -- Max_Event_Count  : Positive := 1;
   -- Event_Count      : Natural  := 0;

   Is_Enabled : Boolean := False;

   --  -------------------------------------------------------------------------
   procedure Enable is
   begin
      Is_Enabled := True;
   end Enable;

   procedure Disable is
   begin
      Is_Enabled := False;
   end Disable;

   --  -------------------------------------------------------------------------
   procedure Initialize_Progress_Bar (Max_Event : Positive) is
   begin
      null; -- Max_Event_Count := Max_Event;
   end Initialize_Progress_Bar;

   --  -------------------------------------------------------------------------
   --  function String_Part (Current_Idx : Natural) return Natural is
   --    (Current_Idx * (File_Name_Last_Row - File_Name_First_Row + 1) /
   --         Max_Event_Count);
   --  Return Idx of the Current/Max percent of the string

   --  -------------------------------------------------------------------------
   procedure Progress_Bar_Next_Step (File_Name : String) is
      use Ada.Strings;
      -- Progression_Idx : Positive;
   begin
      -- Event_Count := @ + 1;

      if Is_Enabled then
         -- Progression_Idx := String_Part (Event_Count);
         Ada.Strings.Fixed.Move ("   " & File_Name,
                                 Target  => Status_Line (File_Name_First_Row ..
                                   File_Name_Last_Row),
                                 Drop    => Right,
                                 Justify => Left,
                                 Pad     => Space);
         Put (Store);

         Put (Foreground_Colour & Background_Colour);
         Put (Position (1, 1) & Style_Wrap (Text       => Status_Line,
                                            Style      => Bright));
         --  Put (Style_Wrap (Text       => File_Name_Field
         --                   (Progression_Idx + 1 .. File_Name_Field'Last),
         --                   Style      => Invert));

         Put (Restore);
      end if;

   end Progress_Bar_Next_Step;

   --  -------------------------------------------------------------------------
   --  procedure Put_Results (Results : Documents.Test_Results_Count) is
   --  begin
   --     null;
   --  end Put_Results;

   --  -------------------------------------------------------------------------
   procedure Put_Activity (S : String) is
      use Ada.Strings;
   begin
      if Is_Enabled then
         Ada.Strings.Fixed.Move ("   " & S,
                                 Target  => Status_Line
                                   (Activity_First_Row .. Activity_Last_Row),
                                 Drop    => Right,
                                 Justify => Left,
                                 Pad     => Space);
         Put (Store);
         Put (Foreground_Colour & Background_Colour);
         Put (Position (1, 1) & Style_Wrap (Text       => Status_Line,
                                            Style      => Bright));
         Put (Restore);
      end if;
   end Put_Activity;

end BBT.Status_Bar;
