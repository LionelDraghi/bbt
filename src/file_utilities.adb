-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Directories.Hierarchical_File_Names,
     Ada.Strings,
     Ada.Strings.Fixed,
     Ada.Strings.Maps,
     Ada.Text_IO;

use Ada.Directories.Hierarchical_File_Names,
    Ada.Strings.Maps;

package body File_Utilities is

   Parent_Dir : constant String := ".." & Separator;
   Separators : constant Character_Set :=
                  (if Separator = '/' then To_Set ("/") else To_Set ("/\"));
   -- On Windows, both character should be considered as separator

   -- --------------------------------------------------------------------------
   function Short_Path (From_Dir : String;
                        To_File  : String;
                        Prefix   : String := "") return String
   is
      -- -----------------------------------------------------------------------
      function Remove_Final_Separator (From : String) return String is
        (if Is_In (From (From'Last), Separators) and From'Length > 1
         then (From (From'First .. From'Last - 1))
         else From);
      -- "Separator or '/'" will remove '/' on Unix like systems,
      -- and '/' or '\' on Windows

      -- -----------------------------------------------------------------------
      function Remove_Heading_Separator (From : String) return String is
        (if Is_In (From (From'First), Separators)
         then (From (From'First + 1 .. From'Last))
         else From);

      -- -----------------------------------------------------------------------
      --  procedure Common_Part_Length (Path1, Path2 : String;
      --                                Common_Length : out Natural;
      --                                Directory_Count : out Natural) is
      --  -- Returns the length of the common left part of both path,
      --  -- but taking into account the directory semantics.
      --  -- For example : with
      --  --   Path1 = c:\Users\lio\
      --  --   Path2 = c:\users\lionel\
      --  -- will return first the index in Path1 of the second '\'
      --  --   Path1 = c:\Users\lionel
      --  --   Path2 = c:\users\lionel\Tmp
      --  -- will return last 'l' index.
      --  --
      --  -- Returns 0 if no common part found
      --     S1  : constant String (1 .. Path1'Length) := Path1;
      --     S2  : constant String (1 .. Path2'Length) := Path2;
      --     Last : constant Natural := Natural'Min (S1'Length, S2'Length);
      --     Idx : Natural := Last;
      --     -- The search starts backward at the end of the common part
      --     use Ada.Strings.Fixed;
      --     use Ada.Strings;
      --
      --     procedure Put_State is
      --     begin
      --        Ada.Text_IO.Put_Line ("Path1 =" & Path1);
      --        Ada.Text_IO.Put_Line ("Path2 =" & Path2);
      --        Ada.Text_IO.Put_Line ("Idx   =" & Idx'Image);
      --        Ada.Text_IO.Put_Line ("Dir count =" & Directory_Count'Image);
      --        Ada.Text_IO.Put_Line ("Path1 [ .. Idx] =" &
      --                                Path1 (Path1'First .. Idx));
      --     end Put_State;
      --
      --  begin
      --     Directory_Count := 0;
      --
      --     if Path1 = Path2 then
      --        -- Common case : files are rooted the same
      --        Common_Length   := Last;
      --        Directory_Count := Count (Source => S1, Set => Separators);
      --        Put_State;
      --        return;
      --     end if;
      --
      --     loop
      --        Idx := Index (Source    => S1,
      --                      Set       => Separators,
      --                      From      => Idx,
      --                      Going     => Backward);
      --        if Idx = 0 then
      --           -- no common directory part
      --           Common_Length := 0;
      --           Directory_Count := 0;
      --           Put_State;
      --           return;
      --
      --        elsif Idx = 1 then
      --           -- Could be Path1 = '/' and Path2 = "/usr/bin'
      --           Common_Length := 1;
      --           Directory_Count := 0;
      --           Put_State;
      --           return;
      --
      --        elsif S1 (1 .. Idx) = S2 (1 .. Idx) then
      --           -- the normal case Path1 = '/usr' and Path2 = "/usr/bin'
      --           Common_Length := Idx;
      --           Put_State;
      --           return;
      --
      --        else
      --           -- continue the search
      --           Directory_Count := @ + 1;
      --           Idx := @ - 1;
      --           -- (We did check before that Idx is not equal to 1)
      --           Put_State;
      --
      --        end if;
      --     end loop;
      --  end Common_Part_Length;

      -- Dir and File are From_Dir and To_File without final Separator:
      Dir  : constant String := Remove_Final_Separator (From_Dir);
      File : constant String := Remove_Final_Separator (To_File);

      use Ada.Strings.Fixed;

   begin
      -- -----------------------------------------------------------------------
      if (Is_Root_Directory_Name (Dir) and Is_Full_Name (File))
      -- From_Dir = "/" (or "c:\" on Windows) and File starts
      -- also with "/" or c:\"
          or else Is_Current_Directory_Name (Dir)
      then
         return File;
         -- This test is also the way to stop recursing until error
         -- when From_Dir and To_File have nothing in common.
      end if;


      if Dir = File then return '.' & Separator; end if;
      -- Otherwise, the function returns the weird "../current_dir"

      if Dir (Dir'First .. Dir'First + 1) /= File (File'First .. File'First + 1)
      then return File; end if;
      -- Optimization for a frequent case: there is no common path between
      -- Dir and File, so we return immediately File

      declare
         Length : constant Natural := Natural'Min (Dir'Length, File'Length);
         Right  : constant String  := File (File'First ..
                                              File'First + Length - 1);
         --  Common_Length, Dir_Count : Natural;

      begin
         --  Common_Part_Length (Dir, File,
         --                      Common_Length       => Common_Length,
         --                      Directory_Count     => Dir_Count);
         if Dir'Length <= File'Length and then Right = Dir then
         -- if Common_Length /= 0 then
            -- The left part of both string is identical
            -- e.g.:
            --    From_Dir = /home/lionel/Proj/smk/tests
            --    To_File  = /home/lionel/Proj/smk/tests/mysite/idx.txt
            return Prefix & -- Dir_Count * (Separator & Parent_Dir) &
              Remove_Heading_Separator (File (Right'Last + 1 .. File'Last));
              -- Remove_Heading_Separator
              -- (File (Right'First + Common_Length .. File'Last));

         else
            -- To_File'length <= From_Dir'length, e.g.:
            --    From_Dir = /home/tests/mysite/site/
            --    To_File  = /home/readme.txt
            -- or else From_Dir is not a To_File's parent, e.g.:
            --    From_Dir = /home/lionel/Proj/12/34
            --    To_File  = /home/lionel/Proj/mysite/site/idx.txt

            -- recursive call:
            return Short_Path (From_Dir => Containing_Directory (Dir),
                               To_File  => File,
                               Prefix   => Prefix & Parent_Dir);
         end if;
      end;

   end Short_Path;

   -- --------------------------------------------------------------------------
   function Escape (Text : String) return String is
      Src_Idx       : Natural := Text'First;
      To_Be_Escaped : constant Ada.Strings.Maps.Character_Set := To_Set (' '
                                                        & '"' & '#' & '$'
                                                        & '&' & ''' & '('
                                                        & ')' & '*' & ','
                                                        & ';' & '<' & '>'
                                                        & '?' & '[' & '\'
                                                        & ']' & '^' & '`'
                                                        & '{' & '|' & '}');
      Blank_Count   : constant Natural
        := Ada.Strings.Fixed.Count (Text, Set => To_Be_Escaped);
      Out_Str       : String (Text'First .. Text'Last + Blank_Count);
   begin
      Out_Str (Text'First .. Text'Last) := Text;

      for I in 1 .. Blank_Count loop
         Src_Idx := Ada.Strings.Fixed.Index (Out_Str (Src_Idx .. Out_Str'Last),
                                             To_Be_Escaped);
         Ada.Strings.Fixed.Insert (Out_Str,
                                   Before   => Src_Idx,
                                   New_Item => [1 => Separator],
                                   Drop     => Ada.Strings.Right);
         Src_Idx := Src_Idx + 2;
      end loop;
      return Out_Str;
   end Escape;

end File_Utilities;
