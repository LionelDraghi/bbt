-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Directories.Hierarchical_File_Names,
     Ada.Strings,
     Ada.Strings.Fixed,
     Ada.Strings.Maps;

use Ada.Directories.Hierarchical_File_Names;

package body File_Utilities is

   Parent_Dir : constant String := ".." & Separator;

   -- --------------------------------------------------------------------------
   function Short_Path (From_Dir : String;
                        To_File  : String;
                        Prefix   : String := "") return String
   is
      -- -----------------------------------------------------------------------
      function Remove_Final_Separator (From : String) return String is
        (if From (From'Last) = Separator and From'Length > 1
         then (From (From'First .. From'Last - 1))
         else From);

      -- -----------------------------------------------------------------------
      function Remove_Heading_Separator (From : String) return String is
        (if From (From'First) = Separator
         then (From (From'First + 1 .. From'Last))
         else From);

      -- Dir and File are From_Dir and To_File without final Separator:
      Dir  : constant String := Remove_Final_Separator (From_Dir);
      File : constant String := Remove_Final_Separator (To_File);

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


      if Dir = File then return "./"; end if;
      -- Otherwise, the function returns the weird "../current_dir"

      if Dir (Dir'First .. Dir'First + 1) /= File (File'First .. File'First + 1)
      then return File; end if;
      -- Optimization for a frequent case: there is no common path between
      -- Dir and File, so we return immediately File

      declare
         Length : constant Natural := Natural'Min (Dir'Length, File'Length);
         Right  : constant String  := File (File'First ..
                                              File'First + Length - 1);
      begin
         if Dir'Length <= File'Length and then Right = Dir then
            -- The left part of both string is identical
            -- e.g.:
            --    From_Dir = /home/lionel/Proj/smk/tests
            --    To_File  = /home/lionel/Proj/smk/tests/mysite/idx.txt
            return Prefix &
              Remove_Heading_Separator (File (Right'Last + 1 .. File'Last));

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
      use Ada.Strings.Maps;
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
                                   New_Item => "\",
                                   Drop     => Ada.Strings.Right);
         Src_Idx := Src_Idx + 2;
      end loop;
      return Out_Str;
   end Escape;

end File_Utilities;
