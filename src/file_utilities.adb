-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Directories.Hierarchical_File_Names,
     Ada.Strings,
     Ada.Strings.Fixed.Equal_Case_Insensitive,
     Ada.Strings.Maps;

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

      -- Dir and File are From_Dir and To_File without final Separator:
      Dir  : constant String := Remove_Final_Separator (From_Dir);
      File : constant String := Remove_Final_Separator (To_File);

      -- -----------------------------------------------------------------------
      function "=" (S1, S2 : String) return Boolean is
      begin
         if On_Windows then
            return Ada.Strings.Fixed.Equal_Case_Insensitive (S1, S2);
         else
            return Standard."=" (S1, S2);
         end if;
      end "=";

      -- -----------------------------------------------------------------------
      function "=" (C1, C2 : Character) return Boolean is
        ([1 => C1] = [1 => C2]);
        -- call the "=" equal function above that is case insensitive on Windows

   begin
      -- -----------------------------------------------------------------------
      if (Is_Root_Directory_Name (Dir) and then -- Is_Full_Name (File))
          Dir = File (File'First .. File'First + Dir'Length - 1))
        -- Dir = "/"  and File = "/usr/foo"
        --  (or "c:\" and "c:\foo" on Windows)
        or else Is_Current_Directory_Name (Dir)
        -- Dir = "."

      then
         -- On Windows, if on the same drive, returns File without the drive
         if (On_Windows and File'Length > 1 and Dir'Length > 1) and then
                                   (File (File'First .. File'First + 1) =
                                        Dir (Dir'First .. Dir'First + 1))
         then
            return File (File'First + 2 .. File'Last);
         else
            return File;
            -- This test is also the way to stop recursing until error
            -- when From_Dir and To_File have nothing in common.
         end if;
      end if;


      if Dir = File then return '.' & Separator; end if;
      -- Otherwise, the function returns the weird "../current_dir"

      if Dir (Dir'First .. Dir'First + 1) /= File (File'First .. File'First + 1)
        -- if Dir (Dir'First) /= File (File'First)
      then return File; end if;
      -- Optimization for a frequent case: there is no common path between
      -- Dir and File, so we return immediately File
      -- Need to be done on 2 chars, because on Windows dir may be "C:"
      -- and the file name start with "c".

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
