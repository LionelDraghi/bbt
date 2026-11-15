-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

package body BBT_Backends.Text is

   Blanks : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set (" "
                                   & ASCII.HT  & ASCII.CR
                                   & ASCII.LF  & ASCII.VT
                                   & ASCII.FF);

   function Is_Blank (C : Character) return Boolean is
     (Ada.Strings.Maps.Is_In (C, Blanks));

   function Normalized (S : String) return String is
      B        : String (S'Range);
      L        : Natural := B'First - 1;
      At_Blank : Boolean := True;
      --  True outside the scanned text and after a blank, so that
      --  leading blanks are dropped and blank runs collapse to one
   begin
      for C of S loop
         if Is_Blank (C) then
            At_Blank := True;
         else
            if At_Blank and then L >= B'First then
               L := L + 1;
               B (L) := ' ';
            end if;
            L := L + 1;
            B (L) := C;
            At_Blank := False;
         end if;
      end loop;
      return B (B'First .. L);
   end Normalized;

   function Contains
     (Content, Expected : String;
      Ignore_Case       : Boolean := False) return Boolean
   is
      use Ada.Strings.Fixed;
      C : constant String := Normalized (Content);
      E : constant String := Normalized (Expected);
   begin
      if Ignore_Case then
         return Index (Ada.Characters.Handling.To_Lower (C),
                       Ada.Characters.Handling.To_Lower (E)) /= 0;
      else
         return Index (C, E) /= 0;
      end if;
   end Contains;

end BBT_Backends.Text;
