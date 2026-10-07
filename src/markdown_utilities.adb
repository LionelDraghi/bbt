-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Strings.Maps;

package body Markdown_Utilities is

   -- --------------------------------------------------------------------------
   function Web_Path (Path : String) return String is
      Web_Mapping : constant Ada.Strings.Maps.Character_Mapping :=
        Ada.Strings.Maps.To_Mapping (From => "\", To => "/");
   begin
      return Ada.Strings.Fixed.Translate (Path, Web_Mapping);
   end Web_Path;

   -- --------------------------------------------------------------------------
   function Link (Name : String;
                 Path : String) return String is
     ("[" & Name & "](" & Path & ")");

   -- --------------------------------------------------------------------------
   function Checkbox (Done : Boolean) return String is
     (if Done then "[X]" else "[ ]");

end Markdown_Utilities;
