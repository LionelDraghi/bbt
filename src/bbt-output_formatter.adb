-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO;

package body BBT.Output_Formatter is

   -- --------------------------------------------------------------------------
   Formatter_List : array (Format) of Interface_Access;

   use BBT.IO;
   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := Output)
                             renames BBT.IO.Put_Line;
   pragma Warnings (Off, Put_Debug_Line);

   -- -------------------------------------------------------------------------
   function File_Extensions (For_Format : Format) return String is
     (File_Extensions (Formatter_List (For_Format).all));
   -- Dispatching call

   function Default_Extension (For_Format : Format) return String is
     (Default_Extension (Formatter_List (For_Format).all));
   -- Dispatching call

   -- --------------------------------------------------------------------------
   procedure Register (Formatter   : Interface_Access;
                       For_Format : Format) is
   begin
      Formatter_List (For_Format) := Formatter;
   end Register;

end BBT.Output_Formatter;
