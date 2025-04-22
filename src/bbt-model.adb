-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

package body BBT.Model is

   -- --------------------------------------------------------------------------
   procedure Filter (N : in out Root_Node) is
   begin
      Put_Debug_Line ("Filter '" & (+N.Name) & "'");
      N.Filtered := True;
   end Filter;
   -- --------------------------------------------------------------------------
   procedure Unfilter (N : in out Root_Node) is
   begin
      Put_Debug_Line ("Unfilter '" & (+N.Name) & "'");
      N.Filtered := False;
   end Unfilter;


   -- --------------------------------------------------------------------------
   procedure Unfilter_Parents (N : in out Non_Root_Node'Class) is
   begin
      Put_Debug_Line ("Unfilter_Parents '" & (+N.Name) & "'");
      if N in Non_Root_Node'Class then
         Put_Debug_Line ("Unfilter '" & (+N.Parent.Name) & "'");
         Unfilter (N.Parent.all);
      end if;

      if N.Parent.all in Non_Root_Node'Class then
         -- We goes up recursively until top level
         Unfilter_Parents (Non_Root_Node'Class (N.Parent.all));
      end if;
   end Unfilter_Parents;

end BBT.Model;
