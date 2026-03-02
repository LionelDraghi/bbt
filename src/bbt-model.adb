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
   procedure Set_Start_Time (N : in out Root_Node) is
   begin
      Put_Debug_Line ("Set_Start_Time for '" & (+N.Name) & "'");
      N.Start_Time := Ada.Calendar.Clock;
   end Set_Start_Time;

   -- --------------------------------------------------------------------------
   procedure Set_End_Time (N : in out Root_Node) is
   begin
      Put_Debug_Line ("Set_End_Time for '" & (+N.Name) & "'");
      N.End_Time := Ada.Calendar.Clock;
   end Set_End_Time;

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

   -- --------------------------------------------------------------------------
   -- Calculate the elapsed time (duration) between Start_Time and End_Time
   -- --------------------------------------------------------------------------
   function Elapsed_Time (N : Root_Node'Class) return Duration is
   begin
      return N.End_Time - N.Start_Time;
   end Elapsed_Time;

end BBT.Model;
