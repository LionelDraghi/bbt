-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO,
     BBT.Settings,
     Text_Utilities;

use BBT.IO,
    Text_Utilities;

with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

private package BBT.Model is
-- Defines the main bbt internal data structure, wich is essentialy a tree
-- of Documents containing Features containing Scenario containing steps.
-- This structure is a a simplified subset of Gerkhin AST :
-- https://github.com/cucumber/gherkin?tab=readme-ov-file#abstract-syntax-tree-ast
--
-- The various type of step are also defined here, and this is bbt's own
-- vocabulary.

   function "+" (Name : Unbounded_String) return String is
     (To_String (Name));
   function "+" (Name : String) return Unbounded_String is
     (To_Unbounded_String (Name));

   function Filtered_By_Default return Boolean is (Settings.Selection_Mode);
   -- If in Selection mode, then items are filtered by default, and selected
   -- items should be provided with --select.
   -- If in normal mode, nothing is filtered by default, and filtered items
   -- should be provided with --exclude.

   -- --------------------------------------------------------------------------
   type Root_Node is abstract tagged record
      Filtered : Boolean          := Filtered_By_Default;
      Location : Location_Type    := No_Location;
      Comment  : Text             := Empty_Text;
      Name     : Unbounded_String := Null_Unbounded_String;
   end record;

   type Node_Access is not null access all Root_Node'Class;

   procedure Filter
     (N : in out Root_Node);
   procedure Unfilter
     (N : in out Root_Node);
   -- Mark the item as filtered
   procedure Apply_Filters_To
     (N : in out Root_Node) is abstract;
   function Has_Background
     (D : Root_Node) return Boolean is (False);

   -- --------------------------------------------------------------------------
   type Non_Root_Node is abstract new Root_Node with record
      Parent : aliased Node_Access; -- There is always a parent node
   end record;

   procedure Unfilter_Parents
     (N : in out Non_Root_Node'Class);

private
   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line
     (Item      : String;
      Location  : Location_Type    := No_Location;
      Verbosity : Verbosity_Levels := Debug;
      Topic     : Extended_Topics  := IO.Model)
      renames BBT.IO.Put_Line;

end BBT.Model;
