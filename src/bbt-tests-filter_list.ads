-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package BBT.Tests.Filter_List is
-- This package manages the filters that will be applied to the tests.
-- Filters may apply to file name, feature name, scenario name, tag, etc.

   -- -------------------------------------------------------------------------
   type Filter_Scope is (Apply_To_All,
                         Doc_Name,
                         Feature,
                         Scenario,
                         Step);

   subtype Apply_On is Filter_Scope range
     Filter_Scope'Succ (Apply_To_All) .. Filter_Scope'Last;

   type Filter_Mode is (Include, Exclude);

   type Filter is private;

   -- Filter are stored and process in order they where submitted to Add_Filter.
   -- There is no coherency check of the list :
   -- - the same filter may be recorded twice, in that case only the first
   --   one will be taken into account,
   -- - there may be an include filter that contradict an exclude filter,
   -- - and a filter may be a subset of another.

   -- -------------------------------------------------------------------------
   procedure Add_Filter (Pattern : String;
                         Target  : Filter_Scope;
                         Mode    : Filter_Mode);

   type Filter_Result is (Filtered, Selected, No_Match);

   -- -------------------------------------------------------------------------
   function Is_Filtered (S         : String;
                         Item_Type : Apply_On) return Filter_Result;
   -- Filters chain is processed in reverse order. The first match is
   -- returned (Filtered or Selected), or No_Match otherwise.

   -- -------------------------------------------------------------------------
   function Short_Image (F : Filter) return String;

private
   -- type Filter_List is access all Filter_Lists.Vector;
   type Filter is record
      Pattern : Unbounded_String;
      Target  : Filter_Scope;
      Mode    : Filter_Mode;
   end record;

end BBT.Tests.Filter_List;
