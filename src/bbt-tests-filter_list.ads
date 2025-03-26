with BBT.Documents;

use BBT.Documents;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package BBT.Tests.Filter_List is
-- This package manages the filters that will be applied to the tests.
-- Filters may apply to file name, feature name, scenario name, tag, etc.

   type Apply_On is (Apply_To_All,
                     File_Name,
                     Feature,
                     Scenario,
                     Step);

   subtype Filtered_Item is Apply_On range
     Apply_On'Succ (Apply_To_All) .. Apply_On'Last;

   type Filter_Mode is (Include, Exclude);

   type Filter is private;
   procedure Add_Filter (S : String;
                         A : Apply_On;
                         M : Filter_Mode);

   procedure Apply_Filters_To (Docs : access Documents_Lists.Vector);
   -- Apply all added filters on the provided document list

private
   -- type Filter_List is access all Filter_Lists.Vector;
   type Filter is record
      S : Unbounded_String;
      A : Apply_On;
      M : Filter_Mode;
   end record;

end BBT.Tests.Filter_List;
