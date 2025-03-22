-- with BBT.Documents;

-- use BBT,
    -- BBT.Documents;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package BBT.Tests.Filter_List is
-- This package manages the filters that will be applied to the tests.
-- Filters may apply to file name, feature name, scenario name, tag, etc.

   type Global_Mode is (Selection, Exclusion)
     with Default_Value => Exclusion;

   procedure Set_Global_Mode (M : Global_Mode);
   -- Default Global Mode is Include, meaning that the initial situation
   -- is that nothing is filtered. But if the user wants to select specific
   -- items, then this should be called with Mode => Exclude, and then
   -- some filter should be added with Mode => Include

   type Filters is (Apply_To_All, File_Name, Feature_Name, Scenario_Name, Tag);
   type Filter_Mode is (Include, Exclude);

   type Filter is private;
   procedure Add_Filter (S : String;
                         A : Filters;
                         M : Filter_Mode);

   -- function Filtered_Document_List return access Documents_Lists.Vector;

private
   -- type Filter_List is access all Filter_Lists.Vector;
   type Filter is record
      S : Unbounded_String;
      A : Filters;
      M : Filter_Mode;
   end record;

end BBT.Tests.Filter_List;
