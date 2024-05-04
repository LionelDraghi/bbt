with BBT.Documents; use BBT.Documents;

-- This package is in charge of building the tests list
-- by processing lines extracted from scenario files
package BBT.Tests_Builder is

   Missing_Scenario : exception;

   -- --------------------------------------------------------------------------
   procedure Add_Document   (Name : String);
   procedure Add_Feature    (Name : String);
   procedure Add_Scenario   (Name : String);
   procedure Add_Background (Name : String);
   procedure Add_Step       (Step : Step_Details);
   procedure Add_Line       (Line : String);
   procedure Add_Code_Block;

   -- --------------------------------------------------------------------------
   function The_Document_List return access Documents_Lists.Vector;

end BBT.Tests_Builder;
