
package BBT.Tests.Builder is
-- This package is in charge of building the tests list
-- by processing lines extracted from Scenarios files.

   Missing_Scenario : exception;

   -- --------------------------------------------------------------------------
   procedure Add_Document   (Name : String);
   procedure Add_Feature    (Name : String;    Loc : Location_Type);
   procedure Add_Scenario   (Name : String;    Loc : Location_Type);
   procedure Add_Background (Name : String;    Loc : Location_Type);
   procedure Add_Step       (Step : Step_Type);
   procedure Add_Line       (Line : String;    Loc : Location_Type);
   procedure Add_Code_Block (Loc : Location_Type);

   -- --------------------------------------------------------------------------
   function The_Tests_List return access Documents_Lists.Vector;

end BBT.Tests.Builder;
