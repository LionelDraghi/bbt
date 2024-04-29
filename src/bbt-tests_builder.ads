with BBT.Documents; use BBT.Documents;

package BBT.Tests_Builder is

   -- This package is in charge of building the tests list
   -- by processing lines extracted from scenario files

   procedure Add_Document (Name : String);
   procedure Add_Feature  (Name : String);
   procedure Add_Scenario (Name : String);
   --  procedure Add_Step     (Kind : Step_Kind;
   --                          Text : String);
   procedure Add_Step     (Step : Step_Details);
   --  procedure Add_Step (Step : Given_Step_Type);
   --  procedure Add_Step (Step : When_Step_Type);
   --  procedure Add_Step (Step : Then_Step_Type);
   procedure Add_Code_Block;
   -- procedure End_Document;
   -- A file start/stop line is ```
   -- All lines in between will be recorded thanks to Add_Line

   Missing_Scenario : exception;

   procedure Add_Line (Line : String);
   function The_Document_List return access Documents_Lists.Vector;

end BBT.Tests_Builder;
