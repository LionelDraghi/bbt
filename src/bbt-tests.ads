with BBT.IO;      use BBT.IO;
with BBT.Documents; use BBT.Documents;

private package BBT.Tests is
-- This package (and child packages) is in charge of building the tests list
-- and running it.

   -- --------------------------------------------------------------------------
   --  function Postfix (Loc : IO.Location_Type) return String is
   --                      (" (" & Image (Loc) & ")  ");

   --  function Prefix (Success : Boolean;
   --                   Loc     : Location_Type) return String;

   -- --------------------------------------------------------------------------
   procedure Put_Step_Result (Step        : Step_Type;
                              Success     : Boolean;
                              -- Success_Msg : String;
                              Fail_Msg    : String;
                              Loc         : Location_Type;
                              Scenario    : in out Scenario_Type);
   -- Common procedure to normalize result output, indented like steps

end BBT.Tests;
