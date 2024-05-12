
package body BBT.Tests is

   Pref : constant array (Boolean) of String (1 .. 9) :=
            [True  => "    OK  :",
             False => "*** NOK :"];

   -- --------------------------------------------------------------------------
   procedure Put_Step_Result (Step        : Step_Type;
                              Success     :        Boolean;
                              -- Success_Msg :        String;
                              Fail_Msg    :        String;
                              Loc         :        Location_Type;
                              Scenario    : in out Scenario_Type) is
      Pre  : constant String := Pref (Success) & " ";
   begin
      Add_Result (Success, Scenario);
      if Success then
         IO.Put_Line (Item      => Pre & (+Step.Step_String),
                      Verbosity => IO.Verbose);
      else
         IO.Put_Line (Item      => Pre & (+Step.Step_String) & "  ",
                      Verbosity => IO.Normal);
         -- IO.New_Line (Verbosity => IO.Quiet);
         IO.Put_Line (Image (Loc) & Fail_Msg & "  ",
                      Verbosity => IO.Normal);
         -- IO.New_Line (Verbosity => IO.Quiet);
      end if;
   end Put_Step_Result;

   --  function Prefix (Success     : Boolean;
   --                   Loc         : Location_Type) return String is
   --  begin
   --     return Pref (Success) & Image (Loc);
   --  end Prefix;

end BBT.Tests;
