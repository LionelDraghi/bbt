-- -----------------------------------------------------------------------------
-- bbt, the BlackBox tester (https://github.com/LionelDraghi/bbt)
-- © 2024 Lionel Draghi <lionel.draghi@free.fr>
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------

package body BBT.Tests is

   Pref : constant array (Boolean) of String (1 .. 9) :=
            [True  => "    OK  :",
             False => "*** NOK :"];

   -- --------------------------------------------------------------------------
   procedure Put_Step_Result (Step     : Step_Type;
                              Success  : Boolean;
                              Fail_Msg : String;
                              Loc      : Location_Type) is
      Pre  : constant String := Pref (Success) & " ";
   begin
      --  Put_Line ("Put_Step_Result = " & Step'Image);
      --  Put_Line ("Step.Parent     = " & Step.Parent_Scenario'Image);
      Add_Result (Success, Step.Parent_Scenario.all);
      if Success then
         IO.Put_Line (Item      => Pre & (+Step.Step_String),
                      Verbosity => IO.Verbose);
      else
         IO.Put_Line (Item      => Pre & (+Step.Step_String) & "  ",
                      Verbosity => IO.Normal);
         IO.Put_Line (Image (Loc) & Fail_Msg & "  ",
                      Verbosity => IO.Normal);
      end if;
   end Put_Step_Result;

end BBT.Tests;
