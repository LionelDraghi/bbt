-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
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
         IO.Pause_Tee; --  We don't want this level of detail in the
                       --  generated test index.
         IO.Put_Line (Item      => Pre & (+Step.Step_String),
                      Verbosity => IO.Verbose);
         IO.Restore_Tee;
      else
         IO.Put_Line (Pre & (+Step.Step_String) & " (" & Image (Loc) & ")  ",
                      Verbosity => IO.Normal); --Verbose);
         IO.Put_Error (Fail_Msg & "  ", Loc);
      end if;
   end Put_Step_Result;

end BBT.Tests;
