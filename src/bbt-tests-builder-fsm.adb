-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

separate (BBT.Tests.Builder)

-- -----------------------------------------------------------------------------
package body FSM is

   Internal_State            : States            := In_Document;
   Internal_Previous_State   : States;
   Internal_Step_State       : Step_States;
   Internal_Background       : Backgrounds       := Doc;
   Internal_Doc_State        : Doc_States        := In_Document;

   -- Code blocks management :
   CB_Set                    : Boolean           := False;
   CB_Missing                : Boolean           := False;
   Previous_Step_CB_Expected : Boolean           := False;

   -- --------------------------------------------------------------------------
   function Current_State  return States is (Internal_State);

   procedure Restore_Previous_State is
   begin
      Internal_State := Internal_Previous_State;
   end Restore_Previous_State;

   procedure Set_State (To_State    : States;
                        CB_Expected : Boolean := False) is
   begin
      -- Stack new state
      Internal_Previous_State := Internal_State;
      Internal_State          := To_State;

      Put_Line ("State : " & Internal_Previous_State'Image & " --> " &
                  Internal_State'Image & "   CB_Expected : " & CB_Expected'Image
                & "   CB_Missing : " & CB_Missing'Image,
                Verbosity => Debug);

      -- When living the In_Step state, check that the expected code block
      -- was provided
      if Internal_Previous_State = In_Step
        and then To_State /= In_File_Content
        and then Previous_Step_CB_Expected
        and then not CB_Set
      then
         CB_Missing := True;
      end if;

      case To_State is
         when In_Document     |
              In_Feature      |
              In_Scenario     |
              In_Background   => Internal_Step_State := In_Given_Step;
         when In_Step         |
              In_File_Content => null;
      end case;

      case To_State is
         when In_Document => Internal_Doc_State := In_Document;
         when In_Feature  => Internal_Doc_State := In_Feature;
         when others      => null;
      end case;

      case To_State is
         when In_Document => Internal_Background := Doc;
         when In_Feature  => Internal_Background := Feature;
         when In_Scenario => Internal_Background := None;
         when others      => null;
      end case;

      case To_State is
         when In_File_Content => CB_Set := True;
         when others          => CB_Set := False;
      end case;

      Previous_Step_CB_Expected := CB_Expected;

   end Set_State;

   -- --------------------------------------------------------------------------
   function Current_Doc_State  return Doc_States  is (Internal_Doc_State);
   function Current_Background return Backgrounds is (Internal_Background);
   function Current_Step_State return Step_States is (Internal_Step_State);

   -- --------------------------------------------------------------------------
   procedure Set_Step_State (To_State            : Step_States;
                             Code_Block_Expected : Boolean) is
   begin
      Internal_Step_State := To_State;
      Set_State (In_Step, Code_Block_Expected);
   end Set_Step_State;

   -- --------------------------------------------------------------------------
   function Code_Block_Already_Set return Boolean is (CB_Set);

   -- --------------------------------------------------------------------------
   function Code_Block_Missing return Boolean is
   begin
      if CB_Missing then
         CB_Missing := False;
         return True;
      else
         return False;
      end if;
   end Code_Block_Missing;

end FSM;
