-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

separate (BBT.Tests.Builder)

-- -----------------------------------------------------------------------------
package body FSM is

   Internal_State          : States      := In_Document;
   Internal_Previous_State : States;
   Internal_Step_State     : Step_States;
   Internal_Background     : Backgrounds := Doc;
   Internal_Doc_State      : Doc_States  := In_Document;
   Code_Block_Set          : Boolean     := False;
   -- We store the count instead of simply a Boolean, so that we can put a
   -- warning message only on the first  ``` in excess.

   -- --------------------------------------------------------------------------
   function Current_State  return States is (Internal_State);

   procedure Restore_Previous_State is
   begin
      Internal_State := Internal_Previous_State;
   end Restore_Previous_State;

   procedure Set_State (To_State : States) is
   begin
      -- Stack new state
      Internal_Previous_State := Internal_State;
      Internal_State          := To_State;

      Put_Line ("State : " & Internal_Previous_State'Image & " --> " &
                  Internal_State'Image,
                Verbosity => Debug);

      case To_State is
         when In_Step         => Code_Block_Set := False;
         when In_File_Content => Code_Block_Set := True;
         when others => null;
      end case;

      case To_State is
         when In_Document   |
              In_Feature    |
              In_Scenario   |
              In_Background =>
            -- Reset Background and Step related states
            Internal_Step_State := In_Given_Step;
         when others => null;
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

   end Set_State;

   -- --------------------------------------------------------------------------
   function Current_Doc_State  return Doc_States  is (Internal_Doc_State);
   function Current_Background return Backgrounds is (Internal_Background);
   function Current_Step_State return Step_States is (Internal_Step_State);

   -- --------------------------------------------------------------------------
   procedure Set_Step_State (To_State : Step_States) is
   begin
      Internal_Step_State := To_State;
      Set_State (In_Step);
   end Set_Step_State;

   -- --------------------------------------------------------------------------
   function Code_Block_Already_Set return Boolean is (Code_Block_Set);

end FSM;
