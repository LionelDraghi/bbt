separate (BBT.Tests.Builder)

-- -----------------------------------------------------------------------------
package body FSM is

   Internal_State          : States := In_Document;
   Internal_Previous_State : States;

   Internal_Step_State     : Step_States;

   Internal_Background     : Backgrounds := Doc;

   Internal_Doc_State      : Doc_States := In_Document;

   -- --------------------------------------------------------------------------
   function Current_State  return States is (Internal_State);

   procedure Restore_Previous_State is
   begin
      Internal_State := Internal_Previous_State;
   end Restore_Previous_State;

   procedure Set_State (To_State : States) is
   begin
      --  if To_State = Internal_State then
      --     -- If we are already in this state, ignore the call
      --     null;
      --
      --  else
      -- Stack new state
      Internal_Previous_State := Internal_State;
      Internal_State          := To_State;

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

      --  end if;

   end Set_State;

   -- --------------------------------------------------------------------------
   function Current_Doc_State  return Doc_States  is (Internal_Doc_State);
   function Current_Background return Backgrounds is (Internal_Background);
   function Current_Step_State return Step_States is (Internal_Step_State);

   -- --------------------------------------------------------------------------
   procedure Set_Step_State (To_State : Step_States) is
   begin
      Internal_Step_State := To_State;
   end Set_Step_State;

end FSM;
