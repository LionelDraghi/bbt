separate (BBT.Tests_Builder)

-- --------------------------------------------------------------------------
package body FSM is

   Internal_State          : States := In_Document;
   Internal_Previous_State : States;

   Internal_Step_State     : Step_States;

   Internal_Background     : Backgrounds := Doc;

   -- --------------------------------------------------------------------------
   function Current_State  return States is (Internal_State);

   procedure Restore_Previous_State is
   begin
      Internal_State := Internal_Previous_State;
   end Restore_Previous_State;

   procedure Set_State (To_State : States) is
   begin
      Internal_Previous_State := Internal_State;
      Internal_State := To_State;

      case Internal_State is
         when In_Document   |
              In_Feature    |
              In_Scenario   |
              In_Background =>
            Internal_Step_State := In_Given_Step;
            Internal_Background := Doc;
         when others => null;
      end case;

   end Set_State;

   -- --------------------------------------------------------------------------
   function Current_Step_State return Step_States is (Internal_Step_State);

   procedure Set_Step_State (To_State : Step_States) is
   begin
      Internal_Step_State := To_State;
   end Set_Step_State;

   -- --------------------------------------------------------------------------
   procedure Set_Background (The_Background : Backgrounds) is
   begin
      Internal_Background := The_Background;
   end Set_Background;

   function Current_Background return Backgrounds is (Internal_Background);

end FSM;
