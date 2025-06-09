-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

separate (BBT.Tests.Builder)

-- -----------------------------------------------------------------------------
package body FSM is

   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := IO.FSM)
                             renames BBT.IO.Put_Line;
   pragma Warnings (Off, Put_Debug_Line);

   -- -----------------------------------------------------------------------
   Internal_State            : States      := In_Document;
   Internal_Previous_State   : States;
   Internal_Step_State       : Step_States;
   Internal_Background       : Backgrounds := Doc;
   Internal_Doc_State        : Doc_States  := In_Document;

   -- Code blocks management :
   CB_Missing                : Boolean     := False;
   Previous_Step_CB_Expected : Boolean     := False;

   -- --------------------------------------------------------------------------
   package body Code_Block_Marks is

      CB_Mark_Count : Natural := 0;

      -- --------------------------------------------------------------------------
      procedure Reset_Count is
      begin
         CB_Mark_Count := 0;
      end Reset_Count;

      -- --------------------------------------------------------------------------
      procedure Add_Mark is
      begin
         CB_Mark_Count := @ + 1;
         if CB_Mark_Count >= 2 then
            -- One code block has been loaded
            Previous_Step_CB_Expected := False;
         end if;
         Put_Debug_Line ("====== Add_Mark Expected = "
                         & Previous_Step_CB_Expected'Image
                         & ", Count = " & Count'Image);
      end Add_Mark;

      -- --------------------------------------------------------------------------
      function Count return Natural is (CB_Mark_Count);

      -- --------------------------------------------------------------------------
      function In_The_First_Code_Block return Boolean is
        (CB_Mark_Count = 1);

      function In_A_Code_Block return Boolean is
        (CB_Mark_Count mod 2 /= 0);
      -- When in a code block start, count is at 1, 3, 5, etc.

      function Code_Block_Already_Provided return Boolean is
         (CB_Mark_Count > 1);

   end Code_Block_Marks;



   -- --------------------------------------------------------------------------
   function Current_State return States is (Internal_State);

   -- --------------------------------------------------------------------------
   procedure Restore_Previous_State is
   begin
      Internal_State := Internal_Previous_State;
   end Restore_Previous_State;

   procedure Set_State (To_State    : States;
                        CB_Expected : Boolean := False;
                        Loc         : Location_Type) is
   begin
      -- Stack new state
      Internal_Previous_State := Internal_State;
      Internal_State          := To_State;

      Put_Debug_Line
        ("State : " & Internal_Previous_State'Image & " --> " &
           Internal_State'Image & "   CB_Expected : " & CB_Expected'Image
         & "   CB_Missing : " & CB_Missing'Image, Loc);

      -- When living the In_Step state, check that the expected code block
      -- was provided
      if Internal_Previous_State = In_Step
        and then To_State /= In_File_Content
        and then Previous_Step_CB_Expected
        and then not Code_Block_Marks.Code_Block_Already_Provided
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
         when In_File_Content => null;
         when others          => Code_Block_Marks.Reset_Count;
      end case;

      Previous_Step_CB_Expected := CB_Expected;

   end Set_State;

   -- --------------------------------------------------------------------------
   function Current_Doc_State  return Doc_States  is (Internal_Doc_State);
   function Current_Background return Backgrounds is (Internal_Background);
   function Current_Step_State return Step_States is (Internal_Step_State);

   -- --------------------------------------------------------------------------
   procedure Set_Step_State (To_State            : Step_States;
                             Code_Block_Expected : Boolean;
                             Loc                 : Location_Type) is
   begin
      Internal_Step_State := To_State;
      Set_State (In_Step, Code_Block_Expected, Loc);
   end Set_Step_State;

   -- --------------------------------------------------------------------------
   function Code_Block_Expected return Boolean is
     (Current_State = In_Step and Previous_Step_CB_Expected);

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
