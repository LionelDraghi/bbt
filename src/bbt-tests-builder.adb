-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Settings;   use BBT.Settings;


with Text_Utilities; use Text_Utilities;

with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body BBT.Tests.Builder is

   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := IO.Builder)
                             renames BBT.IO.Put_Line;

   -- --------------------------------------------------------------------------
   type States is (In_Document,
                   In_Feature,
                   In_Scenario,
                   In_Background,
                   In_Step,
                   In_File_Content);

   type Step_States is (In_Given_Step,
                        In_When_Step,
                        In_Then_Step);

   type Backgrounds is (None, Doc, Feature);

   subtype Doc_States is States range In_Document .. In_Feature;

   -- --------------------------------------------------------------------------
   package FSM is

      function Current_State return States;
      procedure Set_State (To_State    : States;
                           CB_Expected : Boolean := False);
      procedure Restore_Previous_State;

      function Current_Doc_State return Doc_States;
      function Current_Background return Backgrounds;
      function Current_Step_State return Step_States;

      procedure Set_Step_State (To_State            : Step_States;
                                Code_Block_Expected : Boolean);

      function Code_Block_Already_Set return Boolean;
      -- Tell if a code block has already been provided in that step.
      -- Reset when calling Set_State (In_Step),
      -- that is when a new step starts.

      function Code_Block_Missing return Boolean;
      -- Tell if a code block is missing in a Step needing one.
      -- WARNING : this function has side effect, to avoid multiple
      -- error reporting, it will return True only once;

   end FSM;

   package body FSM is separate;

   Opening_Marker_Line : Ada.Text_IO.Count := 0;

   use FSM;

   -- --------------------------------------------------------------------------
   procedure Check_Missing_Code_Block (Loc : Location_Type) is
   begin
      if Code_Block_Missing then
         Put_Error ("missing expected Code Block", Loc);
      end if;
   end Check_Missing_Code_Block;

    -- --------------------------------------------------------------------------
   function Last_Scenario return Scenario_Maybe is
   begin
      case Current_Doc_State is
         when In_Document =>
            return Current_Doc.Last_Scenario_In_Doc;
         when In_Feature =>
            return Current_Doc.Last_Scenario_In_Feature;
      end case;
   end Last_Scenario;

   -- --------------------------------------------------------------------------
   function Last_Step return Step_Maybe is
   begin
      case Current_Background is
         when None =>
            return Last_Scenario.Last_Step;
         when Doc =>
            return Current_Doc.Background.Last_Step;
         when Feature =>
            return Current_Doc.Last_Feature.Background.Last_Step;
      end case;
   end Last_Step;

   -- --------------------------------------------------------------------------
   procedure Add_Document (Name : String) is
   begin
      Put_Debug_Line ("Add_Document " & Name'Image, Verbosity => IO.Debug);
      The_Tests_List.Append
        (Create_Document (Name     => To_Unbounded_String (Name),
                          Location => Location (Name, 0)));
      Set_State (In_Document);
   end Add_Document;

   -- --------------------------------------------------------------------------
   procedure Add_Feature (Name : String; Loc : Location_Type) is
   begin
      Put_Debug_Line ("Add_Feature " & Name'Image, Verbosity => IO.Debug);
      Current_Doc.Feature_List.Append
        (Create_Feature (Name       => To_Unbounded_String (Name),
                         Location   => Loc));
      Set_State (In_Feature);
      Check_Missing_Code_Block (Loc);
   end Add_Feature;

   -- --------------------------------------------------------------------------
   procedure Add_Scenario (Name : String; Loc : Location_Type) is
   begin
      case Current_Doc_State is
         when In_Document =>
            Put_Debug_Line ("Add_Scenario in Doc " & Name'Image,
                      Verbosity => IO.Debug);
            Current_Doc.Scenario_List.Append
              (Create_Scenario (Name     => Name,
                                Location => Loc));
         when In_Feature =>
            Put_Debug_Line ("Add_Scenario in Feature " & Name'Image,
                      Verbosity => IO.Debug);
            Last_Feature.Scenario_List.Append
              (Create_Scenario (Name           => Name,
                                Parent_Feature => Last_Feature,
                                Location       => Loc));
      end case;
      Set_State (In_Scenario);
      Check_Missing_Code_Block (Loc);
   end Add_Scenario;

   -- --------------------------------------------------------------------------
   procedure Add_Background (Name : String; Loc : Location_Type) is
      pragma Warnings (Off, "use of an anonymous access type allocator");
   begin
      case Current_State is
         when In_Document =>
            Put_Debug_Line ("Add_Background in Doc " & Name'Image,
                      Verbosity => IO.Debug);
            Current_Doc.Background := new Scenario_Type'
              (Create_Scenario (Name     => Name,
                                Location => Loc));

         when In_Feature =>
            Put_Debug_Line ("Add_Background in Feature " & Name'Image,
                      Verbosity => IO.Debug);
            Last_Feature.Background := new Scenario_Type'(
              Create_Scenario (Name           => Name,
                               Parent_Feature => Last_Feature,
                               Location       => Loc));

         when others =>
            IO.Put_Error ("Background should be declared at document"
                          & " or at Feature level, not " & Current_State'Image,
                          Loc);
      end case;

      Set_State (In_Background);
      Check_Missing_Code_Block (Loc);

   end Add_Background;

   -- --------------------------------------------------------------------------
   procedure Add_Step (Step                : in out Step_Type;
                       Code_Block_Expected :        Boolean;
                       Cmd_List            :        Cmd_Lists.Vector) is

      -- -----------------------------------------------------------------------
      procedure Append_Step (Scen : Scenario_Maybe) is
      begin
         Step.Set_Parent_Scenario (Scen);
         if Cmd_List.Is_Empty then
            -- normal case, there is no "or"
            Scen.Step_List.Append (Step);
            --  declare
            --     S2 : Step_Type := Step;
            --  begin
            --     S2.Parent_Scenario := Scen;
            --     Scen.Step_List.Append ((S2));
            --  end;

         else
            -- "or" case, we will duplicate the whole scenario, except the
            -- cmd

            -- the existing scenario will receive the first cmd
            Scen.Step_List.Append (Step);

            --  for S in 1 .. Cmd_List.Count - 1 loop
            --     Scen.Step_List
            --  end loop;
            --
            --  Scen.Cmd_List := Cmd_List;
            --  Scen.Cmd_List_Step_Index := Scen.Step_List.Last_Index;
            -- By definition, the Last_Index point to the current Step,
            -- the one we want to store as containing the command list.
            -- NB : there can't be two "or" per scenario, only one.
         end if;
      end Append_Step;

   begin
      --  Put_Debug_Line ("Add_Step " & Step'Image, Step.Location,
      --            Verbosity => IO.Debug);
      if Current_State = In_Document or Current_State = In_Feature then
         raise Missing_Scenario with "Prefix & Premature Step """ &
           To_String (Step.Step_String)
           & """ declaration, should be in Scenario or Background";
      end if;

      case Step.Cat is
         when Unknown =>
            if Step.Action = None then
               IO.Put_Error ("No context to determine step kind of '"
                             & To_String (Step.Step_String) & "'",
                             Step.Location);
            end if;

         when Given_Step =>
            if Settings.Strict_Gherkin then
               if Current_Step_State = In_When_Step then
                  IO.Put_Warning ("Given step """ & To_String (Step.Step_String) &
                                    """ appears after a ""When""",
                                  Step.Location);
               elsif Current_Step_State = In_Then_Step then
                  IO.Put_Warning ("Given step """ & To_String (Step.Step_String) &
                                    """ appears after a ""Then""",
                                  Step.Location);
               end if;
            end if;
            Set_Step_State (In_Given_Step, Code_Block_Expected);

         when When_Step  =>
            if Settings.Strict_Gherkin then
               if Current_Step_State = In_Then_Step then
                  IO.Put_Warning ("When step """
                                  & To_String (Step.Step_String)
                                  & """ appears after a ""Then""",
                                  Step.Location);
               elsif Current_Step_State = In_When_Step then
                  IO.Put_Warning ("Multiple When in the same Scenario """
                                  & To_String (Step.Step_String),
                                 Step.Location);
               end if;
            end if;
            Set_Step_State (In_When_Step, Code_Block_Expected);

         when Then_Step  =>
            Set_Step_State (In_Then_Step, Code_Block_Expected);

      end case;

      case Current_Background is
         when Doc     =>
            Put_Debug_Line ("Add_Step in Doc's Background : " & Step.Step_String'Image,
                      Verbosity => IO.Debug);
            Append_Step (Current_Doc.Background);
         when Feature =>
            Put_Debug_Line ("Add_Step in Feature's Background : " & Step.Step_String'Image,
                      Verbosity => IO.Debug);
            Append_Step (Last_Feature.Background);
         when None    =>
            Put_Debug_Line ("Add_Step in Scenario : " & Step.Step_String'Image,
                      Verbosity => IO.Debug);
            Set_Parent_Scenario (Scenario => Last_Scenario,
                                 Step     => Step);
            Last_Scenario.Add_Step (Step);
            --  Last_Scenario_Ref.Element.Step_List.Append
            --    ((Step with delta Parent_Scenario => Last_Scenario_Ref.Element));

      end case;
      Check_Missing_Code_Block (Step.Location);
   end Add_Step;

   -- --------------------------------------------------------------------------
   procedure Add_Code_Fence (Loc : Location_Type) is
   -- Code block outside of Steps definition is considered as a comment.
   -- There is no state change, the code block mark is just recorded in
   -- comments at the right level.
   -- At the end of the code block section, the previous Step state
   -- is restored.
   begin
      -- Put_Debug_Line ("Add_Code_Block, Current_State = ",
      --  Loc, Verbosity => IO.Debug);
      case Current_State is
         when In_Step =>
            Opening_Marker_Line := Line (Loc);
            if Code_Block_Already_Set then
               Put_Warning
                 ("File content already provided, ignoring this code fence",
                  Loc);
               Last_Step.Comment.Append ("```");
            else
               Set_State (In_File_Content);
            end if;

         when In_File_Content =>
            -- Exiting code block
            Opening_Marker_Line := 0;
            Restore_Previous_State;
            Put_Debug_Line ("Add_Code_Block, exiting code block. File_Content = "
                      & Last_Step.File_Content'Image,
                      Loc, Verbosity => IO.Debug);

         when In_Document =>
            Current_Doc.Comment.Append ("```");

         when In_Feature =>
            Last_Feature.Comment.Append ("```");

         when In_Scenario =>
            Last_Scenario.Comment.Append ("```");

         when In_Background =>
            case Current_Background is
               when None =>
                  Put_Error ("No Doc or Feature Background??");
               when Doc =>
                  Current_Doc.Comment.Append ("```");
               when Feature =>
                  Last_Feature.Comment.Append ("```");
            end case;

      end case;
   end Add_Code_Fence;

   -- --------------------------------------------------------------------------
   function In_File_Content return Boolean is (Current_State = In_File_Content);

   -- --------------------------------------------------------------------------
   procedure End_Of_Scenario (Loc : Location_Type) is
   begin
      Check_Missing_Code_Block (Loc);
      if Current_State = In_File_Content then
         Put_Error ("Code fenced block opened line"
                    & Opening_Marker_Line'Image &
                      ", but not not closed", Loc);
      end if;
   end End_Of_Scenario;

   -- --------------------------------------------------------------------------
   procedure Add_Line (Line : String) is
   begin
      case Current_State is
         when In_Document =>
            Current_Doc.Comment.Append (Line);

         when In_Feature =>
            Last_Feature.Comment.Append (Line);

         when In_Scenario =>
            Last_Scenario.Comment.Append (Line);

         when In_Step =>
            Last_Step.Comment.Append (Line);

         when In_Background =>
            case Current_Background is
               when None =>
                  Put_Error ("No Doc or Feature Background??");
               when Doc =>
                  Current_Doc.Comment.Append (Line);
               when Feature =>
                  Last_Feature.Comment.Append (Line);
            end case;

         when In_File_Content =>
            -- Not a comment, we are in a file content description
            Last_Step.File_Content.Append (Line);

      end case;
   end Add_Line;

   -- --------------------------------------------------------------------------
   procedure Duplicate_Multiple_Run is
      procedure Duplicate (Scen : in out Scenario_Type'Class) is
      begin
         if Has_Cmd_List (Scen) then
            -- Put_Debug_Line ("Has_Cmd_List = true =============");
            for Cmd of Scen.Cmd_List loop
               Put_Debug_Line
                 ("Cmd list item = " & Cmd'Image, Verbosity => IO.Verbose);
            end loop;
         --  else
         --     Put_Debug_Line ("Has_Cmd_List = false =============");
         end if;
      end Duplicate;

   begin
      --  Put_Debug_Line ("Duplicate_Multiple_Run =============");

      for D of The_Tests_List.all loop
         for S of D.Scenario_List loop
            Duplicate (S);
         end loop;

         for F of D.Feature_List loop
            for S of F.Scenario_List loop
               Duplicate (S);
            end loop;
         end loop;
      end loop;
   end Duplicate_Multiple_Run;

end BBT.Tests.Builder;
