-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Settings,
     BBT.Model.Steps,
     BBT.Model.Scenarios,
     BBT.Model.Features,
     BBT.Model.Documents;

use BBT.Model.Steps,
    BBT.Model.Scenarios,
    BBT.Model.Features,
    BBT.Settings,
    BBT.Model.Documents;

with Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

package body BBT.Tests.Builder is

   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := IO.Builder)
                             renames BBT.IO.Put_Line;

   -- --------------------------------------------------------------------------
   type States is (Not_In_Document,
                   In_Document,
                   In_Feature,
                   In_Scenario,
                   In_Background,
                   In_Step,
                   In_File_Content) with default_Value => Not_In_Document;

   subtype Doc_States is States range In_Document .. In_Feature;

   type Step_States is (In_Given_Step,
                        In_When_Step,
                        In_Then_Step);

   type Backgrounds is (None, Doc, Feature);

   -- --------------------------------------------------------------------------
   package FSM is

      function Current_State return States;
      procedure Set_State (To_State    : States;
                           CB_Expected : Boolean := False;
                           Loc         : Location_Type);
      procedure Restore_Previous_State;
      -- Restore_State_Before_In_Step;
      -- When exiting the File content state, it restore the previous one

      function Current_Doc_State return Doc_States;
      function Current_Background return Backgrounds;
      function Current_Step_State return Step_States;

      procedure Set_Step_State (To_State            : Step_States;
                                Code_Block_Expected : Boolean;
                                Loc                 : Location_Type);

      function Code_Block_Expected return Boolean;
      -- Tell if we are processing a step and that if that step is
      -- expecting a code block.

      function Code_Block_Missing return Boolean;
      -- Tell if a code block is missing in a Step needing one.
      -- WARNING : this function has side effect, to avoid multiple
      -- error reporting, it will return True only once;

      -- -----------------------------------------------------------------------
      package Code_Block_Marks is

         procedure Reset_Count;
         procedure Add_Mark;
         -- Incremented Count
         -- Count = 0, 2, 4, etc. => not in Code_Block
         -- Count = 1, 3, 5, etc. =>     in Code_Block

         -- --------------------------------------------------------------------
         function Count return Natural;

         function In_The_First_Code_Block return Boolean;
         function In_A_Code_Block return Boolean;
         function Code_Block_Already_Provided return Boolean;

      end Code_Block_Marks;

   end FSM;

   package body FSM is separate;

   Code_BLock_Expected_Line,
   Opening_Marker_Line : Ada.Text_IO.Count := 0;

   use FSM;

   -- --------------------------------------------------------------------------
   procedure Check_Missing_Code_Block (Loc : Location_Type) is
   begin
      if Code_Block_Missing then
         Put_Error ("Missing Code Block expected line"
                    & Code_BLock_Expected_Line'Image, Loc);
      elsif Current_State = In_File_Content then
         Put_Error ("Code fenced block opened line"
                    & Opening_Marker_Line'Image &
                      ", but not closed", Loc);
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
      Put_Debug_Line ("Add_Document " & Name'Image, Location (Name, 1));
      Doc_List.Append
        (Create_Document (Name     => To_Unbounded_String (Name),
                          Location => Location (Name, 0)));
      Set_State (In_Document, Loc => Location (Name, 0));
   end Add_Document;

   -- --------------------------------------------------------------------------
   procedure Add_Feature (Name : String; Loc : Location_Type) is
   begin
      Put_Debug_Line ("Add_Feature " & Name'Image, Loc);
      Current_Doc.Feature_List.Append
        (Create_Feature (Name       => To_Unbounded_String (Name),
                         Parent     => Current_Doc,
                         Location   => Loc));
      Set_State (In_Feature, Loc => Loc);
      Check_Missing_Code_Block (Loc);
   end Add_Feature;

   -- --------------------------------------------------------------------------
   procedure Add_Scenario (Name : String; Loc : Location_Type) is
   begin
      case Current_Doc_State is
         when In_Document =>
            Put_Debug_Line ("Add_Scenario " & Name'Image & " in doc "
                            & Current_Doc.Name'Image, Loc);
            Current_Doc.Scenario_List.Append
              (Create_Scenario (Name     => Name,
                                Parent   => Node_Access (Current_Doc),
                                Location => Loc));
         when In_Feature =>
            Put_Debug_Line ("Add_Scenario " & Name'Image & " in feature"
                            & Last_Feature.Name'Image, Loc);
            Last_Feature.Scenario_List.Append
              (Create_Scenario (Name     => Name,
                                Parent   => Node_Access (Last_Feature),
                                Location => Loc));
      end case;
      Set_State (In_Scenario, Loc => Loc);
      Check_Missing_Code_Block (Loc);
   end Add_Scenario;

   -- --------------------------------------------------------------------------
   procedure Add_Background (Name : String; Loc : Location_Type) is
      pragma Warnings (Off, "use of an anonymous access type allocator");
   begin
      case Current_State is
         when In_Document =>
            Put_Debug_Line ("Add_Background in Doc " & Name'Image,
                            Loc);
            Current_Doc.Background := new Scenario_Type'
              (Create_Scenario (Name          => Name,
                                Parent        => Node_Access (Current_Doc),
                                Location      => Loc,
                                Is_Background => True));

         when In_Feature =>
            Put_Debug_Line ("Add_Background in Feature " & Name'Image,
                            Loc);
            Last_Feature.Background := new Scenario_Type'
              (Create_Scenario (Name          => Name,
                                Parent        => Node_Access (Last_Feature),
                                Location      => Loc,
                                Is_Background => True));

         when others =>
            IO.Put_Error ("Background should be declared at document"
                          & " or at Feature level, not " & Current_State'Image,
                          Loc);
      end case;

      Set_State (In_Background, Loc => Loc);
      Check_Missing_Code_Block (Loc);

   end Add_Background;

   -- --------------------------------------------------------------------------
   procedure Add_Step (Step_Info           : in out Model.Steps.Step_Data;
                       Code_Block_Expected : Boolean;
                       Loc                 : Location_Type;
                       Syntax_Error        : Boolean := False)
   is
      -- -----------------------------------------------------------------------
      procedure Append_Step (Scen : Scenario_Access) is
         Step : Step_Type := Create_Step (Step_Info, Loc, Scen);
      begin
         Step.Set_Has_Syntax_Error (Syntax_Error);
         Scen.Add_Step (Step);
         if not Step_Info.Commands.Is_Empty then
            if Scen.Cmd_List.Is_Empty then
               Scen.Cmd_List := Step_Info.Commands;
               -- We store the index of the Step that contain several commands
               Scen.Cmd_List_Step_Index := Last_Step_Index (Scen.all);
            else
               Put_Error ("Only one Step with ' cmd1 or cmd2' allowed per Scenario");
               Put_Error ("Step number " & Scen.Cmd_List_Step_Index'Image & " has it already");
            end if;
         end if;
      end Append_Step;

   begin
      if Code_Block_Expected then
         Code_BLock_Expected_Line := Line (Loc);
      end if;

      if Current_State in Not_In_Document | In_Document | In_Feature then
         raise Missing_Scenario with "Prefix & Premature Step " &
           Step_Info.Src_Code'Image
           & " declaration, should be in Scenario or Background";
      end if;

      case Step_Info.Cat is
         when Unknown =>
            if Step_Info.Action = None then
               IO.Put_Error ("No context to determine step kind of "
                             & Step_Info.Src_Code'Image, Loc);
            end if;

         when Given_Step =>
            if Settings.Strict_Gherkin then
               if Current_Step_State = In_When_Step then
                  IO.Put_Warning
                    ("Given step " & Step_Info.Src_Code'Image &
                       " appears after a ""When""", Loc);
               elsif Current_Step_State = In_Then_Step then
                  IO.Put_Warning
                    ("Given step " & Step_Info.Src_Code'Image &
                       " appears after a ""Then""", Loc);
               end if;
            end if;
            Set_Step_State (In_Given_Step, Code_Block_Expected, Loc);

         when When_Step  =>
            if Settings.Strict_Gherkin then
               if Current_Step_State = In_Then_Step then
                  IO.Put_Warning ("When step "
                                  & Step_Info.Src_Code'Image
                                  & " appears after a ""Then""", Loc);
               elsif Current_Step_State = In_When_Step then
                  IO.Put_Warning ("Multiple When in the same Scenario "
                                  & Step_Info.Src_Code'Image, Loc);
               end if;
            end if;
            Set_Step_State (In_When_Step, Code_Block_Expected, Loc);

         when Then_Step  =>
            Set_Step_State (In_Then_Step, Code_Block_Expected, Loc);

      end case;

      case Current_Background is
         when Doc     =>
            Put_Debug_Line
              ("Add_Step in Doc's Background : " & Step_Info.Src_Code'Image,
               Loc);
               Append_Step (Current_Doc.Background);

         when Feature =>
            Put_Debug_Line
              ("Add_Step in Feature's Background : " & Step_Info.Src_Code'Image,
               Loc);
               Append_Step (Last_Feature.Background);

         when None    =>
            if Last_Scenario = null then
               Put_Error
                 ("Adding step to doc, but there is no scenario yet", Loc);
            else
               Put_Debug_Line
                 ("Add_Step " & Step_Info.Src_Code'Image & " in scenario "
                  & Last_Scenario.Name'Image, Loc);
               Append_Step (Scenario_Access (Last_Scenario));
            end if;

      end case;
      Check_Missing_Code_Block (Loc);
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
      --                 Loc, Verbosity => IO.Debug);
      Code_Block_Marks.Add_Mark;

      case Current_State is
         when In_Step =>
            Opening_Marker_Line := Line (Loc);
            if FSM.Code_Block_Marks.Count = 1 and Code_Block_Expected then
               Set_State (In_File_Content, Loc => Loc);

            else
               Last_Step.Comment.Append ("```");

            end if;

         when In_File_Content =>
            -- Exiting code block
            Opening_Marker_Line := 0;
            Restore_Previous_State;
            Put_Debug_Line
              ("Add_Code_Fence, exiting code block. File_Content = "
               & Last_Step.Data.File_Content'Image, Loc);

         when In_Document =>
            Current_Doc.Comment.Append ("```");

         when In_Feature =>
            Last_Feature.Comment.Append ("```");

         when In_Scenario =>
            Last_Scenario.Comment.Append ("```");

         when In_Background =>
            case Current_Background is
               when None =>
                  Put_Error ("No Doc or Feature Background???");
               when Doc =>
                  Current_Doc.Comment.Append ("```");
               when Feature =>
                  Last_Feature.Comment.Append ("```");
            end case;

         when Not_In_Document =>
            Put_Error ("Code fence outside of document???", Loc);

      end case;
   end Add_Code_Fence;

   -- --------------------------------------------------------------------------
   procedure Close_Document (Loc : Location_Type) is
   begin
      Check_Missing_Code_Block (Loc);
      Set_State (Not_In_Document, Loc => No_Location);
   end Close_Document;

   -- --------------------------------------------------------------------------
   procedure Add_Line (Line : String;
                       Loc  : Location_Type) is
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
            Last_Step.Data.File_Content.Append (Line);

        when Not_In_Document =>
            Put_Error ("Add Line outside of document???", Loc);

      end case;
   end Add_Line;

   -- --------------------------------------------------------------------------
   procedure Duplicate_Multiple_Run is

      -- -----------------------------------------------------------------------
      procedure Duplicate (L : in out Scenarios.List) is
         -- --------------------------------------------------------------------
         function Copy_Scenario
           (Source   : in Scenario_Type'Class;
            With_Cmd : in String) return Scenario_Type'Class
         is
         -- copy A to B neutralizing the Step_List, replaced with a single Cmd
            Target : Scenario_Type'Class := Source;
            Step : Step_Data renames
              Target.Step_List (Target.Cmd_List_Step_Index).Data;
         begin
            Target.Cmd_List    := Empty_Cmd_List;
            Step.Commands      := Model.Steps.Empty_Cmd_List;
            Step.Object_String := +With_Cmd;
            Replace_Slice (Source => Step.Src_Code,
                           Low    => Step.Code_Span_First,
                           High   => Step.Code_Span_Last,
                           By     => With_Cmd);
            return Target;
         end Copy_Scenario;

         -- --------------------------------------------------------------------
         procedure Prefix_Name (S : in out Scenario_Type'Class;
                                A : Positive;
                                B : Ada.Containers.Count_Type) is
         -- Prefix the scenario name with " A/B "
            use Ada.Strings;
         begin
            S.Name := @ & A'Image & "/" & Trim (Source => +B'Image,
                                                Side   => Left);
         end Prefix_Name;

         use Scenario_Lists;
         Scen_Cursor, To_Be_Split : Cursor := L.First;

      begin
         while Has_Element (Scen_Cursor) loop
            declare
               Scen : constant Scenario_Type'Class := Element (Scen_Cursor);
               Cmd_Idx : Positive := 1;

            begin
               if not Scen.Cmd_List.Is_Empty then
                  -- This scenario has an "cmd1 or cmd2"
                  -- It will be copied/modified for each cmd,
                  -- and deleted at the end:
                  --   Scen A with "cmd1 or cmd2"
                  --     |
                  --     V
                  --   Scen A with "cmd1 or cmd2"
                  --   Scen A 1/2 with "cmd1"
                  --   Scen A 2/2 with "cmd2"
                  --     |
                  --     V
                  --   Scen A 1/2 with "cmd1"
                  --   Scen A 2/2 with "cmd2"

                  To_Be_Split := Scen_Cursor;
                  for Cmd of Scen.Cmd_List loop
                     declare
                        Scen_B : Scenario_Type'Class := Copy_Scenario
                          (Source   => Scen,
                           With_Cmd => Cmd);
                     begin
                        Prefix_Name (Scen_B, Cmd_Idx, Scen.Cmd_List.Length);
                        L.Append (New_Item => Scen_B);

                        Cmd_Idx := @ + 1;
                        Scen_Cursor := Next (Scen_Cursor);
                     end;
                  end loop;
                  L.Delete (To_Be_Split);
               end if;
            end;
            Next (Scen_Cursor);
         end loop;
      end Duplicate;

   begin
      for D of Doc_List.all loop
         Duplicate (D.Scenario_List);

         for F of D.Feature_List loop
            Duplicate (F.Scenario_List);
         end loop;
      end loop;
   end Duplicate_Multiple_Run;

end BBT.Tests.Builder;
