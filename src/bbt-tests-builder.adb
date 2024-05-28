-- -----------------------------------------------------------------------------
-- bbt, the BlackBox tester (https://github.com/LionelDraghi/bbt)
-- © 2024 Lionel Draghi <lionel.draghi@free.fr>
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with BBT.IO;

with Text_Utilities; use Text_Utilities;

package body BBT.Tests.Builder is

   The_Doc_List : aliased Documents_Lists.Vector;

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
      procedure Set_State (To_State : States);
      procedure Restore_Previous_State;

      function Current_Doc_State return Doc_States;
      function Current_Background return Backgrounds;
      function Current_Step_State return Step_States;

      procedure Set_Step_State (To_State : Step_States);

   end FSM;

   package body FSM is separate;

   use FSM;

   -- --------------------------------------------------------------------------
   function Last_Doc_Ref return Documents_Lists.Reference_Type is
     (The_Doc_List.Reference (The_Doc_List.Last));

   function Last_Feature_Ref return Feature_Lists.Reference_Type is
     (Last_Doc_Ref.Feature_List.Reference
        (Last_Doc_Ref.Element.Feature_List.Last));

   function Last_Scenario_Ref return Scenario_Lists.Reference_Type is
     (if Current_Doc_State = In_Feature
      then Last_Feature_Ref.Element.Scenario_List.Reference
        (Last_Feature_Ref.Scenario_List.Last)
      else Last_Doc_Ref.    Element.Scenario_List.Reference
        (Last_Doc_Ref.    Scenario_List.Last));

   function Last_Step_Ref return Step_Lists.Reference_Type is
     (Last_Scenario_Ref.Element.Step_List.Reference
        (Last_Scenario_Ref.Step_List.Last));

   -- --------------------------------------------------------------------------
   procedure Add_Document (Name : String) is
   begin
      -- Put_Line ("Add_Document " & Name'Image, Verbosity => IO.Debug);
      The_Doc_List.Append
        (Document_Type'(Name   => To_Unbounded_String (Name),
                        others => <>));
      Set_State (In_Document);
   end Add_Document;

   -- --------------------------------------------------------------------------
   procedure Add_Feature (Name : String; Loc : Location_Type) is
   begin
      -- Put_Line ("Add_Feature " & Name'Image, Verbosity => IO.Debug);
      Last_Doc_Ref.Feature_List.Append
        (Feature_Type'(Name            => To_Unbounded_String (Name),
                       Parent_Document => Last_Doc_Ref.Element,
                       Location        => Loc,
                       others          => <>));
      Set_State (In_Feature);
   end Add_Feature;

   -- --------------------------------------------------------------------------
   procedure Add_Scenario (Name : String; Loc : Location_Type) is
   begin
      case Current_Doc_State is
         when In_Document =>
            Last_Doc_Ref.Scenario_List.Append
              (Scenario_Type'(Name            => To_Unbounded_String (Name),
                              Parent_Document => Last_Doc_Ref.Element,
                              Location        => Loc,
                              others          => <>));
         when In_Feature =>
            Last_Feature_Ref.Scenario_List.Append
              (Scenario_Type'(Name           => To_Unbounded_String (Name),
                              Parent_Feature => Last_Feature_Ref.Element,
                              Location       => Loc,
                              others         => <>));
      end case;
      Set_State (In_Scenario);
   end Add_Scenario;

   -- --------------------------------------------------------------------------
   procedure Add_Background (Name : String; Loc : Location_Type) is
   begin
      case Current_State is
         when In_Document =>
            Last_Doc_Ref.Background := new Scenario_Type'
              (Name            => To_Unbounded_String (Name),
               Parent_Document => Last_Doc_Ref.Element,
               others          => <>);

         when In_Feature =>
            Last_Feature_Ref.Background := new Scenario_Type'
              (Name           => To_Unbounded_String (Name),
               Parent_Feature => Last_Feature_Ref.Element,
               others         => <>);

         when others =>
            IO.Put_Error ("Background should be declared at document"
                          & "or at Feature level, not " & Current_State'Image,
                          Loc);
      end case;

      Set_State (In_Background);
   end Add_Background;

   -- --------------------------------------------------------------------------
   procedure Add_Step (Step     : Step_Type;
                       Cmd_List : Cmd_Lists.Vector) is

      -- -----------------------------------------------------------------------
      procedure Append_Step (Scen : not null access Scenario_Type) is
      begin
         -- Scen.Step_List.Append
         --  (Step_Type'(Step with delta Parent_Scenario => Scen));
         if Cmd_List.Is_Empty then
            -- normal case, there is no "or"
            Scen.Step_List.Append ((Step with delta Parent_Scenario => Scen));
            --  declare
            --     S2 : Step_Type := Step;
            --  begin
            --     S2.Parent_Scenario := Scen;
            --     Scen.Step_List.Append ((S2));
            --  end;

         else
            -- "or" case, we will duplicate the whome scenario, except the
            -- cmd

            -- the existing scenario will receive the first cmd
            Scen.Step_List.Append ((Step with delta
                                     Object_String => +Cmd_List.First_Element,
                                   Parent_Scenario => Scen));

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
      --  Put_Line ("Add_Step " & Step'Image, Step.Location,
      --            Verbosity => IO.Debug);
      if Current_State = In_Document or Current_State = In_Feature then
         raise Missing_Scenario with "Prefix & Premature Step """ &
           To_String (Step.Step_String)
           & """ declaration, should be in Scenarios or Background";
      end if;

      case Step.Cat is
         when Unknown =>
            if Step.Action = None then
               IO.Put_Error ("No context to determine step kind of "
                             & To_String (Step.Step_String),
                             Step.Location);
            end if;

         when Given_Step =>
            if Current_Step_State = In_When_Step then
               IO.Put_Warning ("Given step """ & To_String (Step.Step_String) &
                                 """ appears to late, after a ""When""",
                               Step.Location);
            elsif Current_Step_State = In_Then_Step then
               IO.Put_Warning ("Given step """ & To_String (Step.Step_String) &
                                 """ appears to late, after a ""Then""",
                               Step.Location);
            end if;
            Set_Step_State (In_Given_Step);

         when When_Step  =>
            if Current_Step_State = In_Then_Step then
               IO.Put_Warning ("When step """ & To_String (Step.Step_String) &
                                 """ appears to late, after a ""Then""",
                               Step.Location);
            end if;
            Set_Step_State (In_When_Step);

         when Then_Step  =>
            Set_Step_State (In_Then_Step);

      end case;

      case Current_Background is
         when Doc     =>
            Append_Step (Last_Doc_Ref.Background);
            Put_Line ("Add_Step to Last_Doc_Ref.Background",
                      Step.Location,
                      Verbosity => IO.Debug);
         when Feature =>
            Append_Step (Last_Feature_Ref.Background);
            Put_Line ("Add_Step to Last_Feature_Ref.Background",
                      Step.Location,
                      Verbosity => IO.Debug);
         when None    =>
            Last_Scenario_Ref.Element.Step_List.Append
              ((Step with delta Parent_Scenario => Last_Scenario_Ref.Element));
            Put_Line ("Add_Step to Last_Scenario_Ref" &
                        Scenario_Type'(Last_Scenario_Ref)'Image,
                      Step.Location,
                      Verbosity => IO.Debug);
      end case;
   end Add_Step;

   -- --------------------------------------------------------------------------
   procedure Add_Code_Block (Loc : Location_Type) is
   -- Code block outside of Steps definition is considered as a comment.
   -- There is no state change, the code block mark is just recorded in
   -- comments at the right level.
   -- At the end of the code block section, the previous Step state
   -- is restored.
   begin
      -- Put_Line ("Add_Code_Block, Current_State = ", Loc, Verbosity => IO.Debug);
      case Current_State is
         when In_Step | In_Scenario | In_Background =>
            -- Entering code block
            -- note that In_Scenario is included, so that if a comment is
            -- inserted between the step and the code fenced file content,
            -- it will work.
            Set_State (In_File_Content);

         when In_File_Content =>
            -- Exiting code block
            Restore_Previous_State;
            Put_Line ("Add_Code_Block, exiting code block. File_Content = "
                      & Last_Step_Ref.File_Content'Image,
                      Loc, Verbosity => IO.Debug);

         when In_Document =>
            Last_Doc_Ref.Comment.Append ("```");

         when In_Feature =>
            Last_Feature_Ref.Comment.Append ("```");

      end case;
   end Add_Code_Block;

   -- --------------------------------------------------------------------------
   procedure Add_Line (Line : String; Loc : Location_Type) is
   begin
      -- Put_Line ("Add_Line " & Line'Image, Loc, Verbosity => IO.Debug);
      case Current_State is
         when In_Document =>
            Last_Doc_Ref.Comment.Append (Line);

         when In_Feature =>
            Last_Feature_Ref.Comment.Append (Line);

         when In_Scenario | In_Step | In_Background =>
            Last_Scenario_Ref.Comment.Append (Line);
            -- There is no comment attached to Step

         when In_File_Content =>
            -- this is not a comment, we are in a file content description
            Put_Line ("File content = """ & Line & """", Loc,
                      Verbosity => IO.Debug);
            Last_Step_Ref.File_Content.Append (Line);

      end case;
   end Add_Line;

   -- --------------------------------------------------------------------------
   procedure Duplicate_Multiple_Run is
      procedure Duplicate (Scen : in out Scenario_Type) is
      begin
         if Has_Cmd_List (Scen) then
            for Cmd of Scen.Cmd_List loop
               Put_Line ("Cmd list item = " & Cmd'Image,
                         Verbosity => IO.Verbose);
            end loop;
         end if;
      end Duplicate;

   begin
      for D of The_Doc_List loop
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

   -- --------------------------------------------------------------------------
   function The_Tests_List return access Documents_Lists.Vector is
     (The_Doc_List'Access);

end BBT.Tests.Builder;
