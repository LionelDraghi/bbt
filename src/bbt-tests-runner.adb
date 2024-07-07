-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------

with BBT.Created_File_List; use BBT.Created_File_List;
with BBT.Documents;         use BBT.Documents;
with BBT.IO;
with BBT.Tests.Builder;
with BBT.Tests.Actions;     use BBT.Tests.Actions;
with BBT.Settings;
with Text_Utilities;        use Text_Utilities;

with GNAT.Traceback.Symbolic;

with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body BBT.Tests.Runner is

  -- -----------------------------------------------------------------------
   function Subject_Or_Object_String (Step : Step_Type) return String is
     (To_String (if Step.Object_String = Null_Unbounded_String then
           Step.Subject_String else Step.Object_String));

   Return_Code : Integer := 0;

   -- -----------------------------------------------------------------------
   procedure Run_Scenario (Scen : in out Scenario_Type) is
      Spawn_OK : Boolean := False;
      The_Doc  : constant Document_Type := Parent_Doc (Scen).all;
   begin
      Step_Processing : for Step of Scen.Step_List loop
         Spawn_OK := False;
         begin
            case Step.Action is
            when Run_Cmd =>
               Run_Cmd (Step         => Step,
                        Cmd          => To_String (Step.Object_String),
                        Output_Name  => Output_File_Name (The_Doc),
                        Successfully => False,
                        Spawn_OK     => Spawn_OK,
                        Return_Code  => Return_Code);
               exit Step_Processing when not Spawn_OK;

            when Run_Without_Error =>
               Run_Cmd (Step         => Step,
                        Cmd          => To_String (Step.Object_String),
                        Output_Name  => Output_File_Name (The_Doc),
                        Successfully => True,
                        Spawn_OK     => Spawn_OK,
                        Return_Code  => Return_Code);

               exit Step_Processing when not (Spawn_OK and
                                                Is_Success (Return_Code));

            when Error_Return_Code =>
               Return_Error (Return_Code, Step);

            when No_Error_Return_Code =>
               Return_No_Error (Return_Code, Step);

            when Output_Is =>
               Output_Is (Get_Text (Output_File_Name (The_Doc)), Step);

            when Output_Contains =>
               -- example : Then the output contains `--version`
               Output_Contains (Get_Text (Output_File_Name (The_Doc)), Step);

            when File_Is =>
               Files_Is (Step);

            when File_Contains =>
               File_Contains (Step);

            when Check_No_File =>
               Check_No_File (Subject_Or_Object_String (Step), Step);

            when Check_No_Dir =>
               Check_No_Dir (Subject_Or_Object_String (Step), Step);

            when Check_File_Existence =>
               Check_File_Existence (Subject_Or_Object_String (Step), Step);

            when Check_Dir_Existence =>
               Check_Dir_Existence (Subject_Or_Object_String (Step), Step);

            when Create_File =>
               Create_New (Step);

            when Create_Directory =>
               Create_New (Step);

            when Erase_And_Create =>
               Erase_And_Create (Step);

            when Setup_No_File =>
               Setup_No_File (Step);

            when Setup_No_Dir =>
               Setup_No_Dir (Step);

            when None =>
               Add_Result (False, Scen);
               IO.Put_Error ("Unrecognized step " & Step.Step_String'Image,
                             Step.Location);

            end case;

         exception
            when E : others =>
               Add_Result (False, Scen);
               Put_Exception ("while processing scenario "
                              & Scen.Name'Image
                              & " : " & Ada.Exceptions.Exception_Message (E)
                              & GNAT.Traceback.Symbolic.Symbolic_Traceback (E),
                              Step.Location);
         end;

      end loop Step_Processing;

   end Run_Scenario;

   -- --------------------------------------------------------------------------
   procedure Run_Doc_Background (Scen : in out Scenario_Type) is
   -- Run background scenario at document level, if any
      Doc : constant Document_Type := Parent_Doc (Scen).all;
   begin
      if Has_Background (Doc) then
         Put_Line ("  Document Background "
                   & (+Doc.Background.Name)
                   & "  ", IO.No_Location, IO.Verbose);
         Run_Scenario (Doc.Background.all);
         Move_Results (From_Scen => Doc.Background.all,
                       To_Scen   => Scen);
      end if;
   end Run_Doc_Background;

   -- --------------------------------------------------------------------------
   procedure Run_Feature_Background (Scen : in out Scenario_Type) is
   -- Run background scenario at feature level, if any
   begin
      if Is_In_Feature (Scen) and Has_Background (Scen.Parent_Feature.all)
      then
         declare
            Feat : constant Feature_Type := Scen.Parent_Feature.all;
         begin
            Put_Line ("  Feature background " & (+Feat.Background.Name) &
                        "  ", IO.No_Location, IO.Verbose);
            Run_Scenario (Feat.Background.all);
            Move_Results (From_Scen => Feat.Background.all, To_Scen => Scen);
         end;
      end if;
   end Run_Feature_Background;

   -- --------------------------------------------------------------------------
   procedure Run_Scenario_List (L : in out Scenario_Lists.Vector) is
      function Link_Image (Scen : Scenario_Type) return String is
        ("[" & (+Scen.Name) & "](" & (+Parent_Doc (Scen).Name) & ")");

   begin
      for Scen of L loop
         IO.New_Line (Verbosity => IO.Normal);
         --  Put_Line ("  Scenario " & Scen.Name'Image & "  ",
         --            IO.No_Location, IO.Verbose);

         -- Run background scenarios
         Run_Doc_Background (Scen);
         if Is_In_Feature (Scen) then
            Run_Feature_Background (Scen);
         end if;

         -- And finally run the scenario
         Run_Scenario (Scen);

         case Documents.Result (Scen) is
            when Empty =>
               -- Note the two spaces at the end of each line, to cause a
               -- new line in Markdown format when this line is followed
               -- by an error message.
               Put_Line ("  - [ ] scenario " & Link_Image (Scen) &
                           " is empty, nothing tested  ");
            when Successful =>
               Put_Line ("  - [X] scenario " & Link_Image (Scen) & " pass  ");
            when Failed =>
               Put_Line ("  - [ ] scenario " & Link_Image (Scen) & " fails  ");
         end case;
      end loop;
   end Run_Scenario_List;

   -- --------------------------------------------------------------------------
   procedure Run_All is
   begin
      -- let's run the test
      for D of BBT.Tests.Builder.The_Tests_List.all loop
         IO.New_Line (Verbosity => IO.Normal);
         Put_Line ("Running file " & D.Name'Image & "  ");

         if D.Scenario_List.Is_Empty and then D.Feature_List.Is_Empty
         then
            Put_Warning ("No scenario in document " & D.Name'Image & "  ",
                         D.Location);
         end if;

         -- Run scenarios directly attached to the document (not in a Feature)
         Run_Scenario_List (D.Scenario_List);

         for F of D.Feature_List loop
            -- Then run scenarios attached to each Feature
            IO.Put_Line ("Running feature " & F.Name'Image & "  ",
                         Verbosity => Verbose);

            if F.Scenario_List.Is_Empty then
               Put_Warning ("No scenario in feature " & F.Name'Image & "  ",
                            F.Location);
            else
               Run_Scenario_List (F.Scenario_List);
            end if;

         end loop;

         if Settings.Cleanup then
            Created_File_List.Delete_All;
         end if;

      end loop;

   end Run_All;

end BBT.Tests.Runner;
