-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Created_File_List; use BBT.Created_File_List;
with BBT.Documents;         use BBT.Documents;
with BBT.IO;
with BBT.Settings;
with BBT.Status_Bar;
with BBT.Tests.Builder;
with BBT.Tests.Actions;     use BBT.Tests.Actions;
with File_Utilities;
with Text_Utilities;        use Text_Utilities;

with Ada.Calendar.Formatting;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- with GNAT.Compiler_Version;
with GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;

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
               Created_File_List.Add (Output_File_Name (The_Doc));
               Run_Cmd (Step         => Step,
                        Cmd          => To_String (Step.Object_String),
                        Output_Name  => Output_File_Name (The_Doc),
                        Successfully => False,
                        Spawn_OK     => Spawn_OK,
                        Return_Code  => Return_Code);
               exit Step_Processing when not Spawn_OK;

            when Run_Without_Error =>
               Created_File_List.Add (Output_File_Name (The_Doc));
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

            when No_Output =>
               Check_No_Output (Get_Text (Output_File_Name (The_Doc)), Step);

            when Output_Contains =>
               -- example : Then the output contains `--version`
               Output_Contains (Get_Text (Output_File_Name (The_Doc)), Step);

            when Output_Does_Not_Contain =>
               Output_Does_Not_Contain (Get_Text (Output_File_Name (The_Doc)), Step);

            when File_Is =>
               Files_Is (Step);

            when File_Contains =>
               File_Contains (Step);

            when File_Does_Not_Contain =>
               File_Does_Not_Contain (Step);

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

         --  if IO.Some_Error and not Settings.Keep_Going then
         --     exit Step_Processing;
         --  end if;
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
   procedure Run_Scenario_List (L            : in out Scenario_Lists.Vector;
                                Path_To_Scen :        String) is

   begin
      for Scen of L loop
         declare
            Link_Image : constant String
              := ("[" & (+Scen.Name) & "](" & Path_To_Scen & ")");

         begin
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
                  Put_Line ("  - [ ] scenario " & Link_Image &
                              " is empty, nothing tested  ",
                            Verbosity => Normal);
                  IO.New_Line (Verbosity => Normal);
               when Successful =>
                  Put_Line ("  - [X] scenario " & Link_Image & " pass  ",
                            Verbosity => Verbose);
                  IO.New_Line (Verbosity => Verbose);
               when Failed =>
                  Put_Line ("  - [ ] scenario " & Link_Image & " fails  ",
                            Verbosity => Quiet);
                  IO.New_Line (Verbosity => Quiet);
            end case;
         end;

         --  if IO.Some_Error and not Settings.Keep_Going then
         --     exit;
         --  end if;

      end loop;
   end Run_Scenario_List;

   -- --------------------------------------------------------------------------
   procedure Run_All is
      use File_Utilities;
      use Status_Bar;
      File_Count : Natural := Natural (BBT.Tests.Builder.The_Tests_List.Length);
      -- package CVer is new GNAT.Compiler_Version;

   begin
      -- First, let's move to a different exec dir, if any
      Ada.Directories.Set_Directory (Settings.Exec_Dir);

      Status_Bar.Initialize_Progress_Bar (File_Count);

      Put_Line ("Time: " & Ada.Calendar.Formatting.Image
                (Date                  => Ada.Calendar.Clock,
                 Include_Time_Fraction => True));
      -- Put_Line ("GNAT version: " & CVer.Version);

      -- let's run the test
      for D of BBT.Tests.Builder.The_Tests_List.all loop
         BBT.Created_File_List.Open
           (Ada.Directories.Simple_Name (To_String (D.Name))
            & ".created_files");

         IO.New_Line (Verbosity => IO.Normal);
         begin
            declare
               Path_To_Scen  : constant String
                 := Short_Path (From_Dir => Settings.Result_Dir,
                                To_File  => (+D.Name));
            begin
               --  IO.Put_Line ("From_Dir   => " & Settings.Result_Dir);
               --  IO.Put_Line ("To_File    => " & (+D.Name));
               --  IO.Put_Line ("Short_Path => " & Path_To_Scen);
               Put_Line ("## [" & Ada.Directories.Simple_Name (Path_To_Scen)
                         & "](" & (Path_To_Scen) & ")  ",
                         Verbosity => Normal);
               IO.New_Line (Verbosity => Verbose);

               Status_Bar.Progress_Bar_Next_Step (Path_To_Scen); delay (0.1);

               if D.Scenario_List.Is_Empty and then D.Feature_List.Is_Empty
               then
                  Put_Warning ("No scenario in document " & D.Name'Image & "  ",
                               D.Location);
               end if;

               -- Run scenarios directly attached to the document (not in a Feature)
               Run_Scenario_List (D.Scenario_List, Path_To_Scen);

               for F of D.Feature_List loop
                  -- Then run scenarios attached to each Feature
                  IO.Put_Line ("  ### Feature: " & (+F.Name) & "  ",
                               Verbosity => Verbose);
                  IO.New_Line (Verbosity => Verbose);

                  if F.Scenario_List.Is_Empty then
                     Put_Warning ("No scenario in feature " & F.Name'Image & "  ",
                                  F.Location);
                  else
                     Run_Scenario_List (F.Scenario_List, Path_To_Scen);

                     --  if IO.Some_Error and not Settings.Keep_Going then
                     --     exit;
                     --  end if;
                  end if;

                  --  if IO.Some_Error and not Settings.Keep_Going then
                  --     exit;
                  --  end if;

               end loop;

            end;
         exception
            when E : others =>
               Put_Exception (Ada.Exceptions.Exception_Message (E)
                              & GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
               IO.Put_Line ("From_Dir   => " & Settings.Result_Dir);
               IO.Put_Line ("To_File    => " & (+D.Name));
               -- IO.Put_Line ("Short_Path => " & Path_To_Scen);
         end;
         Created_File_List.Delete_All;

      end loop;

   end Run_All;

end BBT.Tests.Runner;
