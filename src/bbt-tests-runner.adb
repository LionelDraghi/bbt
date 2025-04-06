-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Created_File_List; use BBT.Created_File_List;
with BBT.Documents;         use BBT.Documents;
with BBT.IO;                use BBT.IO;
with BBT.Settings;
with BBT.Status_Bar;
with BBT.Tests.Builder;
with BBT.Tests.Actions;     use BBT.Tests.Actions;
with BBT.Writers;           use BBT.Writers;
with File_Utilities;
with Text_Utilities;        use Text_Utilities;

with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Traceback.Symbolic;

package body BBT.Tests.Runner is

   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := IO.Runner)
                             renames BBT.IO.Put_Line;
   pragma Warnings (Off, Put_Debug_Line);

   -- --------------------------------------------------------------------------
   function Subject_Or_Object_String (Step : Step_Type) return String is
     (To_String (if Step.Object_File_Name = Null_Unbounded_String then
           Step.Subject_String else Step.Object_File_Name));
   -- Fixme: Clearly not confortable with that function, it's magic.


   -- --------------------------------------------------------------------------
   procedure Run_Step (Step      : in out Step_Type;
                       Run_Error :    out Boolean);
   procedure Run_Scenario (Scen : in out Scenario_Type);
   -- run only the scenario, not Doc and / or Feature Background.
   procedure Run_Background (Scen : in out Scenario_Type);
   -- run only Doc and / or Feature Background, not the scenario himself.
   procedure Run_Doc_Background (Scen : in out Scenario_Type);
   procedure Run_Feature_Background (Scen : in out Scenario_Type);
   procedure Run_Scenario_List (L : in out Scenario_Lists.Vector);

   -- --------------------------------------------------------------------------
   procedure Run_Step (Step      : in out Step_Type;
                       Run_Error :    out Boolean)
   is
      Spawn_OK    : Boolean;
      Return_Code : Integer;
      Output      : constant String :=
                      Output_File_Name (Enclosing_Doc (Step).all);
   begin
      Run_Error := False;

      if Step.Filtered then
         Put_Debug_Line
           ("  ====== Skipping filtered step " & Step.Step_String'Image);
         return;
      end if;

      Put_Debug_Line ("  ====== Running Step " & Step.Step_String'Image);

      case Step.Action is
         when Run_Cmd =>
            Created_File_List.Add (Output);
            Run_Cmd (Step         => Step,
                     Cmd          => To_String (Step.Object_String),
                     Output_Name  => Output,
                     Check_Result => False,
                     Spawn_OK     => Spawn_OK,
                     Return_Code  => Return_Code);
            Run_Error := not (Spawn_OK and Is_Success (Return_Code));

         when Run_Without_Error =>
            Created_File_List.Add (Output);
            Run_Cmd (Step         => Step,
                     Cmd          => To_String (Step.Object_String),
                     Output_Name  => Output,
                     Check_Result => True,
                     Spawn_OK     => Spawn_OK,
                     Return_Code  => Return_Code);
            Run_Error := not (Spawn_OK and Is_Success (Return_Code));

         when Error_Return_Code =>
            Return_Error (Return_Code, Step);

         when No_Error_Return_Code =>
            Return_No_Error (Return_Code, Step);

         when Output_Is =>
            Output_Is (Get_Text (Output), Step);

         when No_Output =>
            Check_No_Output
              (Get_Text (Output), Step);

         when Output_Contains =>
            -- example : Then the output contains `--version`
            Output_Contains
              (Get_Text (Output), Step);

         when Output_Matches =>
            -- example : Then the output contains `--version`
            Output_Matches
              (Get_Text (Output), Step);

         when Output_Does_Not_Contain =>
            Output_Does_Not_Contain
              (Get_Text (Output), Step);

         when Output_Does_Not_Match =>
            Output_Does_Not_Match
              (Get_Text (Output), Step);

         when File_Is =>
            Files_Is (Step);

         when File_Is_Not =>
            Files_Is_Not (Step);

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

         when Create_If_None =>
            Create_If_None (Step);

         when Erase_And_Create =>
            Erase_And_Create (Step);

         when Setup_No_File =>
            Setup_No_File (Step);

         when Setup_No_Dir =>
            Setup_No_Dir (Step);

         when None =>
            Add_Result (False, Step.Parent_Scenario.all);
            IO.Put_Error ("Unrecognized step " & Step.Step_String'Image,
                          Step.Location);

      end case;

   exception
      when E : others =>
         Add_Result (False, Step.Parent_Scenario.all);
         Put_Exception ("while processing step "
                        & Step'Image
                        & " : " & Ada.Exceptions.Exception_Name (E) & " "
                        & Ada.Exceptions.Exception_Message (E)
                        & GNAT.Traceback.Symbolic.Symbolic_Traceback (E),
                        Step.Location);

   end Run_Step;

   -- --------------------------------------------------------------------------
   procedure Run_Scenario (Scen : in out Scenario_Type) is
      Spawn_OK : Boolean := False;

   begin
      if Scen.Filtered then
         Put_Debug_Line ("  ====== Skipping filtered scen " & Scen.Name'Image);
         return;

      else
         Put_Debug_Line ("  ====== Running Scen " & Scen.Name'Image);
         Scen.Has_Run := True;

         Put_Scenario_Start (Scen);

         Step_Processing : for Step of Scen.Step_List loop
            Run_Step (Step, Spawn_OK);

            if IO.Some_Error and not Settings.Keep_Going then
               exit Step_Processing;
            end if;
         end loop Step_Processing;

         Put_Scenario_Result (Scen);

      end if;

      IO.New_Line (Verbosity => Verbose);

   end Run_Scenario;

   -- --------------------------------------------------------------------------
   procedure Run_Background (Scen : in out Scenario_Type) is
   begin
      if Scen.Filtered then
         Put_Debug_Line ("  ====== Skipping background of filtered scen " & Scen.Name'Image);
         return;

      else
         -- Run background scenarios
         Run_Doc_Background     (Scen);
         Run_Feature_Background (Scen);

      end if;

   end Run_Background;

   -- --------------------------------------------------------------------------
   procedure Run_Doc_Background (Scen : in out Scenario_Type) is
   -- Run background scenario at document level, if any
      Doc : constant Document_Type := Parent_Doc (Scen).all;
   begin
      if Has_Background (Doc) then
         if Doc.Background.Filtered then
            Put_Debug_Line
              ("  Skipping filtered document Background """ & (+Doc.Background.Name)
               & """  ", IO.No_Location);
         else
            Put_Debug_Line
              ("  Running document Background """ & (+Doc.Background.Name)
               & """  ", IO.No_Location);
            Run_Scenario (Doc.Background.all);
            Move_Results (From_Scen => Doc.Background.all,
                          To_Scen   => Scen);
         end if;
      end if;
   end Run_Doc_Background;

   -- --------------------------------------------------------------------------
   procedure Run_Feature_Background (Scen : in out Scenario_Type) is
   -- Run background scenario at feature level, if any
   begin
      if Is_In_Feature (Scen) and then Has_Background (Scen.Parent_Feature.all)
      then
         declare
            Feat : constant Feature_Type := Scen.Parent_Feature.all;
         begin
            if Scen.Parent_Feature.Filtered then
               Put_Debug_Line
                 ("  Skipping filtered feature Background """ & (+Feat.Background.Name) &
                    """  ", IO.No_Location);
            else
               Put_Debug_Line
                 ("  Running feature Background """ & (+Feat.Background.Name) &
                    """  ", IO.No_Location);
               Run_Scenario (Feat.Background.all);
               Move_Results (From_Scen => Feat.Background.all, To_Scen => Scen);
            end if;
         end;
      end if;
   end Run_Feature_Background;

   -- --------------------------------------------------------------------------
   procedure Run_Scenario_List (L : in out Scenario_Lists.Vector) is
   begin
      for Scen of L loop
         Run_Background (Scen);
         Run_Scenario (Scen);
         exit when IO.Some_Error and not Settings.Keep_Going;
      end loop;
   end Run_Scenario_List;

   -- --------------------------------------------------------------------------
   procedure Run_Doc (Doc : in out Document_Type)  is
      use File_Utilities;
      --  File_Count : constant Natural :=
      --                 Natural (Tests.Builder.The_Tests_List.Length);
      -- package CVer is new GNAT.Compiler_Version;

   begin
      if Doc.Filtered then
         Put_Debug_Line
           ("  Skipping filtered doc '" & (+Doc.Name) &
              "'  ", IO.No_Location);
         return;
      end if;


      declare
         Path_To_Scen  : constant String
           := Short_Path (From_Dir => Settings.Result_Dir,
                          To_File  => (+Doc.Name));
      begin
         Put_Document_Start (Doc);

         Status_Bar.Progress_Bar_Next_Step (Path_To_Scen); delay (0.1);

         if Doc.Scenario_List.Is_Empty and then Doc.Feature_List.Is_Empty
         then
            Put_Warning ("No scenario in document " & Doc.Name'Image & "  ",
                         Doc.Location);
         end if;

         -- Run scenarios directly attached to the document
         -- (that is not in a Feature)
         Run_Scenario_List (Doc.Scenario_List);
         if IO.Some_Error and not Settings.Keep_Going then
            return;
         end if;

         for F of Doc.Feature_List loop
            -- Then run scenarios attached to each Feature
            Writers.Put_Feature_Start (F);

            if F.Scenario_List.Is_Empty then
               Put_Warning
                 ("No scenario in feature " & F.Name'Image & "  ",
                  F.Location);
            else
               Run_Scenario_List (F.Scenario_List);

               if IO.Some_Error and not Settings.Keep_Going then
                  return;
               end if;
            end if;

         end loop;

      end;

      Created_File_List.Delete_All;
      -- All files and dir created during bbt run of this document
      -- are removed if -c | --cleanup option was used.
      -- Files are not removed when something goes wrong to ease debugging.

   exception
      when E : others =>
         Put_Exception (Ada.Exceptions.Exception_Message (E)
                        & GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
         Put_Debug_Line
           ("  exception in Run_Doc (" & (+Doc.Name) &
              ") ", IO.No_Location);
   end Run_Doc;

   -- --------------------------------------------------------------------------
   procedure Run_All is
      -- use File_Utilities;
      File_Count : constant Natural :=
                     Natural (Tests.Builder.The_Tests_List.Length);
      -- package CVer is new GNAT.Compiler_Version;

   begin
      -- First, let's move to a different exec dir, if any
      Ada.Directories.Set_Directory (Settings.Exec_Dir);

      if not Ada.Directories.Exists (Settings.Tmp_Dir) then
         Created_File_List.Add (Settings.Tmp_Dir);
         Ada.Directories.Create_Path (Settings.Tmp_Dir);
      end if;

      Status_Bar.Initialize_Progress_Bar (File_Count);

      --  Put_Line ("Time: " & Ada.Calendar.Formatting.Image
      -- -- or BBT.IO.Image??
      --            (Date                  => Ada.Calendar.Clock,
      --             Include_Time_Fraction => True));
      -- Put_Line ("GNAT version: " & CVer.Version);

      -- let's run the test
      for D of BBT.Tests.Builder.The_Tests_List.all loop
         Run_Doc (D);

         if IO.Some_Error and not Settings.Keep_Going then
            exit;
         end if;

      end loop;

   end Run_All;

end BBT.Tests.Runner;
