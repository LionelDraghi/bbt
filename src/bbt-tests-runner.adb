-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Created_File_List,
     BBT.Model,
     BBT.Model.Documents,
     BBT.Model.Features,
     BBT.Model.Scenarios,
     BBT.Model.Steps,
     BBT.IO,
     BBT.Settings,
     BBT.Status_Bar,
     BBT.Tests.Actions,
     BBT.Writers,
     File_Utilities,
     Text_Utilities;

use BBT.Created_File_List,
    BBT.Model,
    BBT.Model.Documents,
    BBT.Model.Features,
    BBT.Model.Scenarios,
    BBT.Model.Steps,
    BBT.IO,
    BBT.Tests.Actions,
    BBT.Writers,
    Text_Utilities;

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
   function Subject_Or_Object_String (Step : Step_Type'Class) return String is
     (To_String (if Step.Data.Object_File_Name = Null_Unbounded_String then
           Step.Data.Subject_String else Step.Data.Object_File_Name));
   -- Fixme: Clearly not confortable with that function, it's magic.


   -- --------------------------------------------------------------------------
   procedure Run_Step (Step      : in out Step_Type'Class;
                       Run_Error :    out Boolean;
                       Verbosity : Verbosity_Levels);
   procedure Run_Scenario (Scen : in out Scenario_Type'Class);
   -- run only the scenario, not Doc and / or Feature Background.
   procedure Run_Background (Scen : in out Scenario_Type'Class);
   -- run only Doc and / or Feature Background, not the scenario himself.
   procedure Run_Doc_Background (Scen : in out Scenario_Type'Class);
   procedure Run_Feature_Background (Scen : in out Scenario_Type'Class);
   procedure Run_Scenario_List (L : in out Scenario_Lists.Vector);

   -- --------------------------------------------------------------------------
   procedure Run_Step (Step      : in out Step_Type'Class;
                       Run_Error :    out Boolean;
                       Verbosity : Verbosity_Levels)
   is
      Spawn_OK    : Boolean;
      Return_Code : Integer;
      Output      : constant String :=
                      Output_File_Name (Parent_Doc (Step).all);
   begin
      Run_Error := False;

      if Step.Filtered then
         Put_Debug_Line
           ("  ====== Skipping filtered step " & Step.Data.Src_Code'Image);
         return;
      end if;

      if Step.Has_Syntax_Error then
      -- Fixme: defensive code that should be replaced by
      -- an assertion.
         Put_Warning ("Skipping step with syntax error", Step.Location);
         Run_Error := True;
         return;
      end if;

      Put_Debug_Line ("  ====== Running Step " & Step.Data.Src_Code'Image);

      case Step.Data.Action is
         when Run_Cmd =>
            Created_File_List.Add (Output);
            Run_Cmd (Step         => Step,
                     Cmd          => To_String (Step.Data.Object_String),
                     Output_Name  => Output,
                     Check_Result => False,
                     Verbosity    => Verbosity,
                     Spawn_OK     => Spawn_OK,
                     Return_Code  => Return_Code);
            Run_Error := not (Spawn_OK);

         when Run_Without_Error =>
            Created_File_List.Add (Output);
            Run_Cmd (Step         => Step,
                     Cmd          => To_String (Step.Data.Object_String),
                     Output_Name  => Output,
                     Check_Result => True,
                     Verbosity    => Verbosity,
                     Spawn_OK     => Spawn_OK,
                     Return_Code  => Return_Code);
            Run_Error := not (Spawn_OK);

         when Error_Return_Code =>
            Return_Error (Return_Code, Step, Verbosity);

         when No_Error_Return_Code =>
            Return_No_Error (Return_Code, Step, Verbosity);

         when Output_Is =>
            Output_Is (Get_Text (Output), Step, Verbosity);

         when No_Output =>
            Check_No_Output
              (Get_Text (Output), Step, Verbosity);

         when Output_Contains =>
            Output_Contains
              (Get_Text (Output), Step, Verbosity);

         when Output_Matches =>
            Output_Matches
              (Get_Text (Output), Step, Verbosity);

         when Output_Does_Not_Contain =>
            Output_Does_Not_Contain
              (Get_Text (Output), Step, Verbosity);

         when Output_Does_Not_Match =>
            Output_Does_Not_Match
              (Get_Text (Output), Step, Verbosity);

         when File_Is =>
            Files_Is (Step, Verbosity);

         when File_Is_Not =>
            Files_Is_Not (Step, Verbosity);

         when File_Contains =>
            File_Contains (Step, Verbosity);

         when File_Does_Not_Contain =>
            File_Does_Not_Contain (Step, Verbosity);

         when Check_No_File =>
            Check_No_File (Subject_Or_Object_String (Step), Step, Verbosity);

         when Check_No_Dir =>
            Check_No_Dir (Subject_Or_Object_String (Step), Step, Verbosity);

         when Check_File_Existence =>
            Check_File_Existence (Subject_Or_Object_String (Step), Step, Verbosity);

         when Check_Dir_Existence =>
            Check_Dir_Existence (Subject_Or_Object_String (Step), Step, Verbosity);

         when Create_If_None =>
            Create_If_None (Step, Verbosity);

         when Erase_And_Create =>
            Erase_And_Create (Step, Verbosity);

         when Setup_No_File =>
            Setup_No_File (Step, Verbosity);

         when Setup_No_Dir =>
            Setup_No_Dir (Step, Verbosity);

         when None =>
            IO.Put_Error ("Unrecognized step " & Step.Data.Src_Code'Image,
                          Step.Location);

      end case;

   exception
      when E : others =>
         Put_Exception ("while processing step "
                        & Step'Image
                        & " : " & Ada.Exceptions.Exception_Name (E) & " "
                        & Ada.Exceptions.Exception_Message (E)
                        & GNAT.Traceback.Symbolic.Symbolic_Traceback (E),
                        Step.Location);

   end Run_Step;

   -- --------------------------------------------------------------------------
   procedure Run_Scenario (Scen : in out Scenario_Type'Class) is
      Run_Error : Boolean := False;
      Verbosity : constant Verbosity_Levels :=
                    (if Scen.Is_Background then Verbose else Normal);
   begin
      Reset_Error_Counts;
      if Scen.Filtered then
         Put_Debug_Line ("  ====== Skipping filtered scen " & Scen.Name'Image);
         return;

      else
         Put_Debug_Line ("  ====== Running Scen " & Scen.Name'Image);
         Scen.Has_Run := True;

         Put_Scenario_Start (Scen, Verbosity);

         Step_Processing : for Step of Scen.Step_List loop
            Run_Step (Step      => Step,
                      Run_Error => Run_Error,
                      Verbosity => Verbosity);
            Add_Result (Success => (not Run_Error) and IO.No_Error,
                        To      => Scen);
            exit Step_Processing when IO.Some_Error and Settings.Stop_On_Error;

         end loop Step_Processing;

         Put_Scenario_Result (Scen, Verbosity);

      end if;

      IO.New_Line (Verbosity => Verbose);

   end Run_Scenario;

   -- --------------------------------------------------------------------------
   procedure Run_Background (Scen : in out Scenario_Type'Class) is
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
   procedure Run_Doc_Background (Scen : in out Scenario_Type'Class) is
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
   procedure Run_Feature_Background (Scen : in out Scenario_Type'Class) is
   -- Run background scenario at feature level, if any
   begin
      if Is_In_Feature (Scen) and then Has_Background (Scen.Parent.all)
      then
         declare
            Feat : constant Feature_Type := Feature_Type (Scen.Parent.all);
         begin
            if Scen.Parent.Filtered then
               Put_Debug_Line
                 ("  Skipping filtered feature Background """ &
                  (+Feat.Background.Name) &
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
   procedure Run_Doc (Doc : in out Document_Type'Class)  is
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
         if IO.Some_Error and Settings.Stop_On_Error then
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

               if IO.Some_Error and Settings.Stop_On_Error then
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
           ("  exception in Run_Doc (" & (+Doc.Name) & ") ",
            IO.No_Location);
   end Run_Doc;

   -- --------------------------------------------------------------------------
   procedure Run_All is
      File_Count : constant Natural := Natural (Doc_List.Length);
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
      for D of Doc_List.all loop
         Run_Doc (D);

         if IO.Some_Error and Settings.Stop_On_Error then
            exit;
         end if;

      end loop;

   end Run_All;

end BBT.Tests.Runner;
