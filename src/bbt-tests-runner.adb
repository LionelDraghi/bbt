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
      Error_Output : constant String :=
                       Output (Output'First .. Output'Last - 4) & ".err";
      -- Output ends with ".out"

      function Checks_Error_Output return Boolean is
         (for some S of Parent (Step).Step_List =>
             S.Data.Action in Stderr_Is
                            | Stderr_Contains
                            | Stderr_Does_Not_Contain
                            | No_Stderr);
      -- When the scenario checks the error output, the commands it runs
      -- write their standard error to Error_Output, and their standard
      -- output alone to Output.

      function Error_Output_Name return String is
        (if Checks_Error_Output then Error_Output else "");

   begin
      Run_Error := False;

      if Step.Filtered then
         Put_Debug_Line
           ("  ====== Skipping filtered step " & Step.Data.Src_Code'Image);
         return;
      end if;

      Set_Start_Time (Step);

      if Step.Has_Syntax_Error then
      -- Fixme: defensive code that should be replaced by
      -- an assertion.
         Put_Warning ("Skipping step with syntax error", Step.Location);
         Run_Error := True;
         return;
      end if;

      Put_Debug_Line ("  ====== Running Step " & Step.Data.Src_Code'Image);

      if Step.Data.Action in Output_Is
                           | Output_Contains
                           | Output_Does_Not_Contain
                           | Output_Matches
                           | Output_Does_Not_Match
                           | No_Output
                           | Stderr_Is
                           | Stderr_Contains
                           | Stderr_Does_Not_Contain
                           | No_Stderr
      then
         -- The output file may not exist, for instance when the command
         -- of a previous run step could not be spawned, and the run
         -- continued (--keep_going): report a clean error on the step
         -- instead of raising NAME_ERROR in Get_Text.
         declare
            File_Name : constant String :=
                          (if Step.Data.Action in Stderr_Is
                                                      | Stderr_Contains
                                                      | Stderr_Does_Not_Contain
                                                      | No_Stderr
                           then Error_Output else Output);
         begin
            if not Ada.Directories.Exists (File_Name) then
               Put_Step_Result (Step     => Step,
                                Success  => False,
                                Fail_Msg => "cannot open "
                                  & Ada.Directories.Simple_Name (File_Name)
                                  & ": the command did not run",
                                Loc       => Step.Location,
                                Verbosity => Verbosity);
               Run_Error := True;
               Set_End_Time (Step);
               return;
            end if;
         end;
      end if;

      case Step.Data.Action is
         when Run_Cmd =>
            Created_File_List.Add (Output);
            if Error_Output_Name /= "" then
               Created_File_List.Add (Error_Output);
            end if;
            Run_Cmd (Step            => Step,
                     Cmd             => To_String (Step.Data.Object_String),
                     Output_Name     => Output,
                     Expected_Result => Not_Specified,
                     Verbosity       => Verbosity,
                     Spawn_OK        => Spawn_OK,
                     Return_Code     => Return_Code,
                     Error_Output_Name => Error_Output_Name);
            Run_Error := not (Spawn_OK);

         when Run_Without_Error =>
            Created_File_List.Add (Output);
            if Error_Output_Name /= "" then
               Created_File_List.Add (Error_Output);
            end if;
            Run_Cmd (Step            => Step,
                     Cmd             => To_String (Step.Data.Object_String),
                     Output_Name     => Output,
                     Expected_Result => Success,
                     Verbosity       => Verbosity,
                     Spawn_OK        => Spawn_OK,
                     Return_Code     => Return_Code,
                     Error_Output_Name => Error_Output_Name);
            Run_Error := not (Spawn_OK);

         when Run_With_Error =>
            Created_File_List.Add (Output);
            if Error_Output_Name /= "" then
               Created_File_List.Add (Error_Output);
            end if;
            Run_Cmd (Step            => Step,
                     Cmd             => To_String (Step.Data.Subject_String),
                     Output_Name     => Output,
                     Expected_Result => Failure,
                     Verbosity       => Verbosity,
                     Spawn_OK        => Spawn_OK,
                     Return_Code     => Return_Code,
                     Error_Output_Name => Error_Output_Name);
            Run_Error := not (Spawn_OK);

         when Error_Return_Code =>
            Return_Error (Last_Exit_Code, Step, Verbosity);

         when No_Error_Return_Code =>
            Return_No_Error (Last_Exit_Code, Step, Verbosity);

         when Output_Is =>
            Output_Is (Get_Text (Output), Step, Verbosity);

         when No_Output =>
            Check_No_Output
              (Get_Text (Output), Step, Verbosity);

         when Output_Contains =>
            Output_Contains (Get_Text (Output), Step, Verbosity);

         when Output_Does_Not_Contain =>
            Output_Does_Not_Contain
              (Get_Text (Output), Step, Verbosity);

         when Output_Matches =>
            Output_Matches
              (Get_Text (Output), Step, Verbosity);

         when Output_Does_Not_Match =>
            Output_Does_Not_Match
              (Get_Text (Output), Step, Verbosity);

         when File_Matches =>
            File_Matches (Step, Verbosity);

         when File_Does_Not_Match =>
            File_Does_Not_Match (Step, Verbosity);

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

         when Set_Env_Var =>
            Set_Env_Var (Step, Verbosity);

         when Unset_Env_Var =>
            Unset_Env_Var (Step, Verbosity);

         when Stderr_Is =>
            Output_Is (Get_Text (Error_Output), Step, Verbosity);

         when Stderr_Contains =>
            Output_Contains (Get_Text (Error_Output), Step, Verbosity);

         when Stderr_Does_Not_Contain =>
            Output_Does_Not_Contain (Get_Text (Error_Output), Step, Verbosity);

         when No_Stderr =>
            Check_No_Output (Get_Text (Error_Output), Step, Verbosity);

         when Exit_Code_Is =>
            Exit_Code_Is (Step, Verbosity);

         when None =>
            IO.Put_Error ("Unrecognized step " & Step.Data.Src_Code'Image,
                          Step.Location);
            IO.Put_Error ("Internal error, should have been identified in Validate_Step_State and Has_Syntax_Error set.",
                          Step.Location);
            IO.Put_Error ("Please report the step line and your bbt version to maintainer at https://github.com/LionelDraghi/bbt/issues",
                          Step.Location);

      end case;
      Set_End_Time (Step);

   exception
      when E : others =>
         --  An exception while processing a step is a failure of
         --  the step, and thus of the scenario.
         Run_Error := True;
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

         Set_Start_Time (Scen);

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
         Set_End_Time (Scen);

      end if;

      IO.New_Line (Verbosity => Verbose);

   end Run_Scenario;

   -- --------------------------------------------------------------------------
   procedure Run_Feature (F : in out Feature_Type'Class) is
   begin
      Writers.Put_Feature_Start (F);
      Set_Start_Time (F);

      if F.Scenario_List.Is_Empty then
         Put_Warning ("No scenario in feature " & F.Name'Image & "  ",
                      F.Location);
      else
         Run_Scenario_List (F.Scenario_List);
      end if;
      Set_End_Time (F);
   end Run_Feature;

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
         Restore_Environment;
         -- Variables set in the scenario or its backgrounds apply to that
         -- scenario only.
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

      Set_Start_Time (Doc);

      declare
         Path_To_Scen  : constant String
           := Short_Path (From_Dir => Settings.Index_Dir,
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
            Set_End_Time (Doc);
            return;
         end if;

         for F of Doc.Feature_List loop
            -- Then run scenarios attached to each Feature
            Run_Feature (F);
            if IO.Some_Error and Settings.Stop_On_Error then
               Set_End_Time (Doc);
               return;
            end if;

         end loop;

      end;

      Set_End_Time (Doc);

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
