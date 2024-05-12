with BBT.Documents;         use BBT.Documents;

with BBT.IO;
with BBT.Tests.Builder;
with BBT.Tests.Actions;     use BBT.Tests.Actions;
with Text_Utilities;        use Text_Utilities;

with GNAT.Traceback.Symbolic;

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body BBT.Tests.Runner is

   -- -----------------------------------------------------------------------
   function Output_File_Name (D : Document_Type) return String is
     (To_String (D.Name) & ".out");

   function Is_Success (I : Integer) return Boolean is
     (I = Integer (Ada.Command_Line.Success));
   -- Fixme: can I compare the return status of spawn with
   --        Ada.Command_Line Success or Fail?

   function Subject_Or_Object_String (Step : Step_Type) return String is
     (To_String (if Step.Object_String = Null_Unbounded_String then
           Step.Subject_String else Step.Object_String));

   Return_Code : Integer := 0;

   -- -----------------------------------------------------------------------
   procedure Run_Scenario (The_Scenario : in out Scenario_Type;
                           The_Feature  :        Feature_Type;
                           The_Doc      :        Document_Type) is
      pragma Unreferenced (The_Feature);
   begin
      Step_Processing : for Step of The_Scenario.Step_List loop
         declare
            Spawn_OK : Boolean := False;
         begin
            case Step.Action is
               when Run_Cmd =>
                  Run_Cmd (Cmd         => To_String (Step.Object_String),
                           Output_Name => Output_File_Name (The_Doc),
                           Spawn_OK    => Spawn_OK,
                           Return_Code => Return_Code);
                  Put_Step_Result (Step        => Step,
                                   Success     => Spawn_OK,
                                   --  Success_Msg => "Run " &
                                   --    Step.Object_String'Image,
                                   Fail_Msg    => "Couldn't run " &
                                     Step.Object_String'Image,
                                   Loc         => Step.Location,
                                   Scenario    => The_Scenario);
                  if not Spawn_OK then
                     exit Step_Processing;
                  end if;

               when Run_Without_Error =>
                  Run_Cmd (Cmd         => To_String (Step.Object_String),
                           Output_Name => Output_File_Name (The_Doc),
                           Spawn_OK    => Spawn_OK,
                           Return_Code => Return_Code);

                  if not Spawn_OK then
                     Put_Step_Result (Step => Step,
                                      Success     => False,
                                      -- Success_Msg => "", -- impossible
                                      Fail_Msg    => "Couldn't run " &
                                        Step.Object_String'Image,
                                      Loc         => Step.Location,
                                      Scenario    => The_Scenario);
                     exit Step_Processing;

                  else
                     Put_Step_Result (Step => Step,
                                      Success     => Is_Success (Return_Code),
                                      --  Success_Msg => "Successfully run " &
                                      --    Step.Object_String'Image,
                                      Fail_Msg    => "Unsuccessfully run " &
                                        Step.Object_String'Image,
                                      Loc         => Step.Location,
                                      Scenario    => The_Scenario);
                  end if;

               when Error_Return_Code =>
                  Return_Error (Return_Code, Step, The_Scenario);

               when No_Error_Return_Code =>
                  Return_No_Error (Return_Code, Step, The_Scenario);

               when Output_Is =>
                  Output_Equal_To (Get_Text (Output_File_Name (The_Doc)),
                                   Step,
                                   The_Scenario);

               when Output_Contains =>
                  -- example : Then the output contains `--version`
                  Output_Contains (Get_Text (Output_File_Name (The_Doc)),
                                   Step,
                                   The_Scenario);

               when File_Is   =>
                  Files_Equal_To (Step, The_Scenario);

               when File_Contains   =>
                  File_Contains (Step, The_Scenario);

               when Check_No_File =>
                  Check_No_File (Subject_Or_Object_String (Step),
                                 Step,
                                 The_Scenario);

               when Check_File_Existence =>
                  Check_File_Existence (Subject_Or_Object_String (Step),
                                        Step,
                                        The_Scenario);

               when Create_If_None =>
                  Create_File (Step, The_Scenario);

               when Create_New =>
                  Delete_File (Step, The_Scenario);
                  Create_File (Step, The_Scenario);

               when None =>
                  Add_Result (False, The_Scenario);
                  IO.Put_Error ("Unrecognized step " & Step.Step_String'Image,
                                Step.Location);

            end case;

         exception
            when E : others =>
               Add_Result (False, The_Scenario);
               Put_Exception ("while processing scenario "
                              & The_Scenario.Name'Image
                              & Ada.Exceptions.Exception_Message (E)
                              & GNAT.Traceback.Symbolic.Symbolic_Traceback (E),
                              Step.Location);
         end;

      end loop Step_Processing;

   end Run_Scenario;

   -- --------------------------------------------------------------------------
   procedure Run_All is
   begin
      -- let's run the test
      for D of BBT.Tests.Builder.The_Tests_List.all loop
         IO.New_Line (Verbosity => IO.Normal);
         Put_Line ("Running file " & D.Name'Image & "  ");
         for F of D.Feature_List loop

            for Scen of F.Scenario_List loop
               IO.New_Line (Verbosity => IO.Normal);
               Put_Line ("  Scenario " & Scen.Name'Image & "  ",
                         IO.No_Location, IO.Verbose);

               -- Run background scenario at document level, if any
               if D.Background /= Empty_Scenario then
                  Put_Line ("  Document Background " & (+D.Background.Name) &
                              "  ", IO.No_Location, IO.Verbose);
                  Run_Scenario (D.Background,
                                F,
                                D);
                  -- Background results are reported in the scenario
                  Scen.Failed_Step_Count
                    := @ + D.Background.Failed_Step_Count;
                  Scen.Successful_Step_Count
                    := @ + D.Background.Successful_Step_Count;
               end if;

               -- Run background scenario at feature level, if any
               if F.Background /= Empty_Scenario then
                  Put_Line ("  Feature background " & (+F.Background.Name) &
                              "  ", IO.No_Location, IO.Verbose);
                  Run_Scenario (F.Background,
                                F,
                                D);
                  -- Background results are reported in the scenario
                  Scen.Failed_Step_Count
                    := @ + F.Background.Failed_Step_Count;
                  Scen.Successful_Step_Count
                    := @ + F.Background.Successful_Step_Count;
               end if;
               Put_Line ("  Scenario  ", IO.No_Location, IO.Verbose);

               -- And finally run the scenario
               Run_Scenario (Scen,
                             F,
                             D);

               case Documents.Result (Scen) is
               when Empty =>
                  -- Note the two spaces at the end to cause a new line
                  -- in Markdown format when this line is followed by an
                  -- error message
                  Put_Line ("  - [ ] [" & To_String (Scen.Name) & "]("
                            & To_String (D.Name)
                            & ") is empty, nothing tested  ");
               when Successful =>
                  Put_Line ("  - [X] [" & To_String (Scen.Name) & "]("
                            & To_String (D.Name)
                            & ") pass  ");
               when Failed =>
                  Put_Line ("  - [ ] [" & To_String (Scen.Name) & "]("
                            & To_String (D.Name)
                            & ") fails  ");
               end case;
            end loop;
         end loop;
         -- New_Line (Verbosity => IO.Normal);
      end loop;

   end Run_All;

end BBT.Tests.Runner;
