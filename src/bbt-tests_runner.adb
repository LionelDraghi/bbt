with BBT.Documents;
with BBT.IO;
with BBT.Settings;          use BBT.Settings;
with BBT.Tests_Builder;

with Text_Utilities; use Text_Utilities;

with GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;

with Ada.Command_Line;
with Ada.Directories;       use Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

package body BBT.Tests_Runner is

   -- --------------------------------------------------------------------------
   -- IO renamed with local Topic
   procedure Put_Line
     (Item  : String;
      File  : String  := "";
      Line  : Integer := 0;
      Level : BBT.Settings.Print_Out_Level := BBT.IO.Normal;
      Topic : Settings.Extended_Topics := Settings.Runner) renames IO.Put_Line;

   -- --------------------------------------------------------------------------
   procedure Spawn_Cmd (Cmd         : String;
                        Output_File : String;
                        Spawn_OK    : out Boolean;
                        Return_Code : out Integer) is
      -- Spawn the Cmd under strace.
      -- OK is set to True if the spawn did it well, and if the command
      -- returns True.

      use GNAT.OS_Lib;
      -- Initial_Dir : constant String  := Current_Directory;
      Spawn_Arg   : constant Argument_List_Access
        := Argument_String_To_List (Cmd);

   begin
      -- Set_Directory (Settings.Run_Dir_Name);

      --  -- Put_Line ("Cmd :" & Cmd);
      --  for A of Spawn_Arg.all loop
      --     Put_Line ("Arg >" & A.all & "<", Level => Debug); -- , Level => Settings.Debug);
      --  end loop;
      --  if Exists (Spawn_Arg.all (1).all) then
      Spawn (Program_Name => Spawn_Arg.all (1).all,
             Args         => Spawn_Arg.all (2 .. Spawn_Arg'Last),
             Success      => Spawn_OK,
             Output_File  => Output_File,
             Return_Code  => Return_Code,
             Err_To_Out   => True);
      --  else
      -- Set_Directory (Initial_Dir);

   end Spawn_Cmd;

   -- --------------------------------------------------------------------------
   procedure Run_All is
      Spawn_OK    : Boolean := True;
      Return_Code : Integer;
      use Documents;

      function Is_Success (I : Integer) return Boolean is
        (I = Integer (Ada.Command_Line.Success));
      -- Fixme: can I compare the return status of spawn with
      --        Ada.Command_Line Success or Fail?

      function Output_File_Name (D : Document_Type) return String is
        (To_String (D.Name) & ".out");

      -- -----------------------------------------------------------------------
      procedure Run_Cmd (Cmd              : String;
                         Output_Name      : String;
                         Spawn_OK         : out Boolean;
                         Return_Code      : out Integer) is
      begin
         -- Put_Line ("Running " & Cmd'Image, Level => Verbose);
         Spawn_Cmd (Cmd         => Cmd,
                    Output_File => Output_Name,
                    Spawn_OK    => Spawn_OK,
                    Return_Code => Return_Code);
         if not Spawn_OK then
            Put_Line ("unable to run """ & Cmd & """",
                      Level => IO.Quiet);
         end if;
      end Run_Cmd;

      -- -----------------------------------------------------------------------
      function Get_Expected (Step : Step_Type) return Text is
         T : Text;
         use type Text;
      begin
         if Step.Expected_Output /= Null_Unbounded_String then
            T := [1 => To_String (Step.Expected_Output)];
         elsif Step.File_Content /= Empty_Text then
            T := Step.File_Content;
         else
            T := Get_Text (Step.File_Name);
         end if;
         return T;
      end Get_Expected;

      -- -----------------------------------------------------------------------
      procedure Run_Scenario (The_Scenario : in out Scenario_Type;
                              The_Feature  :        Feature_Type;
                              The_Doc      :        Document_Type) is
         pragma Unreferenced (The_Feature);
         Prefix : constant array (Boolean) of String (1 .. 10) :=
                    [True  => "    OK  : ",
                     False => "    NOK : "];
      begin
         Step_Processing : for Step of The_Scenario.Step_List loop

            begin
               case Step.Kind is
                  when Run_Cmd =>
                     Run_Cmd (Cmd         => To_String (Step.Cmd),
                              Output_Name => Output_File_Name (The_Doc),
                              Spawn_OK    => Spawn_OK,
                              Return_Code => Return_Code);
                     if Spawn_OK then
                        Add_Success (The_Scenario);
                        Put_Line (Prefix (Spawn_OK) & "Run " & Step.Cmd'Image
                                  & "  ",
                                  Level => IO.Verbose);
                     else
                        Add_Fail (The_Scenario);
                        -- We don't want the run fail to goes unnoticed,
                        -- so an error is immediatly added
                        Put_Line (Prefix (Spawn_OK) & "Couldn't run " &
                                    Step.Cmd'Image & "  ",
                                  Level => IO.Quiet);
                     end if;
                     if not Spawn_OK then
                        Put_Line (Prefix (Spawn_OK) & "Exiting scenario");
                        exit Step_Processing;
                     end if;

                  when Successfully_Run_Cmd =>
                     Run_Cmd (Cmd         => To_String (Step.Cmd),
                              Output_Name => Output_File_Name (The_Doc),
                              Spawn_OK    => Spawn_OK,
                              Return_Code => Return_Code);
                     if Spawn_OK and Is_Success (Return_Code) then
                        Add_Success (The_Scenario);
                        Put_Line (Prefix (True) & "Successfully run " &
                                    Step.Cmd'Image & "  ",
                                  Level => IO.Verbose);
                     else
                        Add_Fail (The_Scenario);
                        Put_Line (Prefix (False) & "Unsuccessfully run " &
                                    Step.Cmd'Image & "  ",
                                  Level => IO.Quiet);
                     end if;
                     if not Spawn_OK then
                        Put_Line (Prefix (Spawn_OK) & "Exiting scenario");
                        exit Step_Processing;
                     end if;

                  when Error_Return_Code =>
                     if not Is_Success (Return_Code) then
                        -- error expected, it's a fail
                        Add_Success (The_Scenario);
                        Put_Line (Prefix (True) & "Got expected error code  ",
                                  Level => IO.Verbose);
                     else
                        Add_Fail (The_Scenario);
                        Put_Line
                          (Prefix (False)
                           & "Expected error code, got no error  ",
                           Level => IO.Quiet);
                     end if;

                  when No_Error_Return_Code =>
                     if Is_Success (Return_Code) then
                        Add_Success (The_Scenario);
                        Put_Line (Prefix (True) & "Got no error as expected  ",
                                  Level => IO.Verbose);
                     else
                        Add_Fail (The_Scenario);
                        Put_Line (Prefix (False)
                                  & "No error expected, but got one  ",
                                  Level => IO.Quiet);
                     end if;

                  when Output_Is =>
                     declare
                        T1 : constant Text
                          := Get_Text (Output_File_Name (The_Doc));
                        T2 : constant Text := Get_Expected (Step);
                     begin
                        if Is_Equal (T1, T2) then
                           Add_Success (The_Scenario);
                           Put_Line (Prefix (True) & "Output equal to expected "
                                     & T2'Image & "  ",
                                     Level => IO.Verbose);
                        else
                           Add_Fail (The_Scenario);
                           Put_Line (Prefix (False) & "Output " & T1'Image
                                     & "  ",
                                     Level => IO.Quiet);
                           Put_Line ("    not equal to expected  "
                                     & T2'Image & "  ",
                                     Level => IO.Quiet);
                        end if;
                     end;

                  when Output_Contains =>
                     -- example : Then the output contains `--version`
                     declare
                        T1 : constant Text
                          := Get_Text (Output_File_Name (The_Doc));
                        T2 : constant Text := Get_Expected (Step);
                     begin
                        if Contains (T1, T2) then
                           Add_Success (The_Scenario);
                           Put_Line (Prefix (True) & "Output contains expected "
                                     & T2'Image & "  ",
                                     Level => IO.Verbose);
                        else
                           Add_Fail (The_Scenario);
                           Put_Line (Prefix (False) & "Output " & T1'Image
                                     & "  ",
                                     Level => IO.Quiet);
                           Put_Line ("    does not contain expected  "
                                     & T2'Image & "  ",
                                     Level => IO.Quiet);
                        end if;
                     end;

                  when File_Is   =>
                     declare
                        File_Name : constant String
                          := To_String (Step.File_Name);
                        T1        : constant Text := Get_Text (File_Name);
                        T2        : constant Text := Get_Expected (Step);
                     begin
                        if Is_Equal (T1, T2) then
                           Add_Success (The_Scenario);
                           Put_Line (Prefix (True)
                                     & "File is equal to expected "
                                     & T2'Image & "  ",
                                     Level => IO.Verbose);
                        else
                           Add_Fail (The_Scenario);
                           Put_Line (Prefix (False) & File_Name
                                     & " " & T1'Image
                                     & " not equal to " & T2'Image & "  ",
                                     Level => IO.Quiet);
                        end if;
                     end;

                  when File_Contains   =>
                     declare
                        File_Name : constant String
                          := To_String (Step.File_Name);
                        T1        : constant Text := Get_Text (File_Name);
                        T2        : constant Text := Get_Expected (Step);

                     begin
                        if Contains (T1, T2) then
                           Add_Success (The_Scenario);
                           Put_Line (Prefix (True)
                                     & "  File contains expected " & T2'Image
                                     & "  ",
                                     Level => IO.Verbose);
                        else
                           Add_Fail (The_Scenario);
                           Put_Line (Prefix (False) & "file "
                                     & To_String (Step.File_Name)
                                     & " does not contain "
                                     & T2'Image & "  ",
                                     Level => IO.Quiet);
                        end if;
                     end;

                  when No_File =>
                     declare
                        File_Name : constant String
                          := To_String (Step.File_Name);
                     begin
                        if not Ada.Directories.Exists (File_Name)
                        then
                           Add_Success (The_Scenario);
                           Put_Line (Prefix (True) & "file "
                                     & File_Name'Image
                                     & " doesn't exists" & "  ",
                                     Level => IO.Verbose);
                        else
                           Add_Fail (The_Scenario);
                           Put_Line (Prefix (False) & "file "
                                     & File_Name'Image
                                     & " shouldn't exists" & "  ",
                                     Level => IO.Quiet);
                        end if;
                     end;

                  when Existing_File =>
                     declare
                        File_Name : constant String
                          := To_String (Step.File_Name);
                     begin
                        if Exists (File_Name) then
                           if Kind (File_Name) = Step.File_Type then
                              Add_Success (The_Scenario);
                              Put_Line (Prefix (True) & "Expected file "
                                        & File_Name'Image & " exists" & "  ",
                                        Level => IO.Verbose);
                           else
                              Add_Fail (The_Scenario);
                              Put_Line
                                (Prefix (False) & "File " & File_Name'Image
                                 & " exists but isn't a dir/file as expected  ",
                                 Level => IO.Quiet);
                           end if;

                        else
                           Add_Fail (The_Scenario);
                           Put_Line (Prefix (False) & "Expected file "
                                     & File_Name'Image & " doesn't exists  ",
                                     Level => IO.Quiet);
                        end if;
                     end;

                  when File_Creation =>
                     if Step.File_Type = Ordinary_File then
                        if Create_File
                          (File_Name    => Step.File_Name,
                           With_Content => Step.File_Content)
                        then
                           Add_Success (The_Scenario);

                           if Settings.Is_Authorised (Verbose) then
                              Put_Text (Step.File_Content);
                           end if;

                        else
                           Add_Fail (The_Scenario);
                           -- But if it fails, we don't want that to
                           -- to goes unnoticed.
                           Put_Line (Prefix (False) & "Unable to create file " &
                                       Step.File_Name'Image & "  ",
                                     Level => IO.Quiet);
                        end if;

                     elsif Step.File_Type = Directory then
                        declare
                           Dir_Name : constant String
                             := To_String (Step.File_Name);
                        begin
                           Create_Directory (Dir_Name);
                           if Exists (Dir_Name) and Kind (Dir_Name) = Directory then
                              Add_Success (The_Scenario);
                              Put_Line (Prefix (True) & "Dir "
                                        & Step.File_Name'Image & " created  ",
                                        Level => IO.Verbose);
                           else
                              Add_Fail (The_Scenario);
                              Put_Line (Prefix (False) & "Unable to create directory " &
                                          Step.File_Name'Image & "  ",
                                        Level => IO.Quiet);
                           end if;
                        end;
                     end if;

                  when Unknown =>
                     Add_Fail (The_Scenario);
                     Put_Line (Prefix (False) & "Unrecognised step "
                               & Step.Step_String'Image & "  ",
                               Level => IO.Quiet);

               end case;

            exception
               when E : others =>
                  Add_Fail (The_Scenario);
                  Put_Line (Ada.Exceptions.Exception_Message (E),
                            Level => IO.Quiet);
                  Put_Line ("Unkown exception while processing scenario "
                            & The_Scenario.Name'Image,
                            Level => IO.Quiet);
                  Ada.Text_IO.Put_Line
                    (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));

            end;

         end loop Step_Processing;

      end Run_Scenario;

   begin
      -- let's run the test
      for D of BBT.Tests_Builder.The_Document_List.all loop
         IO.New_Line (Level => Verbose);
         Put_Line ("Running file " & D.Name'Image, Level => Verbose);
         for F of D.Feature_List loop

            for Scen of F.Scenario_List loop
               Return_Code := 0;

               Put_Line ("  Running scenario " & Scen.Name'Image,
                         Level => Verbose);

               -- Run background scenario at document level, if any
               if D.Background /= Empty_Scenario then
                  Put_Line ("  Running background " & D.Background.Name'Image,
                            Level => Verbose);
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
                  Put_Line ("  Running background " & F.Background.Name'Image,
                            Level => Verbose);
                  Run_Scenario (F.Background,
                                F,
                                D);
                  -- Background results are reported in the scenario
                  Scen.Failed_Step_Count
                    := @ + F.Background.Failed_Step_Count;
                  Scen.Successful_Step_Count
                    := @ + F.Background.Successful_Step_Count;
               end if;

               -- ANd finally run the scenario
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
      end loop;
   end Run_All;

end BBT.Tests_Runner;
