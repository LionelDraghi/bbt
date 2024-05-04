with BBT.Documents;
with BBT.IO;
with BBT.Settings;          use BBT.Settings;
with BBT.Tests_Builder;

with GNAT.OS_Lib;

with Ada.Command_Line;
with Ada.Directories;       use Ada.Directories;
-- with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Text_Utilities; use Text_Utilities;

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
         if Step.Details.Expected_Output /= Null_Unbounded_String then
            T := [1 => To_String (Step.Details.Expected_Output)];
         elsif Step.File_Content /= Empty_Text then
            T := Step.File_Content;
         else
            T := Get_Text (Step.Details.File_Name);
         end if;
         return T;
      end Get_Expected;

   begin
      -- let's run the test
      for D of BBT.Tests_Builder.The_Document_List.all loop
         IO.New_Line (Level => Verbose);
         Put_Line ("Running file " & D.Name'Image, Level => Verbose);
         for F of D.Feature_List loop

            for Scen of F.Scenario_List loop
               Put_Line ("  Running scenario " & Scen.Name'Image,
                         Level => Verbose);
               Return_Code := 0;

               Step_Processing : for Step of Scen.Step_List loop

                  begin
                     case Step.Details.Kind is
                     when Run_Cmd =>
                        Run_Cmd (Cmd         => To_String (Step.Details.Cmd),
                                 Output_Name => Output_File_Name (D),
                                 Spawn_OK    => Spawn_OK,
                                 Return_Code => Return_Code);
                        if Spawn_OK then
                           Add_Success (Scen);
                           Put_Line ("  Run " & Step.Details.Cmd'Image,
                                     Level => IO.Verbose);
                        else
                           Add_Fail (Scen);
                           -- We don't want the run fail to goes unnoticed,
                           -- so an error is immediatly added
                           Put_Line ("  Unsuccessfully run " &
                                       Step.Details.Cmd'Image,
                                     Level => IO.Quiet);
                        end if;
                        if not Spawn_OK then
                           Put_Line ("Exiting scenario");
                           exit Step_Processing;
                        end if;

                     when Successfully_Run_Cmd =>
                        Run_Cmd (Cmd         => To_String (Step.Details.Cmd),
                                 Output_Name => Output_File_Name (D),
                                 Spawn_OK    => Spawn_OK,
                                 Return_Code => Return_Code);
                        if Spawn_OK and Is_Success (Return_Code) then
                           Add_Success (Scen);
                           Put_Line ("  Successfully run " &
                                       Step.Details.Cmd'Image,
                                     Level => IO.Verbose);
                        else
                           Add_Fail (Scen);
                           Put_Line ("  Unsuccessfully run " &
                                       Step.Details.Cmd'Image,
                                     Level => IO.Quiet);
                        end if;
                        if not Spawn_OK then
                           Put_Line ("Exiting scenario");
                           exit Step_Processing;
                        end if;

                     when Error_Return_Code =>
                        if not Is_Success (Return_Code) then
                           -- error expected, it's a fail
                           Add_Success (Scen);
                           Put_Line ("  Got expected error code",
                                     Level => IO.Verbose);
                        else
                           Add_Fail (Scen);
                           Put_Line
                             ("  Expected error code, got no error",
                              Level => IO.Quiet);
                        end if;

                     when No_Error_Return_Code =>
                        if Is_Success (Return_Code) then
                           Add_Success (Scen);
                           Put_Line ("  Got no error as expected",
                                     Level => IO.Verbose);
                        else
                           Add_Fail (Scen);
                           Put_Line ("  No error expected, but got one",
                                     Level => IO.Quiet);
                        end if;

                     when Output_Is =>
                        declare
                           T1 : constant Text := Get_Text (Output_File_Name (D));
                           T2 : constant Text := Get_Expected (Step);
                        begin
                           if Is_Equal (T1, T2) then
                              Add_Success (Scen);
                              Put_Line ("  Output equal to expected "
                                        & T2'Image,
                                        Level => IO.Verbose);
                           else
                              Add_Fail (Scen);
                              Put_Line ("  Output " & T1'Image,
                                        Level => IO.Quiet);
                              Put_Line ("  not equal to expected "
                                        & T2'Image,
                                        Level => IO.Quiet);
                           end if;
                        end;

                     when Output_Contains =>
                        -- example : Then the output contains `--version`
                        declare
                           T1 : constant Text := Get_Text (Output_File_Name (D));
                           T2 : constant Text := Get_Expected (Step);
                        begin
                           if Contains (T1, T2) then
                              Add_Success (Scen);
                              Put_Line ("Output contains expected "
                                        & T2'Image,
                                        Level => IO.Verbose);
                           else
                              Add_Fail (Scen);
                              Put_Line ("  Output " & T1'Image,
                                        Level => IO.Quiet);
                              Put_Line ("  does not contain expected "
                                        & T2'Image,
                                        Level => IO.Quiet);
                           end if;
                        end;

                        when File_Is   =>
                           declare
                              T1 : constant Text := Get_Text
                                (To_String (Step.Details.File_Name));
                              -- T2 : constant Text := Step.File_Content;
                              T2 : constant Text := Get_Expected (Step);
                           begin
                              if Is_Equal (T1, T2) then
                                 Add_Success (Scen);
                                 Put_Line ("  File is equal to expected " & T2'Image,
                                           Level => IO.Verbose);
                              else
                                 Add_Fail (Scen);
                                 Put_Line ("  Fail : "
                                           & To_String (Step.Details.File_Name)
                                           & " not equal to "
                                           & T2'Image,
                                           Level => IO.Quiet);
                              end if;
                           end;

                        when File_Contains   =>
                           declare
                              T1 : constant Text := Get_Text
                                (To_String (Step.Details.File_Name));
                              -- T2 : constant Text := Step.File_Content;
                              T2 : constant Text := Get_Expected (Step);

                           begin
                              if Contains (T1, T2) then
                                 Add_Success (Scen);
                                 Put_Line ("  File contains expected " & T2'Image,
                                           Level => IO.Verbose);
                              else
                                 Add_Fail (Scen);
                                 Put_Line ("  Fail : file "
                                           & To_String (Step.Details.File_Name)
                                           & " does not contain "
                                           & T2'Image,
                                           Level => IO.Quiet);
                              end if;
                           end;

                        when Existing_File =>
                           if Ada.Directories.Exists
                             (To_String (Step.Details.File_Name)) then
                              Add_Success (Scen);
                              Put_Line ("  Expected file " &
                                          Step.Details.File_Name'Image &
                                          " exists",
                                        Level => IO.Verbose);
                           else
                              Add_Fail (Scen);
                              Put_Line ("  Expected file " &
                                          Step.Details.File_Name'Image &
                                          " doesn't exists",
                                        Level => IO.Quiet);
                           end if;

                        when File_Creation =>
                           if Create_File
                             (File_Name    => Step.Details.File_Name,
                              With_Content => Step.File_Content)
                           then
                              if Settings.Is_Authorised (Verbose) then
                                 Put_Text (Step.File_Content);
                              end if;
                              -- We don't count the precondition execution as
                              -- a pass test,
                           else
                              Add_Fail (Scen);
                              -- But if it fails, we don't want that to
                              -- to goes unnoticed.
                              Put_Line ("  Unable to create file " &
                                          Step.Details.File_Name'Image,
                                        Level => IO.Quiet);
                           end if;

                        when Unknown =>
                           Add_Fail (Scen);
                           Put_Line ("Unrecognised step " & Step.Step_String'Image,
                                     Level => IO.Quiet);

                     end case;

                     --  exception
                     --     when E : others =>
                     --        Add_Fail (Scen);
                     --        Put_Line (Ada.Exceptions.Exception_Message (E),
                     --                  Level => IO.Quiet);
                     --        Put_Line ("Unkown exception while processing scenario "
                     --                  & Scen.Name'Image,
                     --                  Level => IO.Quiet);
                  end;

               end loop Step_Processing;

               case Documents.Result (Scen) is
                  when Empty =>
                     Put_Line ("  - [ ] " & To_String (D.Name) &
                                 ", Scenario """ & To_String (Scen.Name) &
                                 """ is empty, nothing tested");
                  when Successful =>
                     Put_Line ("  - [X] " & To_String (D.Name) &
                                 ", Scenario """ & To_String (Scen.Name) &
                                 """ pass");
                  when Failed =>
                     Put_Line ("  - [ ] " & To_String (D.Name) &
                                 ", Scenario """ & To_String (Scen.Name) &
                                 """ fails");
               end case;
            end loop;
         end loop;
      end loop;
   end Run_All;

end BBT.Tests_Runner;
