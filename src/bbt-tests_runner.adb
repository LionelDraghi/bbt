with BBT.Documents;
with BBT.IO;
with BBT.Settings;      use BBT.Settings;
with BBT.Tests_Builder;

with GNAT.OS_Lib;

with Ada.Directories;       use Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Text_Utilities; use Text_Utilities;

package body BBT.Tests_Runner is

   -- --------------------------------------------------------------------------
   procedure Spawn_Cmd (Cmd         : String;
                        Output_File : String;
                        Spawn_OK    : out Boolean;
                        Return_Code : out Integer) is
      -- Spawn the Cmd under strace.
      -- OK is set to True if the spawn did it well, and if the command
      -- returns True.

      use GNAT.OS_Lib;
      Initial_Dir : constant String  := Current_Directory;
      Spawn_Arg   : constant Argument_List_Access
        := Argument_String_To_List (Cmd);

   begin
      Set_Directory (Settings.Run_Dir_Name);

      --  -- Put_Line ("Cmd :" & Cmd);
      --  for A of Spawn_Arg.all loop
      --     Put_Line ("Arg >" & A.all & "<", Level => Debug); -- , Level => Settings.Debug);
      --  end loop;
      Spawn (Program_Name => Spawn_Arg.all (1).all,
             Args         => Spawn_Arg.all (2 .. Spawn_Arg'Last),
             Success      => Spawn_OK,
             Output_File  => Output_File,
             Return_Code  => Return_Code,
             Err_To_Out   => True);
      Set_Directory (Initial_Dir);

   end Spawn_Cmd;

   -- --------------------------------------------------------------------------
   procedure Run_All is
      Spawn_OK    : Boolean := True;
      Return_Code : Integer;
      use Documents;

      -- Run_Outfile : Ada.Text_IO.File_Type;

      function Is_Success (I : Integer) return Boolean is (I = 0);
      -- Fixme: harcoded value : use Command_Line Success constant?

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
            IO.Put_Line ("unable to run """ & Cmd & """",
                            Level => IO.Quiet);
         end if;
      end Run_Cmd;

      -- -----------------------------------------------------------------------
      function Output_File_Contains (The_String  : String;
                                     In_The_File : String;
                                     Full_Line   : Boolean) return Boolean is
         Run_Outfile : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Open (Run_Outfile,
                           Ada.Text_IO.In_File,
                           In_The_File);
         while not Ada.Text_IO.End_Of_File (Run_Outfile) loop
            declare
               use Ada.Text_IO;
               L : constant String := Get_Line (Run_Outfile);
            begin
               IO.Put_Line ("=== """ & The_String & """ in " & L'Image,
                            Level => IO.Quiet);
               if (Full_Line and L = The_String) or else
                 (not Full_Line and
                    Ada.Strings.Fixed.Index (Source  => L,
                                             Pattern => The_String,
                                             From    => L'First) /= 0)
                 -- Ada.Strings.Equal_Case_Insensitive
               then
                  IO.Put_Line ("  Found """ & The_String & """ in " & L'Image,
                               Level => IO.Verbose);

                  Ada.Text_IO.Close (Run_Outfile);
                  return True;
               end if;
            end;
         end loop;
         IO.Put_Line ("Fail : " & The_String'Image &
                           ", not found in " & In_The_File,
                         Level => IO.Quiet);

         Ada.Text_IO.Close (Run_Outfile);
         return False;
      end Output_File_Contains;


   begin
      -- let's run the test
      for D of BBT.Tests_Builder.The_Document_List.all loop
         IO.New_Line (Level => Verbose);
         IO.Put_Line ("Running file " & D.Name'Image, Level => Verbose);
         for F of D.Feature_List loop

            for Scen of F.Scenario_List loop
               IO.Put_Line ("  Running scenario " & Scen.Name'Image,
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
                           IO.Put_Line ("  Run " & Step.Details.Cmd'Image,
                                        Level => IO.Verbose);
                        else
                           Add_Fail (Scen);
                           -- We don't want the run fail to goes unnoticed,
                           -- so an error is immediatly added
                           IO.Put_Line ("  Unsuccessfully run " &
                                             Step.Details.Cmd'Image,
                                           Level => IO.Quiet);
                        end if;

                     when Successfully_Run_Cmd =>
                        Run_Cmd (Cmd         => To_String (Step.Details.Cmd),
                                 Output_Name => Output_File_Name (D),
                                 Spawn_OK    => Spawn_OK,
                                 Return_Code => Return_Code);
                        if Spawn_OK and Is_Success (Return_Code) then
                           Add_Success (Scen);
                           IO.Put_Line ("  Successfully run " &
                                          Step.Details.Cmd'Image,
                                        Level => IO.Verbose);
                        else
                           Add_Fail (Scen);
                           IO.Put_Line ("  Unsuccessfully run " &
                                             Step.Details.Cmd'Image,
                                           Level => IO.Quiet);
                        end if;

                     when Error_Return_Code =>
                        if not Is_Success (Return_Code) then
                           -- error expected, it's a fail
                           Add_Success (Scen);
                           IO.Put_Line ("  Got expected error code",
                                        Level => IO.Verbose);
                        else
                           Add_Fail (Scen);
                           IO.Put_Line
                             ("  Expected error code, got no error",
                              Level => IO.Quiet);
                        end if;

                     when No_Error_Return_Code =>
                        if Is_Success (Return_Code) then
                           Add_Success (Scen);
                           IO.Put_Line ("  Got no error as expected",
                                        Level => IO.Verbose);
                        else
                           Add_Fail (Scen);
                           IO.Put_Line ("  No error expected, but got one",
                                           Level => IO.Quiet);
                        end if;

                     when Output_Is_String | Get_Output =>
                        if Output_File_Contains
                          (To_String (Step.Details.Expected_Output),
                           In_The_File => Output_File_Name (D),
                           Full_Line   => True)
                        then
                           Add_Success (Scen);
                           IO.Put_Line ("  Output equal to expected " &
                                          Step.Details.Expected_Output'Image,
                                        Level => IO.Verbose);
                        else
                           Add_Fail (Scen);
                           IO.Put_Line ("  Output not equal to expected "
                                           & Step.Details.Expected_Output'Image,
                                           Level => IO.Quiet);
                        end if;

                     when Output_Contains_String =>
                        -- example : Then the output contains `--version`
                        if Output_File_Contains
                          (To_String (Step.Details.Expected_Output),
                           In_The_File => Output_File_Name (D),
                           Full_Line   => False)
                        then
                           Add_Success (Scen);
                           IO.Put_Line ("  Output contains expected " &
                                          To_String (Step.Details.Expected_Output),
                                        Level => IO.Verbose);
                        else
                           Add_Fail (Scen);
                           IO.Put_Line ("  Output doesn't expected "
                                           & Step.Details.Expected_Output'Image,
                                           Level => IO.Quiet);
                        end if;

                     when File_Contains_String =>
                        -- example : Then `config.ini` contains `--version`
                        if Output_File_Contains
                          (To_String (Step.Details.Expected_Output),
                           In_The_File =>
                             To_String (Step.Details.File_Name),
                           Full_Line   => False)
                        then
                           Add_Success (Scen);
                           IO.Put_Line ("  File contains expected " &
                                          To_String (Step.Details.Expected_Output),
                                        Level => IO.Verbose);
                        else
                           Add_Fail (Scen);
                           IO.Put_Line ("  Fail : no existing " &
                                             Step.Details.File_Name'Image,
                                           Level => IO.Quiet);
                        end if;

                     when File_Is_String =>
                        declare
                           Identical : Boolean;
                           Diff_Index : Natural;
                        begin
                           Compare
                             (Text1 => Step.File_Content,
                              Text2 =>
                                [To_String (Step.Details.Expected_Output)],
                              Identical  => Identical,
                              Diff_Index => Diff_Index);
                           if Identical then
                              Add_Success (Scen);
                              IO.Put_Line
                                ("  File is as expected " &
                                   To_String (Step.Details.Expected_Output),
                                 Level => IO.Verbose);
                           else
                              Add_Fail (Scen);
                              IO.Put_Line ("  Fail : expected " &
                                             To_String (Step.Details.Expected_Output),
                                           Level => IO.Quiet);
                              IO.Put_Line ("         got : ",
                                           Level => IO.Quiet);
                              Put_Text_Head (Item       => Step.File_Content,
                                             Line_Count => 4);
                           end if;
                        end;
                        when Existing_File =>
                           if Ada.Directories.Exists
                             (To_String (Step.Details.File_Name)) then
                              Add_Success (Scen);
                              IO.Put_Line ("  Expected file " &
                                             Step.Details.File_Name'Image &
                                             " exists",
                                           Level => IO.Verbose);
                           else
                              Add_Fail (Scen);
                              IO.Put_Line ("  Expected file " &
                                                Step.Details.File_Name'Image &
                                                " doesn't exists",
                                              Level => IO.Quiet);
                           end if;

                        when Output_Is_File       |
                             File_Is_File         |
                             Output_Contains_File |
                             File_Contains_File   =>
                           IO.Put_Error ("  Not yet implemented");
                           Add_Fail (Scen);

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
                              IO.Put_Line ("  Unable to create file " &
                                                Step.Details.File_Name'Image,
                                              Level => IO.Quiet);
                           end if;

                        when Unknown =>
                           Add_Fail (Scen);
                           IO.Put_Line ("Unrecognised step " & Step.Text'image,
                                        Level => IO.Quiet);
                           -- Put_Line (Step'Image);
                           -- IO.New_Line;

                     end case;

                  --  exception
                  --     when E : others =>
                  --        Add_Fail (Scen);
                  --        IO.Put_Line (Ada.Exceptions.Exception_Message (E),
                  --                        Level => IO.Quiet);
                  --        IO.Put_Line ("Unkown exception while processing scenario "
                  --                        & Scen.Name'Image,
                  --                        Level => IO.Quiet);
                  end;

               end loop Step_Processing;

               case Documents.Result (Scen) is
                  when Empty =>
                     IO.Put_Line ("  - [ ] " & To_String (D.Name) &
                                    ", Scenario """ & To_String (Scen.Name) &
                                    """ is empty, nothing tested");
                     IO.New_Line;

                  when Successful =>
                     IO.Put_Line ("  - [X] " & To_String (D.Name) &
                                    ", Scenario """ & To_String (Scen.Name) &
                                    """ pass");
                     IO.New_Line;

                  when Failed =>
                     IO.Put_Line ("  - [ ] " & To_String (D.Name) &
                                    ", Scenario """ & To_String (Scen.Name) &
                                    """ fails");
                     IO.New_Line;

               end case;
            end loop;
         end loop;
      end loop;
   end Run_All;

end BBT.Tests_Runner;
