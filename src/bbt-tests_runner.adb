with BBT.Documents;
with BBT.IO;
with BBT.Settings;      use BBT.Settings;
with BBT.Tests_Builder;

with GNAT.OS_Lib;

with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;


package body BBT.Tests_Runner is

   -- --------------------------------------------------------------------------
   -- IO renamed with "Spawn" as Topic
   procedure Put_Line  (Item  : String;
                        File  : String  := "";
                        Line  : Integer := 0;
                        Level : Print_Out_Level := Normal;
                        Topic : Extended_Topics := Spawn) renames IO.Put_Line;

   -- --------------------------------------------------------------------------
   procedure Spawn_Cmd (Cmd         : String;
                        Output_File : String;
                        Spawn_OK    : out Boolean;
                        Return_Code : out Integer) is
      -- Spawn the Cmd under strace.
      -- OK is set to True if the spawn did it well, and if the command
      -- returns True

      use GNAT.OS_Lib;
      Initial_Dir : constant String  := Current_Directory;
      Spawn_Arg   : constant Argument_List_Access
          := Argument_String_To_List (Cmd); -- Cmd);

   begin
      -- Put_Line ("cd " & Settings.Run_Dir_Name, Level => Verbose);
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

      --  Put_Line ("Program_Name = " & Spawn_Arg.all (1).all'Image);
      --  for I in Positive'Succ (Spawn_Arg.all'First) .. Spawn_Arg.all'Last loop
      --     Put_Line ("Arg [" & I'Image & "]     = " & Spawn_Arg.all (2).all'Image);
      --  end loop;
      --  Put_Line ("Success      = " & OK'Image);
      --  Put_Line ("Return_Code  = " & Integer'Image (Return_Code));


      Set_Directory (Initial_Dir);
      -- Fixme : ensure turning to Initial_Dir even in case of exception?

   end Spawn_Cmd;

   -- --------------------------------------------------------------------------
   procedure Run_All is
      Spawn_OK : Boolean := True;
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
         Put_Line ("Running " & Cmd'Image, Level => Verbose);
         Spawn_Cmd (Cmd         => Cmd,
                    Output_File => Output_Name,
                    Spawn_OK    => Spawn_OK,
                    Return_Code => Return_Code);
         if not Spawn_OK then
            IO.Put_Error ("unable to run """ & Cmd & """");
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
               if (Full_Line and L = The_String) or else
                 (not Full_Line and
                    Ada.Strings.Fixed.Index (Source  => L,
                                             Pattern => The_String,
                                             From    => L'First) /= 0)
               -- Ada.Strings.Equal_Case_Insensitive
               then
                  IO.Put_Line ("    Found """ & The_String & """ in " & L'Image,
                               Level => IO.Verbose);

                  Ada.Text_IO.Close (Run_Outfile);
                  return True;
               end if;
            end;
         end loop;
         IO.Put_Error ("Fail : " & The_String'Image &
                         ", not found in " & In_The_File);

         Ada.Text_IO.Close (Run_Outfile);
         return False;
      end Output_File_Contains;

   begin
      -- let's run the test
      for D of BBT.Tests_Builder.The_Document_List.all loop
         IO.New_Line;
         IO.Put_Line ("Running file " & D.Name'Image);
         for F of D.Feature_List loop

            for Scen of F.Scenario_List loop
               IO.Put_Line ("  Running scenario " & Scen.Name'Image);
               Return_Code := 0;
               Step_Processing : for Step of Scen.Step_List loop

                  case Step.Details.Kind is
                     when Run_Cmd =>
                        Run_Cmd (Cmd         => To_String (Step.Details.Cmd),
                                 Output_Name => Output_File_Name (D),
                                 Spawn_OK    => Spawn_OK,
                                 Return_Code => Return_Code);
                        if Spawn_OK then
                           Scen.Successful_Step_Count := @ + 1;
                        else
                           Scen.Failed_Step_Count := @ + 1;
                           -- We don't want the run fail to goes unnoticed,
                           -- so an error is immediatly added
                        end if;

                     when Successfully_Run_Cmd =>
                        Run_Cmd (Cmd         => To_String (Step.Details.Cmd),
                                 Output_Name => Output_File_Name (D),
                                 Spawn_OK    => Spawn_OK,
                                 Return_Code => Return_Code);
                        if Spawn_OK and Is_Success (Return_Code) then
                           Add_Success (Scen);
                        else
                           Add_Fail (Scen);
                        end if;

                     when Error_Return_Code =>
                        if not Is_Success (Return_Code) then
                           -- error expected, it's a fail
                           Add_Success (Scen);
                        else
                           Add_Fail (Scen);
                        end if;

                     when No_Error_Return_Code =>
                        if Is_Success (Return_Code) then
                           Add_Success (Scen);
                        else
                           Add_Fail (Scen);
                        end if;

                     when Output_Is | Get_Output =>
                        if Output_File_Contains
                          (To_String (Step.Details.Expected_Output),
                           In_The_File => Output_File_Name (D),
                           Full_Line   => True)
                        then
                           Add_Success (Scen);
                        else
                           Add_Fail (Scen);
                        end if;

                     when Output_Contains =>
                        -- example : Then the output contains `--version`
                        if Output_File_Contains
                          (To_String (Step.Details.Expected_Output),
                           In_The_File => Output_File_Name (D),
                           Full_Line   => False)
                        then
                           Add_Success (Scen);
                        else
                           Add_Fail (Scen);
                        end if;

                     when File_Contains =>
                        -- example : Then `config.ini` contains `--version`
                        if Output_File_Contains
                          (To_String (Step.Details.Expected_Output),
                           In_The_File =>
                             To_String (Step.Details.File_Name),
                           Full_Line   => False)
                        then
                           Add_Success (Scen);
                        else
                           Add_Fail (Scen);
                        end if;

                     when File_Is =>
                        -- Fenced_Code     : Texts.Vector;
                        IO.Put_Error (Step.Details.Kind'Image &
                                        " not yet implemented");

                     when Unknown =>
                        IO.Put_Error (Step.Details.Kind'Image &
                                    " not yet implemented");
                        -- Put_Line (Step'Image);
                        -- IO.New_Line;

                  end case;

               end loop Step_Processing;

               case Documents.Result (Scen) is
                  when Empty =>
                     IO.Put_Line ("    " & To_String (D.Name) &
                                    ", Scenario """ & To_String (Scen.Name) &
                                    """ is empty, nothing tested",
                                  Level => Normal);
                  when Successful =>
                     IO.Put_Line ("    " & To_String (D.Name) &
                                    ", Scenario """ & To_String (Scen.Name) &
                                    """ pass",
                                  Level => Normal);

                  when Failed =>
                     IO.Put_Line ("    " & To_String (D.Name) &
                                    ", Scenario """ & To_String (Scen.Name) &
                                    """ fails",
                                  Level => Normal);
               end case;
            end loop;
         end loop;
      end loop;
   end Run_All;

end BBT.Tests_Runner;
