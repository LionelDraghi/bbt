with BBT.IO;            -- use BBT.IO;
with BBT.Settings;      use BBT.Settings;
with BBT.Tests_Builder;

with GNAT.OS_Lib;

with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with BBT.Documents;


package body BBT.Tests_Runner is

   -- --------------------------------------------------------------------------
   -- IO renamed with "Spawn" as Topic
   procedure Put_Line  (Item  : String;
                        File  : String  := "";
                        Line  : Integer := 0;
                        Level : Print_Out_Level := Normal;
                        Topic : Extended_Topics := Spawn) renames IO.Put_Line;

   -- --------------------------------------------------------------------------
   procedure Run (Cmd   :     String;
                  OK    : out Boolean) is
      -- Spawn the Cmd under strace.
      -- OK is set to True if the spawn did it well.

      use GNAT.OS_Lib;
      -- Debug       : constant Boolean := False;
      -- Prefix      : constant String  := "";
      --  Opt         : constant String  := Settings.Shell_Opt
      --                  & File_Utilities.Escape (Settings.Strace_Cmd
      --                                           & Settings.Strace_Outfile_Name
      --                                           & " " & (+Cmd));
      Initial_Dir : constant String  := Current_Directory;
      Spawn_Arg   : constant Argument_List_Access
          := Argument_String_To_List (Cmd); -- Cmd);

   begin
      -- Put_Line ("cd " & Settings.Run_Dir_Name, Level => Verbose);
      Set_Directory (Settings.Run_Dir_Name);

      -- Put_Line ("Cmd :" & Cmd);
      for A of Spawn_Arg.all loop
         Put_Line ("Arg >" & A.all & "<"); -- , Level => Settings.Debug);
      end loop;

      Spawn (Program_Name => Spawn_Arg.all (1).all,
             Args         => Spawn_Arg.all (2 .. Spawn_Arg'Last),
             Success      => OK);
      if not OK then
         IO.Put_Error (Msg => "Spawn failed for " & Cmd);
      end if;

      Set_Directory (Initial_Dir);
      -- Fixme : ensure turning to Initial_Dir even in case of exception?

   end Run;

   -- --------------------------------------------------------------------------
   procedure Run_All is
      Run_OK : Boolean;

      procedure Run_Step (Step : access Documents.Step_Type;
                          OK   : out Boolean) is
         Cmd : constant String := To_String (Step.Details.Cmd);
      begin
         -- Put_Line ("Details " & Step.all'Image);
         if Settings.Dry_Run then
            -- don't run, just print the command
            Put_Line ("""" & Cmd & """");
            OK := True;
         else
            -- Run the command
            Put_Line ("""" & Cmd & """", Level => Verbose);
            Run (Cmd, OK);
         end if;

         if not OK and not Settings.Keep_Going then
            return;
         end if;
      end Run_Step;

      use Documents;

   begin
      -- let's run the test
      for D of BBT.Tests_Builder.The_Document_List.all loop
         for F of D.Feature_List loop
            for Scen of F.Scenario_List loop
               Put_Line ("Running " & Scen'Image,
                         Level => Verbose);
               for Step of Scen.Step_List loop
                  case Step.Details.Kind is
                     when Run_Cmd =>
                        Put_Line ("Running " & Step.Details.Cmd'Image, Level => Verbose);
                     Run_Step (Step'Access, Run_OK);

                  when Error_Return_Code =>
                     if Run_OK then
                        -- error expected, it's a fail
                        Scen.Failed_Step_Count     := @ + 1;
                     else
                        Scen.Successful_Step_Count := @ + 1;
                     end if;

                  when No_Error_Return_Code =>
                     if Run_OK then
                        Scen.Successful_Step_Count := @ + 1;
                     else
                        Scen.Failed_Step_Count     := @ + 1;
                     end if;

                  when Std_Output | Unknown =>
                        Put_Line (Step.Details.Kind'Image &
                                    " not yet implemented");
                        Put_Line (Step'Image);
                        IO.New_Line;

                  end case;

               end loop;
            end loop;
         end loop;
      end loop;
   end Run_All;

end BBT.Tests_Runner;
