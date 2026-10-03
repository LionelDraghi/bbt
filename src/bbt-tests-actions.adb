-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Settings;
with BBT.Created_File_List;             use BBT.Created_File_List;
with BBT.Writers;                       use BBT.Writers;
with BBT.Tests.Actions.File_Operations; use BBT.Tests.Actions.File_Operations;

with Ada.Command_Line;
with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.OS_Lib;

use Ada, BBT;

package body BBT.Tests.Actions is

   -- -----------------------------------------------------------------------
   procedure Put_Debug_Line
     (Item      : String;
      Location  : IO.Location_Type    := IO.No_Location;
      Verbosity : IO.Verbosity_Levels := IO.Debug;
      Topic     : IO.Extended_Topics  := IO.Step_Actions)
      renames IO.Put_Line;
   pragma Warnings (Off, Put_Debug_Line);

   -- --------------------------------------------------------------------------
   function Is_Success (I : Integer) return Boolean is
     (I = Integer (Command_Line.Success));

   function Entry_Exists (File_Name : String) return Boolean is
     (File_Name /= "" and then Exists (File_Name));

   use type BBT.Tests.Actions.File_Operations.File_Kind;
   function File_Exists (File_Name : String) return Boolean is
     (File_Name /= ""
      and then Exists (File_Name)
      and then Kind (File_Name) = Ordinary_File);

   function Dir_Exists (File_Name : String) return Boolean is
     (File_Name /= ""
      and then Exists (File_Name)
      and then Kind (File_Name) = Directory);

   -- --------------------------------------------------------------------------
   function Get_Expected (Step : Step_Type'Class) return Text is
      use type Text;
   begin
      if Step.Data.File_Content /= Empty_Text then
         -- File content provided in code fenced lines
         Put_Debug_Line ("======= Get_Expected returning Text" & Step.Data.File_Content'Image);
         return Step.Data.File_Content;

      elsif Step.Data.Object_File_Name /= Null_Unbounded_String
        and then File_Exists (+Step.Data.Object_File_Name)
      then
         -- The string denotes a file
         declare
            T : constant Text := Get_Text (+Step.Data.Object_File_Name);
         begin
            Put_Debug_Line ("======= Get_Expected returning content of file " & Step.Data.Object_File_Name'Image);
            -- Put_Text (Item => T);
            return T;
         end;

      elsif Step.Data.Object_String /= Null_Unbounded_String then
         -- The string is the content
         Put_Debug_Line ("======= Get_Expected returning string content" & Step.Data.Object_String'Image);
         return [1 => +Step.Data.Object_String];

      else
         -- Either the provided file content was null (two consecutive code
         -- fence marks), or there is an error somewhere in the scenario.
         -- But scenario errors are supposed to be caught during scenario
         -- analysis, and the run stopped before reaching this point,
         -- unless run with "--keep_going".
         -- In both cases, returning an Empty_Text seems to be the right
         -- things to do.
         Put_Debug_Line ("======= Get_Expected returning empty Text");
         return Empty_Text;
      end if;
   end Get_Expected;

   -- --------------------------------------------------------------------------
   -- Portable dup / dup2 of the GNAT runtime (adaint.c), used to give a
   -- spawned command its own standard error file.
   function Dup (Fd : GNAT.OS_Lib.File_Descriptor)
                 return GNAT.OS_Lib.File_Descriptor;
   pragma Import (C, Dup, "__gnat_dup");
   procedure Dup2 (Old_Fd, New_Fd : GNAT.OS_Lib.File_Descriptor);
   pragma Import (C, Dup2, "__gnat_dup2");

   -- --------------------------------------------------------------------------
   Last_Return_Code : Integer := 0;
   -- Set by each Run_Cmd, read by the exit code checks of the following
   -- steps (each step is run by a separate Runner.Run_Step call).

   function Last_Exit_Code return Integer is (Last_Return_Code);

   -- --------------------------------------------------------------------------
   procedure Run_Cmd (Step              :     Step_Type'Class;
                      Cmd               :     String;
                      Output_Name       :     String;
                      Expected_Result   :     Run_Result;
                      Verbosity         :     Verbosity_Levels;
                      Spawn_OK          : out Boolean;
                      Return_Code       : out Integer;
                      Error_Output_Name :     String := "") is
      use GNAT.OS_Lib;
      -- Initial_Dir : constant String  := Current_Directory;
      Spawn_Arg      : constant Argument_List_Access
        := Argument_String_To_List (Cmd);

   begin
      Put_Debug_Line ("Run_Cmd " & Cmd & " in " & Settings.Exec_Dir &
                        ", output file = " & Output_Name);

      -- The first argument should be an executable (e.g. not a bash
      -- built-in)
      --
      -- If it is, replace it by the fully-qualified path name (spawn
      -- is implemented via execve, which on macOS doesn't
      -- understand PATH)
      Find_The_Executable_If_Any :
      declare
         Full_Path : GNAT.OS_Lib.String_Access;
      begin
         Full_Path := Locate_Exec_On_Path (Spawn_Arg.all (1).all);

         if Exists (Spawn_Arg.all (1).all) and then
           not Is_Executable_File (Spawn_Arg.all (1).all)
         then
            -- IO.Put_Line ("not exec", Verbosity => IO.Normal);
            Spawn_OK := False;
            Put_Step_Result
              (Step      => Step,
               Success   => Spawn_OK,
               Fail_Msg  => Spawn_Arg.all (1).all & " not executable",
               Loc       => Step.Location,
               Verbosity => Verbosity);
            return;

         elsif Full_Path = null then
            -- IO.Put_Line ("not found", Verbosity => IO.Normal);
            Spawn_OK := False;
            Put_Step_Result
              (Step      => Step,
               Success   => Spawn_OK,
               Fail_Msg  => Spawn_Arg.all (1).all & " not found",
               Loc       => Step.Location,
               Verbosity => Verbosity);
            return;

         else
            Put_Debug_Line
              ("Cmd " & Spawn_Arg.all (1).all & " = " & Full_Path.all);
            Free (Spawn_Arg.all (1));
            Spawn_Arg.all (1) := Full_Path;

         end if;
      end Find_The_Executable_If_Any;

      -- Removes quote on argument
      for I in 2 .. Spawn_Arg'Last loop
         -- Put_Debug_Line (">>>>>>>>>>" & Spawn_Arg.all (I).all & "<");
         declare
            Tmp : String := Spawn_Arg.all (I).all;
            I1  : constant Positive := (if Tmp (Tmp'First) = '"' then Tmp'First + 1
                                        else Tmp'First);
            I2  : constant Natural := (if Tmp (Tmp'Last) = '"' then Tmp'Last - 1
                                       else Tmp'Last);
            -- ugly and buggy
         begin
            Free (Spawn_Arg.all (I));
            Spawn_Arg.all (I) := new String'(Tmp (I1 .. I2));
         end;
         -- Put_Debug_Line ("===========" & Spawn_Arg.all (I).all & "<");
      end loop;

      if Error_Output_Name = "" then
         Spawn (Program_Name => Spawn_Arg.all (1).all,
                Args         => Spawn_Arg.all (2 .. Spawn_Arg'Last),
                Success      => Spawn_OK,
                Output_File  => Output_Name,
                Return_Code  => Return_Code,
                Err_To_Out   => True);

      else
         -- The scenario checks the error output: the child's standard
         -- error goes to its own file. Spawn leaves the child's standard
         -- error on ours when Err_To_Out is False, so ours is redirected
         -- to that file for the duration of the call.
         declare
            Err_FD   : constant File_Descriptor :=
                         Create_File (Error_Output_Name, Binary);
            Saved_FD : constant File_Descriptor := Dup (Standerr);
         begin
            Dup2 (Err_FD, Standerr);
            Spawn (Program_Name => Spawn_Arg.all (1).all,
                   Args         => Spawn_Arg.all (2 .. Spawn_Arg'Last),
                   Success      => Spawn_OK,
                   Output_File  => Output_Name,
                   Return_Code  => Return_Code,
                   Err_To_Out   => False);
            Dup2 (Saved_FD, Standerr);
            Close (Saved_FD);
            Close (Err_FD);
         end;
      end if;

      Last_Return_Code := Return_Code;

      Put_Debug_Line ("Spawn returns : Success = " & Spawn_OK'Image &
                        ", Return_Code = " & Return_Code'Image);

      if Spawn_OK and then Expected_Result = Success then
         Put_Step_Result (Step       => Step,
                           Success   => Is_Success (Return_Code),
                           Fail_Msg  => "Unsuccessfully run " &
                              Step.Data.Object_String'Image,
                           Loc       => Step.Location,
                           Verbosity => Verbosity);

      elsif Spawn_OK and then Expected_Result = Failure then
         Put_Step_Result (Step      => Step,
                          Success   => not Is_Success (Return_Code),
                          Fail_Msg  => "Successfully run " &
                            Step.Data.Object_String'Image &
                            " but expected to fail",
                          Loc       => Step.Location,
                          Verbosity => Verbosity);

      else
         -- If Expected_Result = Don't_Care, Success is only
         -- determined by Spawn_OK, not by the return code.
         Put_Step_Result (Step      => Step,
                          Success   => Spawn_OK,
                          Fail_Msg  => "Couldn't run " & Cmd,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      end if;

   end Run_Cmd;

   -- --------------------------------------------------------------------------
   procedure Erase_And_Create (Step         : Step_Type'Class;
                               Verbosity    :     Verbosity_Levels) is
      File_Name  : constant String := To_String (Step.Data.Subject_String);
      Parent_Dir : constant String :=
                     Directories.Containing_Directory (File_Name);
   begin
      Put_Debug_Line ("Create_New " & File_Name);
      Created_File_List.Add (File_Name);
      -- Should be deleted at the end, even if pre-existing.

      case Step.Data.File_Type is
         when Ordinary_File =>
            if Exists (File_Name) then
               Put_Debug_Line (Item => "Deleting existing " & File_Name);
               Delete_File (File_Name);
            elsif not Exists (Parent_Dir) then
               Put_Debug_Line (Item => "Creating missing dir " & Parent_Dir);
               Directories.Create_Path (Parent_Dir);
               -- Create all missing intermediate directories
            end if;
            Create_File (File_Name    => File_Name,
                         With_Content => Get_Expected (Step),
                         Executable   => Step.Data.Executable_File);
            Put_Step_Result (Step      => Step,
                             Success   => File_Exists (File_Name),
                             Fail_Msg  => "File " & File_Name'Image &
                               " creation failed",
                             Loc       => Step.Location,
                             Verbosity => Verbosity);
         when Directory =>
            if not Exists (File_Name) then
               Directories.Create_Path (File_Name);
            end if;
            Put_Step_Result (Step      => Step,
                             Success   => Dir_Exists (File_Name),
                             Fail_Msg  => "Couldn't create directory " &
                               File_Name'Image,
                             Loc       => Step.Location,
                             Verbosity => Verbosity);
         when others =>
            -- don't mess around with special files!
            null;

      end case;
   end Erase_And_Create;

   -- --------------------------------------------------------------------------
   procedure Create_If_None (Step      : Step_Type'Class;
                             Verbosity : Verbosity_Levels) is
      File_Name : constant String := To_String (Step.Data.Subject_String);
   begin
      Put_Debug_Line ("Create_New " & File_Name);
      case Step.Data.File_Type is
         when Ordinary_File =>
            if not Exists (File_Name) then
               Created_File_List.Add (File_Name);
               -- should be deleted at the end only if created here
               Create_File (File_Name    => File_Name,
                            With_Content => Get_Expected (Step),
                            Executable   => Step.Data.Executable_File);

            end if;
            Put_Step_Result (Step     => Step,
                             Success  => File_Exists (File_Name),
                             Fail_Msg => "File " & File_Name'Image &
                               " creation failed",
                             Loc       => Step.Location,
                             Verbosity => Verbosity);
         when Directory =>
            if not Exists (File_Name) then
               Created_File_List.Add (File_Name);
               -- should be deleted at the end only if created here
               Directories.Create_Path (File_Name);
            end if;
            Put_Step_Result (Step     => Step,
                             Success  => Dir_Exists (File_Name),
                             Fail_Msg => "Couldn't create directory " &
                               File_Name'Image,
                             Loc       => Step.Location,
                             Verbosity => Verbosity);
         when others =>
            -- don't mess around with special files!
            null;
      end case;
   end Create_If_None;

   -- --------------------------------------------------------------------------
   procedure Return_Error (Last_Returned_Code : Integer;
                           Step               : Step_Type'Class;
                           Verbosity          : Verbosity_Levels) is
   begin
      Put_Debug_Line ("Return_Error " & Last_Returned_Code'Image);
      Put_Step_Result (Step     => Step,
                       Success  => not Is_Success (Last_Returned_Code),
                       Fail_Msg => "Expected error code, got no error",
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Return_Error;

   -- --------------------------------------------------------------------------
   procedure Return_No_Error (Last_Returned_Code : Integer;
                              Step               : Step_Type'Class;
                              Verbosity          : Verbosity_Levels) is
   begin
      Put_Debug_Line ("Return_No_Error " & Last_Returned_Code'Image);
      Put_Step_Result (Step     => Step,
                       Success  => Is_Success (Last_Returned_Code),
                       Fail_Msg => "No error expected, but got one (" &
                         Last_Returned_Code'Image & ")",
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Return_No_Error;

   -- --------------------------------------------------------------------------
   procedure Check_File_Existence (File_Name : String;
                                   Step      : Step_Type'Class;
                                   Verbosity : Verbosity_Levels) is
   begin
      Put_Debug_Line ("Check_File_Existence " & File_Name);
      if Entry_Exists (File_Name) then
         Put_Step_Result (Step     => Step,
                          Success  => Kind (File_Name) = Ordinary_File,
                          Fail_Msg => File_Name'Image &
                            " exists but its a dir and not a file as expected",
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         Put_Step_Result (Step     => Step,
                          Success  => False,
                          Fail_Msg => "Expected file " &
                            File_Name'Image & " doesn't exists",
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      end if;
   end Check_File_Existence;

   -- --------------------------------------------------------------------------
   procedure Check_Dir_Existence (Dir_Name : String;
                                  Step      : Step_Type'Class;
                                  Verbosity : Verbosity_Levels) is
   begin
      Put_Debug_Line ("Check_Dir_Existence " & Dir_Name);
      if Entry_Exists (Dir_Name) then
         Put_Step_Result (Step     => Step,
                          Success  => Kind (Dir_Name) = Directory,
                          Fail_Msg => "File " & Dir_Name'Image &
                            " exists but isn't a dir as expected",
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         Put_Step_Result (Step     => Step,
                          Success  => False,
                          Fail_Msg => "Expected dir " &
                            Dir_Name'Image & " doesn't exists in Exec_Dir "
                          & Settings.Exec_Dir,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      end if;
   end Check_Dir_Existence;

   -- --------------------------------------------------------------------------
   procedure Check_No_File (File_Name : String;
                            Step      : Step_Type'Class;
                            Verbosity : Verbosity_Levels) is
   begin
      Put_Debug_Line ("Check_No_File " & File_Name);
      Put_Step_Result (Step     => Step,
                       Success  => not File_Exists (File_Name),
                       Fail_Msg => "file " &
                         File_Name'Image & " shouldn't exists",
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Check_No_File;

   -- --------------------------------------------------------------------------
   procedure Check_No_Dir (Dir_Name : String;
                           Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels) is
   begin
      Put_Debug_Line ("Check_No_Dir " & Dir_Name);
      Put_Step_Result (Step     => Step,
                       Success  => not Dir_Exists (Dir_Name),
                       Fail_Msg => "dir " &
                         Dir_Name'Image & " shouldn't exists",
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Check_No_Dir;

   -- --------------------------------------------------------------------------
   procedure Check_No_Output (Output : Text;
                              Step      : Step_Type'Class;
                              Verbosity : Verbosity_Levels) is
      use Texts;
   begin
      Put_Step_Result (Step     => Step,
                       Success  => Output = Empty_Text,
                       Fail_Msg => String'("output not null : " & Output'Image),
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Check_No_Output;

   -- --------------------------------------------------------------------------
   procedure Setup_No_File (Step      : Step_Type'Class;
                            Verbosity : Verbosity_Levels) is
      File_Name : constant String :=
                    +Step.Data.Subject_String & (+Step.Data.Object_File_Name);
   begin
      Put_Debug_Line ("Setup_No_File " & File_Name);
      Delete_File (File_Name);
      Put_Step_Result (Step     => Step,
                       Success  => not File_Exists (File_Name),
                       Fail_Msg => "file " & File_Name'Image & " not deleted",
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Setup_No_File;

   -- --------------------------------------------------------------------------
   procedure Setup_No_Dir (Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels) is
      Dir_Name : constant String :=
                   +Step.Data.Subject_String & (+Step.Data.Object_File_Name);
   begin
      Put_Debug_Line ("Setup_No_Dir " & Dir_Name);
      Delete_Tree (Dir_Name);
      Put_Step_Result (Step     => Step,
                       Success  => not Dir_Exists (Dir_Name),
                       Fail_Msg => "dir " & Dir_Name'Image & " not deleted",
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Setup_No_Dir;

   -- --------------------------------------------------------------------------
   procedure Output_Is (Output    : Text;
                        Step      : Step_Type'Class;
                        Verbosity : Verbosity_Levels) is
      use Texts;
      T2 : constant Text := Get_Expected (Step);
   begin
      -- Put_Debug_Line ("++++++++++ Output_Is Step = " & Step'Image);
      -- Put_Debug_Line ("++++++++++ dir = " & Settings.Launch_Directory);
      Put_Debug_Line ("++++++++++ Output = ");
      -- Put_Text (Item => Output);
      Put_Debug_Line ("++++++++++ T2 = ");
      -- Put_Text (Item => T2);
      declare
         Success : constant Boolean :=
           Is_Equal (Output, T2,
                     Case_Insensitive   => Settings.Ignore_Casing,
                     Ignore_Blanks      => Settings.Ignore_Whitespaces,
                     Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                     Sort_Texts         => Step.Data.Ignore_Order);
         -- First line of Msg is the error message,
         -- then comes the side-by-side comparison of expected and actual.
         -- It is computed only on error.
         Msg : Text := (if Success then Empty_Text
                        else Side_By_Side (T2, Output,
                                           Case_Insensitive   => Settings.Ignore_Casing,
                                           Ignore_Whitespaces => Settings.Ignore_Whitespaces));
      begin
         if not Success then
            Msg.Prepend ("Output not equal to expected:");
         end if;
         Put_Step_Result (Step     => Step,
                          Success  => Success,
                          Fail_Msg => Msg,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      end;
   end Output_Is;

   -- --------------------------------------------------------------------------
   procedure Output_Contains (Output    : Text;
                              Step      : Step_Type'Class;
                              Verbosity : Verbosity_Levels) is
      T2  : constant Text := Get_Expected (Step);
   begin
      Put_Debug_Line ("++++++++++ Output_Contains Step = " & Step'Image);
      Put_Debug_Line ("++++++++++ dir = " & Settings.Launch_Directory);
      Put_Step_Result (Step     => Step,
                       Success  => Contains
                         (Output, T2,
                          Case_Insensitive   => Settings.Ignore_Casing,
                          Ignore_Whitespaces => Settings.Ignore_Whitespaces,
                          Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                          Sort_Texts         => Step.Data.Ignore_Order),
                       Fail_Msg => "Output:  " & Code_Fenced_Image (Output) &
                         "does not contain expected:  " &
                         Code_Fenced_Image (T2),
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Output_Contains;

   -- --------------------------------------------------------------------------
   procedure Output_Does_Not_Contain (Output : Text;
                                      Step      : Step_Type'Class;
                                      Verbosity : Verbosity_Levels) is
      T2  : constant Text := Get_Expected (Step);
   begin
      Put_Debug_Line ("Output_Does_Not_Contain ");
      declare
         Success : constant Boolean :=
           not Contains (Output, T2,
                         Case_Insensitive   => Settings.Ignore_Casing,
                         Ignore_Whitespaces => Settings.Ignore_Whitespaces,
                         Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                         Sort_Texts         => Step.Data.Ignore_Order);
         -- First line of Msg gives the position of the intruder,
         -- then come the intruder lines. It is computed only on error.
         Msg : Text := Empty_Text;
      begin
         if not Success then
            declare
               I : constant Line_Index'Base :=
                 Index_Of (Output, T2,
                           Case_Insensitive   => Settings.Ignore_Casing,
                           Ignore_Whitespaces => Settings.Ignore_Whitespaces);
               -- Last line of the intruder, limited by the end of Output
               Last : constant Line_Index :=
                 Line_Index'Min (I + Line_Index (T2.Length) - 1,
                                 Output.Last_Index);
            begin
               Msg.Append (String'("Output contains unexpected at line" &
                                   I'Image & ":"));
               for J in I .. Last loop
                  Msg.Append (Output (J));
               end loop;
            end;
         end if;
         Put_Step_Result (Step     => Step,
                          Success  => Success,
                          Fail_Msg => Msg,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      end;
   end Output_Does_Not_Contain;

   -- --------------------------------------------------------------------------
   procedure Output_Matches (Output : Text;
                             Step      : Step_Type'Class;
                             Verbosity : Verbosity_Levels) is
      Regexp : constant String := Get_Expected (Step) (1);
   begin
      Put_Debug_Line ("Output_Matches ");
      Put_Step_Result (Step     => Step,
                       Success  => Text_Utilities.Matches (Output, Regexp),
                       Fail_Msg => "Output:  " & Code_Fenced_Image (Output) &
                         "does not match expected:  " & Regexp,
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Output_Matches;

   -- --------------------------------------------------------------------------
   procedure Output_Does_Not_Match (Output : Text;
                                    Step      : Step_Type'Class;
                                    Verbosity : Verbosity_Levels) is
      Regexp : constant String := Get_Expected (Step) (1);
   begin
      Put_Debug_Line ("Output_Does_Not_Match ");
      Put_Step_Result (Step     => Step,
                       Success  => not Text_Utilities.Matches (Output, Regexp),
                       Fail_Msg => "Output:  " & Code_Fenced_Image (Output) &
                         "match unexpected:  " & Regexp,
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Output_Does_Not_Match;

   -- --------------------------------------------------------------------------
   procedure File_Matches (Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels) is
      Regexp    : constant String := Get_Expected (Step) (1);
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
   begin
      Put_Debug_Line ("File_Matches ");
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         Put_Step_Result (Step     => Step,
                          Success  => Text_Utilities.Matches (T1, Regexp),
                          Fail_Msg => "File:  " & File_Name &
                            " does not match expected:  " & Regexp,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end File_Matches;

   -- --------------------------------------------------------------------------
   procedure File_Does_Not_Match (Step      : Step_Type'Class;
                                  Verbosity : Verbosity_Levels) is
      Regexp    : constant String := Get_Expected (Step) (1);
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
   begin
      Put_Debug_Line ("File_Does_Not_Match ");
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         Put_Step_Result (Step     => Step,
                          Success  => not Text_Utilities.Matches (T1, Regexp),
                          Fail_Msg => "File:  " & File_Name &
                            " should not match:  " & Regexp,
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end File_Does_Not_Match;

   -- --------------------------------------------------------------------------
   procedure Files_Is (Step      : Step_Type'Class;
                       Verbosity : Verbosity_Levels) is
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
      T2        : constant Text   := Get_Expected (Step);
   begin
      Put_Debug_Line ("Files_Is " & File_Name &
                        " T1 = " & T1'Image &
                        " T2 = " & T2'Image);
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         declare
            Success : constant Boolean :=
              Is_Equal (T1, T2,
                        Case_Insensitive   => Settings.Ignore_Casing,
                        Ignore_Blanks      => Settings.Ignore_Whitespaces,
                        Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                        Sort_Texts         => Step.Data.Ignore_Order);
            -- First line of Msg is the error message,
            -- then comes the side-by-side comparison of expected and actual.
            -- It is computed only on error.
            Msg : Text := (if Success then Empty_Text
                           else Side_By_Side (T2, T1,
                                              Case_Insensitive   => Settings.Ignore_Casing,
                                              Ignore_Whitespaces => Settings.Ignore_Whitespaces));
         begin
            if not Success then
               Msg.Prepend (File_Name & " not equal to expected:");
            end if;
            Put_Step_Result (Step     => Step,
                             Success  => Success,
                             Fail_Msg => Msg,
                             Loc       => Step.Location,
                             Verbosity => Verbosity);
         end;
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end Files_Is;

   -- --------------------------------------------------------------------------
   procedure Files_Is_Not (Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels) is
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
      T2        : constant Text   := Get_Expected (Step);
   begin
      Put_Debug_Line ("Files_Is_Not " & File_Name);
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         Put_Step_Result (Step     => Step,
                          Success  => not Is_Equal
                            (T1, T2,
                             Case_Insensitive   => Settings.Ignore_Casing,
                             Ignore_Blanks      => Settings.Ignore_Whitespaces,
                             Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                             Sort_Texts         => Step.Data.Ignore_Order),
                          Fail_Msg => File_Name & " expected to be different from " &
                            Code_Fenced_Image (T2),
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end Files_Is_Not;

   -- --------------------------------------------------------------------------
   procedure File_Contains (Step      : Step_Type'Class;
                            Verbosity : Verbosity_Levels) is
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
      T2        : constant Text   := Get_Expected (Step);
   begin
      Put_Debug_Line ("File_Contains " & File_Name &
                        " T1 = " & T1'Image &
                        " T2 = " & T2'Image);
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         Put_Step_Result (Step     => Step,
                          Success  => Contains
                            (T1, T2,
                             Case_Insensitive   => Settings.Ignore_Casing,
                             Ignore_Whitespaces => Settings.Ignore_Whitespaces,
                             Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                             Sort_Texts         => Step.Data.Ignore_Order),
                          Fail_Msg => File_Name &
                            " does not contain expected:  " &
                            Code_Fenced_Image (T2),
                          Loc       => Step.Location,
                          Verbosity => Verbosity);
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end File_Contains;

   -- --------------------------------------------------------------------------
   procedure File_Does_Not_Contain (Step      : Step_Type'Class;
                                    Verbosity : Verbosity_Levels) is
      File_Name : constant String := +Step.Data.Subject_String;
      T1        : Text;
      T2        : constant Text   := Get_Expected (Step);
   begin
      Put_Debug_Line ("File_Does_Not_Contain " & File_Name);
      if Exists (File_Name) then
         T1 := Get_Text (File_Name);
         declare
            Success : constant Boolean :=
              not Contains (T1, T2,
                            Case_Insensitive   => Settings.Ignore_Casing,
                            Ignore_Whitespaces => Settings.Ignore_Whitespaces,
                            Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                            Sort_Texts         => Step.Data.Ignore_Order);
            -- First line of Msg gives the position of the intruder,
            -- then come the intruder lines. It is computed only on error.
            Msg : Text := Empty_Text;
         begin
            if not Success then
               declare
                  I : constant Line_Index'Base :=
                    Index_Of (T1, T2,
                              Case_Insensitive   => Settings.Ignore_Casing,
                              Ignore_Whitespaces => Settings.Ignore_Whitespaces);
                  -- Last line of the intruder, limited by the end of the file
                  Last : constant Line_Index :=
                    Line_Index'Min (I + Line_Index (T2.Length) - 1,
                                    T1.Last_Index);
               begin
                  Msg.Append (String'(File_Name &
                                      " contains unexpected at line" &
                                      I'Image & ":"));
                  for J in I .. Last loop
                     Msg.Append (T1 (J));
                  end loop;
               end;
            end if;
            Put_Step_Result (Step     => Step,
                             Success  => Success,
                             Fail_Msg => Msg,
                             Loc       => Step.Location,
                             Verbosity => Verbosity);
         end;
      else
         IO.Put_Error ("No file " & File_Name, Step.Location);
      end if;
   end File_Does_Not_Contain;

   -- --------------------------------------------------------------------------
   procedure Exit_Code_Is (Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels) is
      Expected : constant Integer := Integer'Value (+Step.Data.Object_String);
      -- Checked in Validate_Step_State
   begin
      Put_Debug_Line ("Exit_Code_Is " & Expected'Image & ", last ="
                      & Last_Return_Code'Image);
      Put_Step_Result (Step      => Step,
                       Success   => Last_Return_Code = Expected,
                       Fail_Msg  => "Expected exit code" & Expected'Image
                                    & ", got" & Last_Return_Code'Image,
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Exit_Code_Is;

   -- --------------------------------------------------------------------------
   -- Environment variables set or unset by the steps of the current
   -- scenario, with the value (or the absence) they had before, so that
   -- Restore_Environment can give them back.
   type Saved_Variable (Name_Length, Value_Length : Natural) is record
      Name    : String (1 .. Name_Length);
      Present : Boolean;
      Value   : String (1 .. Value_Length);
   end record;

   package Saved_Variable_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Saved_Variable);
   Saved_Variables : Saved_Variable_Lists.Vector;

   procedure Remember (Name : String) is
   begin
      for V of Saved_Variables loop
         if V.Name = Name then
            return; -- keep the value from before the first change
         end if;
      end loop;
      if Environment_Variables.Exists (Name) then
         declare
            Old : constant String := Environment_Variables.Value (Name);
         begin
            Saved_Variables.Append
              (Saved_Variable'(Name_Length  => Name'Length,
                Value_Length => Old'Length,
                Name         => Name,
                Present      => True,
                Value        => Old));
         end;
      else
         Saved_Variables.Append
           (Saved_Variable'(Name_Length  => Name'Length,
             Value_Length => 0,
             Name         => Name,
             Present      => False,
             Value        => ""));
      end if;
   end Remember;

   -- --------------------------------------------------------------------------
   procedure Set_Env_Var (Step      : Step_Type'Class;
                          Verbosity : Verbosity_Levels) is
      Name  : constant String := +Step.Data.Subject_String;
      Value : constant String := +Step.Data.Object_String;
      OK    : Boolean := True;
   begin
      Put_Debug_Line ("Set_Env_Var " & Name & " = " & Value);
      Remember (Name);
      begin
         Environment_Variables.Set (Name, Value);
      exception
         when Constraint_Error | Program_Error =>
            -- Illegal name (empty or with '=') or rejected by the OS
            OK := False;
      end;
      Put_Step_Result (Step      => Step,
                       Success   => OK,
                       Fail_Msg  => "Unable to set environment variable "
                                    & Name'Image,
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Set_Env_Var;

   -- --------------------------------------------------------------------------
   procedure Unset_Env_Var (Step      : Step_Type'Class;
                            Verbosity : Verbosity_Levels) is
      Name : constant String := +Step.Data.Subject_String;
      OK   : Boolean := True;
   begin
      Put_Debug_Line ("Unset_Env_Var " & Name);
      Remember (Name);
      begin
         Environment_Variables.Clear (Name);
      exception
         when Constraint_Error | Program_Error =>
            OK := False;
      end;
      Put_Step_Result (Step      => Step,
                       Success   => OK,
                       Fail_Msg  => "Unable to unset environment variable "
                                    & Name'Image,
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Unset_Env_Var;

   -- --------------------------------------------------------------------------
   procedure Restore_Environment is
   begin
      for V of Saved_Variables loop
         if V.Present then
            Environment_Variables.Set (V.Name, V.Value);
         else
            Environment_Variables.Clear (V.Name);
         end if;
      end loop;
      Saved_Variables.Clear;
   end Restore_Environment;

end BBT.Tests.Actions;
