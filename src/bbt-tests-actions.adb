-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO;
with BBT.Settings;
with BBT.Created_File_List;             use BBT.Created_File_List;
with BBT.Tests.Actions.File_Operations; use BBT.Tests.Actions.File_Operations;

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
-- no direct with of Ada.Directories or Ada.Text_IO here

with GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;

package body BBT.Tests.Actions is

   --  function "+" (Name : File_Name) return String is (To_String (Name));
   --  function "+" (Name : String) return File_Name is
   --    (File_Name'(To_Unbounded_String (Name)));

   use type BBT.Tests.Actions.File_Operations.File_Kind;

   -- --------------------------------------------------------------------------
   function Is_Success (I : Integer) return Boolean is
     (I = Integer (Ada.Command_Line.Success));
   -- Fixme: can I compare the return status of spawn with
   --        Ada.Command_Line Success or Fail?

   function Entry_Exists (File_Name : String) return Boolean is
     (File_Name /= "" and then Exists (File_Name));

   function File_Exists (File_Name : String) return Boolean is
     (File_Name /= ""
      and then Exists (File_Name)
      and then Kind (File_Name) = Ordinary_File);

   function Dir_Exists (File_Name : String) return Boolean is
     (File_Name /= ""
      and then Exists (File_Name)
      and then Kind (File_Name) = Directory);

   -- --------------------------------------------------------------------------
   function Get_Expected (Step : Step_Type) return Text is
      use type Text;
   begin
      if Step.File_Content /= Empty_Text then
         -- File content provided in code fenced lines
         return Step.File_Content;

      elsif Step.Object_File_Name /= Null_Unbounded_String
        and then File_Exists (+Step.Object_File_Name)
      then
         -- The string denotes a file
         return Get_Text (+Step.Object_File_Name);

      elsif Step.Object_String /= Null_Unbounded_String then
         -- The string is the content
         return [1 => +Step.Object_String];

      else
         -- Either the provided file content was null (two consecutive code
         -- fence marks), or there is an error somewhere in the scenario.
         -- But scenario errors are supposed to be caught during scenario
         -- analysis, and the run stopped before reaching this point,
         -- unless run with "--keep_going".
         -- In both cases, returning an Empty_Text seems to be the right
         -- things to do.
         return Empty_Text;
      end if;
   end Get_Expected;

   -- --------------------------------------------------------------------------
   procedure Run_Cmd (Step         :     Step_Type;
                      Cmd          :     String;
                      Output_Name  :     String;
                      Check_Result :     Boolean;
                      Spawn_OK     : out Boolean;
                      Return_Code  : out Integer) is
      use GNAT.OS_Lib;
      -- Initial_Dir : constant String  := Current_Directory;
      Spawn_Arg      : constant Argument_List_Access
        := Argument_String_To_List (Cmd);

   begin
      IO.Put_Line ("Run_Cmd " & Cmd & " in " & Settings.Exec_Dir &
                     ", output file = " & Output_Name,
                   Verbosity => IO.Debug);

      -- The first argument should be an executable (e.g. not a bash
      -- built-in)
      --
      -- If it is, replace it by the fully-qualified path name (spawn
      -- is implemented via execve, which _ on macOS - doesn't
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
            Put_Step_Result (Step     => Step,
                             Success  => Spawn_OK,
                             Fail_Msg => Spawn_Arg.all (1).all & " not executable",
                             Loc      => Step.Location);
            return;

         elsif Full_Path = null then
            -- IO.Put_Line ("not found", Verbosity => IO.Normal);
            Spawn_OK := False;
            Put_Step_Result (Step     => Step,
                             Success  => Spawn_OK,
                             Fail_Msg => Spawn_Arg.all (1).all & " not found",
                             Loc      => Step.Location);
            return;

         else
            IO.Put_Line ("Cmd " & Spawn_Arg.all (1).all & " = " & Full_Path.all,
                         Verbosity => IO.Debug);
            Free (Spawn_Arg.all (1));
            Spawn_Arg.all (1) := Full_Path;

         end if;
      end Find_The_Executable_If_Any;

      -- Removes quote on argument
      for I in 2 .. Spawn_Arg'Last loop
         -- IO.Put_Line (">>>>>>>>>>" & Spawn_Arg.all (I).all & "<", Verbosity => IO.Debug);
         declare
            Tmp : String := Spawn_Arg.all (I).all;
            I1  : constant Positive := (if Tmp (Tmp'First) = '"' then Tmp'First + 1
                                        else Tmp'First);
            I2  : constant Natural := (if Tmp (Tmp'Last) = '"' then Tmp'Last - 1
                                       else Tmp'Last);
            -- uggly and buggy
         begin
            Free (Spawn_Arg.all (I));
            Spawn_Arg.all (I) := new String'(Tmp (I1 .. I2));
         end;
         -- IO.Put_Line ("===========" & Spawn_Arg.all (I).all & "<", Verbosity => IO.Debug);
      end loop;

      Spawn (Program_Name => Spawn_Arg.all (1).all,
             Args         => Spawn_Arg.all (2 .. Spawn_Arg'Last),
             Success      => Spawn_OK,
             Output_File  => Output_Name,
             Return_Code  => Return_Code,
             Err_To_Out   => True);

      IO.Put_Line ("Spawn returns : Success = " & Spawn_OK'Image &
                     ", Return_Code = " & Return_Code'Image,
                   Verbosity => IO.Debug);

      Put_Step_Result (Step     => Step,
                       Success  => Spawn_OK,
                       Fail_Msg => "Couldn't run " & Cmd,
                       Loc      => Step.Location);
      if Spawn_OK and then Check_Result then
         Put_Step_Result (Step     => Step,
                          Success  => Is_Success (Return_Code),
                          Fail_Msg => "Unsuccessfully run " &
                            Step.Object_String'Image,
                          Loc      => Step.Location);
      end if;

   end Run_Cmd;

   -- --------------------------------------------------------------------------
   procedure Create_If_None (Step : Step_Type) is
      Success   : Boolean;
      File_Name : constant String := +Step.Object_File_Name;
   begin
      IO.Put_Line ("Create_If_None " & File_Name, Verbosity => Debug);
      if Step.File_Type = Ordinary_File then
         Success := Text_Utilities.Create_File
           (File_Name    => Step.Subject_String,
            With_Content => Step.File_Content);
         Created_File_List.Add (File_Name);
         Put_Step_Result (Step     => Step,
                          Success  => File_Exists (File_Name),
                          Fail_Msg => "Unable To Create File " & File_Name,
                          Loc      => Step.Location);
         if Success and IO.Is_Authorized (IO.Verbose) then
            Put_Text (Step.File_Content);
         end if;

      elsif Step.File_Type = Directory then
         if not Entry_Exists (File_Name) then
            Create_Path (File_Name);
            Created_File_List.Add (File_Name);
         end if;
         Put_Step_Result (Step     => Step,
                          Success  => Dir_Exists (File_Name),
                          Fail_Msg => "Unable to create directory " &
                            File_Name'Image,
                          Loc      => Step.Location);
      end if;

   exception
      when E : others =>
         Put_Error  ("Unable to create """ &
                       Step.Subject_String'Image & """" &
                       Ada.Exceptions.Exception_Message (E) &
                       GNAT.Traceback.Symbolic.Symbolic_Traceback (E),
                     Step.Location);
   end Create_If_None;

   -- --------------------------------------------------------------------------
   procedure Erase_And_Create (Step : Step_Type) is
      File_Name : constant String := To_String (Step.Subject_String);
      File      : File_Type;
   begin
      IO.Put_Line ("Create_New " & File_Name, Verbosity => Debug);
      case Step.File_Type is
         when Ordinary_File =>
            if Exists (File_Name) then
               Delete_File (File_Name);
            end if;
            Create (File, Name => File_Name);
            Created_File_List.Add (File_Name);
            Put_Text (File, Get_Expected (Step));
            Close (File);
            Put_Step_Result (Step     => Step,
                             Success  => File_Exists (File_Name),
                             Fail_Msg => "File " & File_Name'Image &
                               " creation failed",
                             Loc      => Step.Location);
         when Directory =>
            Delete_Tree (File_Name);
            Create_Path (File_Name);
            Created_File_List.Add (File_Name);
            Put_Step_Result (Step     => Step,
                             Success  => Dir_Exists (File_Name),
                             Fail_Msg => "Couldn't create directory " &
                               File_Name'Image,
                             Loc      => Step.Location);
         when others =>
            -- don't mess around with special files!
            null;
      end case;
   end Erase_And_Create;

   -- --------------------------------------------------------------------------
   procedure Create_New (Step : Step_Type) is
      File_Name : constant String := To_String (Step.Subject_String);
      File      : File_Type;
   begin
      IO.Put_Line ("Create_New " & File_Name, Verbosity => Debug);
      case Step.File_Type is
         when Ordinary_File =>
            if Exists (File_Name) then
               Delete_File (File_Name);
            end if;
            Create (File, Name => File_Name);
            Created_File_List.Add (File_Name);
            Put_Text (File, Get_Expected (Step));
            Close (File);
            Put_Step_Result (Step     => Step,
                             Success  => File_Exists (File_Name),
                             Fail_Msg => "File " & File_Name'Image &
                               " creation failed",
                             Loc      => Step.Location);
         when Directory =>
            if Exists (File_Name) then
               Delete_Tree (File_Name);
            end if;
            Create_Path (File_Name);
            Created_File_List.Add (File_Name);
            Put_Step_Result (Step     => Step,
                             Success  => Dir_Exists (File_Name),
                             Fail_Msg => "Couldn't create directory " &
                               File_Name'Image,
                             Loc      => Step.Location);
         when others =>
            -- don't mess around with special files!
            null;
      end case;
   end Create_New;

   -- --------------------------------------------------------------------------
   procedure Return_Error (Last_Returned_Code : Integer;
                           Step               : Step_Type) is
   begin
      IO.Put_Line ("Return_Error " & Last_Returned_Code'Image,
                   Verbosity => Debug);
      Put_Step_Result (Step     => Step,
                       Success  => not Is_Success (Last_Returned_Code),
                       Fail_Msg => "Expected error code, got no error",
                       Loc      => Step.Location);
   end Return_Error;

   -- --------------------------------------------------------------------------
   procedure Return_No_Error (Last_Returned_Code : Integer;
                              Step               : Step_Type) is
   begin
      IO.Put_Line ("Return_No_Error " & Last_Returned_Code'Image,
                   Verbosity => Debug);
      Put_Step_Result (Step     => Step,
                       Success  => Is_Success (Last_Returned_Code),
                       Fail_Msg => "No error expected, but got one (" &
                         Last_Returned_Code'Image & ")",
                       Loc      => Step.Location);
   end Return_No_Error;

   -- --------------------------------------------------------------------------
   procedure Check_File_Existence (File_Name : String;
                                   Step      : Step_Type) is
   begin
      IO.Put_Line ("Check_File_Existence " & File_Name, Verbosity => Debug);
      if Entry_Exists (File_Name) then
         Put_Step_Result (Step     => Step,
                          Success  => Kind (File_Name) = Ordinary_File,
                          Fail_Msg => File_Name'Image &
                            " exists but its a dir and not a file as expected",
                          Loc      => Step.Location);
      else
         Put_Step_Result (Step     => Step,
                          Success  => False,
                          Fail_Msg => "Expected file " &
                            File_Name'Image & " doesn't exists",
                          Loc      => Step.Location);
      end if;
   end Check_File_Existence;

   -- --------------------------------------------------------------------------
   procedure Check_Dir_Existence (Dir_Name : String;
                                  Step     : Step_Type) is
   begin
      IO.Put_Line ("Check_Dir_Existence " & Dir_Name, Verbosity => Debug);
      if Entry_Exists (Dir_Name) then
         Put_Step_Result (Step     => Step,
                          Success  => Kind (Dir_Name) = Directory,
                          Fail_Msg => "File " & Dir_Name'Image &
                            " exists but isn't a dir as expected",
                          Loc      => Step.Location);
      else
         Put_Step_Result (Step     => Step,
                          Success  => False,
                          Fail_Msg => "Expected dir " &
                            Dir_Name'Image & " doesn't exists in Exec_Dir "
                            & Settings.Exec_Dir,
                          Loc      => Step.Location);
      end if;
   end Check_Dir_Existence;

   -- --------------------------------------------------------------------------
   procedure Check_No_File (File_Name : String;
                            Step      : Step_Type) is
   begin
      IO.Put_Line ("Check_No_File " & File_Name, Verbosity => Debug);
      Put_Step_Result (Step     => Step,
                       Success  => not File_Exists (File_Name),
                       Fail_Msg => "file " &
                         File_Name'Image & " shouldn't exists",
                       Loc      => Step.Location);
   end Check_No_File;

   -- --------------------------------------------------------------------------
   procedure Check_No_Dir (Dir_Name : String;
                           Step     : Step_Type) is
   begin
      IO.Put_Line ("Check_No_Dir " & Dir_Name, Verbosity => Debug);
      Put_Step_Result (Step     => Step,
                       Success  => not Dir_Exists (Dir_Name),
                       Fail_Msg => "dir " &
                         Dir_Name'Image & " shouldn't exists",
                       Loc      => Step.Location);
   end Check_No_Dir;

   -- --------------------------------------------------------------------------
   procedure Check_No_Output (Output : Text;
                              Step   : Step_Type) is
      use Texts;
   begin
      Put_Step_Result (Step     => Step,
                       Success  => Output = Empty_Text,
                       Fail_Msg => "output not null : " & Output'Image,
                       Loc      => Step.Location);
   end Check_No_Output;

   -- --------------------------------------------------------------------------
   procedure Setup_No_File (Step : Step_Type) is
      File_Name : constant String :=
                    +Step.Subject_String & (+Step.Object_File_Name);
   begin
      IO.Put_Line ("Setup_No_File " & File_Name, Verbosity => Debug);
      Delete_File (File_Name);
      Put_Step_Result (Step     => Step,
                       Success  => not File_Exists (File_Name),
                       Fail_Msg => "file " & File_Name'Image & " not deleted",
                       Loc      => Step.Location);
   end Setup_No_File;

   -- --------------------------------------------------------------------------
   procedure Setup_No_Dir (Step : Step_Type) is
      Dir_Name : constant String :=
                   +Step.Subject_String & (+Step.Object_File_Name);
   begin
      IO.Put_Line ("Setup_No_Dir " & Dir_Name, Verbosity => Debug);
      Delete_Tree (Dir_Name);
      Put_Step_Result (Step     => Step,
                       Success  => not Dir_Exists (Dir_Name),
                       Fail_Msg => "dir " & Dir_Name'Image & " not deleted",
                       Loc      => Step.Location);
   end Setup_No_Dir;

   -- --------------------------------------------------------------------------
   procedure Output_Is (Output : Text;
                        Step   : Step_Type) is
      use Texts;
      T2 : constant Text := Get_Expected (Step);
   begin
      IO.Put_Line ("Output_Equal_To ", Verbosity => Debug);
      Put_Step_Result (Step     => Step,
                       Success  => Is_Equal (Output, T2,
                         Sort_Texts => Step.Ignore_Order),
                       Fail_Msg => "Output:  " & Code_Fenced_Image (Output) &
                         "not equal to expected:  " & Code_Fenced_Image (T2),
                       Loc      => Step.Location);
   end Output_Is;

   -- --------------------------------------------------------------------------
   procedure Output_Contains (Output : Text;
                              Step   : Step_Type) is
      T2  : constant Text := Get_Expected (Step);
   begin
      IO.Put_Line ("Output_Contains ", Verbosity => Debug);
      Put_Step_Result (Step     => Step,
                       Success  => Contains (Output, T2,
                         Sort_Texts => Step.Ignore_Order),
                       Fail_Msg => "Output:  " & Code_Fenced_Image (Output) &
                         "does not contain expected:  " &
                         Code_Fenced_Image (T2),
                       Loc      => Step.Location);
   end Output_Contains;

   -- --------------------------------------------------------------------------
   procedure Output_Does_Not_Contain (Output : Text;
                                      Step   : Step_Type) is
      T2  : constant Text := Get_Expected (Step);
   begin
      IO.Put_Line ("Output_Does_Not_Contain ", Verbosity => Debug);
      Put_Step_Result (Step     => Step,
                       Success  => not Contains (Output, T2,
                         Sort_Texts => Step.Ignore_Order),
                       Fail_Msg => "Output:  " & Code_Fenced_Image (Output) &
                         "contains unexpected:  " & Code_Fenced_Image (T2),
                       Loc      => Step.Location);
   end Output_Does_Not_Contain;

   -- --------------------------------------------------------------------------
   procedure Output_Matches (Output : Text;
                             Step   : Step_Type) is
      Regexp : constant String := Get_Expected (Step)(1);
   begin
      IO.Put_Line ("Output_Matches ", Verbosity => Debug);
      Put_Step_Result (Step     => Step,
                       Success  => Text_Utilities.Matches (Output, Regexp),
                       -- , Line, Matches),
                       Fail_Msg => "Output:  " & Code_Fenced_Image (Output) &
                         "does not match expected:  " & Regexp,
                       Loc      => Step.Location);
   end Output_Matches;

   -- --------------------------------------------------------------------------
   procedure Output_Does_Not_Match (Output : Text;
                                    Step   : Step_Type) is
      Regexp : constant String := Get_Expected (Step) (1);
   begin
      IO.Put_Line ("Output_Does_Not_Match ", Verbosity => Debug);
      Put_Step_Result (Step     => Step,
                       Success  => not Text_Utilities.Matches (Output, Regexp),
                       --, Line, Matches),
                       Fail_Msg => "Output:  " & Code_Fenced_Image (Output) &
                         "match unexpected:  " & Regexp,
                       Loc      => Step.Location);
   end Output_Does_Not_Match;

   -- --------------------------------------------------------------------------
   procedure Files_Is (Step : Step_Type) is
      File_Name : constant String := +Step.Subject_String;
      T1        : constant Text   := Get_Text (File_Name);
      T2        : constant Text   := Get_Expected (Step);
   begin
      IO.Put_Line ("Files_Is " & File_Name, Verbosity => Debug);
      Put_Step_Result (Step     => Step,
                       Success  => Is_Equal (T1, T2,
                         Sort_Texts => Step.Ignore_Order),
                       Fail_Msg =>  Code_Fenced_Image (T1) &
                         "not equal to expected:  " & Code_Fenced_Image (T2),
                       Loc      => Step.Location);
   end Files_Is;

   -- --------------------------------------------------------------------------
   procedure Files_Is_Not (Step : Step_Type) is
      File_Name : constant String := +Step.Subject_String;
      T1        : constant Text   := Get_Text (File_Name);
      T2        : constant Text   := Get_Expected (Step);
   begin
      IO.Put_Line ("Files_Is_Not " & File_Name, Verbosity => Debug);
      Put_Step_Result (Step     => Step,
                       Success  => not Is_Equal (T1, T2,
                         Sort_Texts => Step.Ignore_Order),
                       Fail_Msg => "file is equal to expected " &
                         Code_Fenced_Image (T1),
                       Loc      => Step.Location);
   end Files_Is_Not;

   -- --------------------------------------------------------------------------
   procedure File_Contains (Step : Step_Type) is
      File_Name : constant String := +Step.Subject_String;
      T1        : constant Text   := Get_Text (File_Name);
      T2        : constant Text   := Get_Expected (Step);
   begin
      IO.Put_Line ("File_Contains " & File_Name, Verbosity => Debug);
      Put_Step_Result (Step     => Step,
                       Success  => Contains (T1, T2,
                         Sort_Texts => Step.Ignore_Order),
                       Fail_Msg => "file " & To_String (Step.Subject_String) &
                         " does not contain expected:  " &
                         Code_Fenced_Image (T2) &
                         "but: " & Code_Fenced_Image (T1),
                       Loc      => Step.Location);
   end File_Contains;

   -- --------------------------------------------------------------------------
   procedure File_Does_Not_Contain (Step : Step_Type) is
      File_Name : constant String := +Step.Subject_String;
      T1        : constant Text   := Get_Text (File_Name);
      T2        : constant Text   := Get_Expected (Step);
   begin
      IO.Put_Line ("File_Does_Not_Contain " & File_Name, Verbosity => Debug);
      Put_Step_Result (Step     => Step,
                       Success  => not Contains (T1, T2,
                         Sort_Texts => Step.Ignore_Order),
                       Fail_Msg => "file " & To_String (Step.Subject_String) &
                         Code_Fenced_Image (T1) &
                         "contain unexpected:  " & Code_Fenced_Image (T2),
                       Loc      => Step.Location);
   end File_Does_Not_Contain;

end BBT.Tests.Actions;
