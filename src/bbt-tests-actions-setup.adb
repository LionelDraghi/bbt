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

with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

use Ada;

package body BBT.Tests.Actions.Setup is

   procedure Put_Debug_Line
     (Item      : String;
      Location  : IO.Location_Type    := IO.No_Location;
      Verbosity : IO.Verbosity_Levels := IO.Debug;
      Topic     : IO.Extended_Topics  := IO.Step_Actions)
      renames IO.Put_Line;
   pragma Warnings (Off, Put_Debug_Line);
   -- --------------------------------------------------------------------------
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
            --  The new keyword means created from scratch: an existing
            --  tree is erased, after user confirmation, and the directory
            --  is recreated empty. When the user refuses the erasing, the
            --  step fails, as the existing tree is not a fresh start.
            declare
               Existed : constant Boolean := Exists (File_Name);
               Erased  : Boolean         := not Existed;
            begin
               if Existed then
                  Put_Debug_Line (Item => "Deleting existing " & File_Name);
                  Delete_Tree (File_Name);
                  Erased := not Exists (File_Name);
               end if;
               Directories.Create_Path (File_Name);
               Put_Step_Result (Step      => Step,
                                Success   => Erased
                                             and then Dir_Exists (File_Name),
                                Fail_Msg  => (if Erased
                                              then "Couldn't create directory "
                                                & File_Name'Image
                                              else "dir " & File_Name'Image
                                                & " not deleted"),
                                Loc       => Step.Location,
                                Verbosity => Verbosity);
            end;
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
               Put_Step_Result (Step     => Step,
                                Success  => File_Exists (File_Name),
                                Fail_Msg => "File " & File_Name'Image &
                                  " creation failed",
                                Loc       => Step.Location,
                                Verbosity => Verbosity);

            elsif Is_Equal (Get_Text (File_Name), Get_Expected (Step),
                            Case_Insensitive   => Settings.Ignore_Casing,
                            Ignore_Blanks      => Settings.Ignore_Whitespaces,
                            Ignore_Blank_Lines => Settings.Ignore_Blank_Lines,
                            Sort_Texts         => Step.Data.Ignore_Order)
            then
               --  The existing file already has the expected content,
               --  in the current match mode: nothing to do.
               Put_Step_Result (Step     => Step,
                                Success  => True,
                                Fail_Msg => "File " & File_Name'Image &
                                  " creation failed",
                                Loc       => Step.Location,
                                Verbosity => Verbosity);

            elsif Confirm_Overwrite (File_Name) then
               Created_File_List.Add (File_Name);
               --  The previous content is lost anyway: the file is
               --  deleted at the end, even if pre-existing.
               Create_File (File_Name    => File_Name,
                            With_Content => Get_Expected (Step),
                            Executable   => Step.Data.Executable_File);
               Put_Step_Result (Step     => Step,
                                Success  => File_Exists (File_Name),
                                Fail_Msg => "File " & File_Name'Image &
                                  " creation failed",
                                Loc       => Step.Location,
                                Verbosity => Verbosity);
            else
               Put_Step_Result (Step     => Step,
                                Success  => False,
                                Fail_Msg => "file " & File_Name'Image &
                                  " not overwritten",
                                Loc       => Step.Location,
                                Verbosity => Verbosity);
            end if;
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


end BBT.Tests.Actions.Setup;
