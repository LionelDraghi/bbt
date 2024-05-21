with BBT.IO;
with BBT.Settings;

with GNAT.OS_Lib;

with Ada.Command_Line;
with Ada.Directories;       use Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Traceback.Symbolic;

package body BBT.Tests.Actions is

   --  function "+" (Name : File_Name) return String is (To_String (Name));
   --  function "+" (Name : String) return File_Name is
   --    (File_Name'(To_Unbounded_String (Name)));

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

   procedure Delete_Directory (Dir_Name, Prompt : String);

   procedure Delete_File (File_Name, Prompt : String) is
      C : Character;
   begin
      if Dir_Exists (File_Name) then
         -- Error case : the dir is in fact a file
         Delete_Directory (File_Name, Prompt);

      elsif File_Exists (File_Name) then

         if BBT.Settings.Assume_Yes then
            Delete_File (File_Name);
            Ada.Text_IO.Put_Line ("Deleting " & File_Name);
         else
            loop
               if Prompt /= "" then
                  Ada.Text_IO.Put_Line (Prompt);
               end if;
               Ada.Text_IO.Put_Line ("Delete existing " & File_Name & "? [yn]");
               Ada.Text_IO.Get_Immediate (C);
               Ada.Text_IO.Flush;
               case C is
               when 'y' =>
                  Ada.Text_IO.Put_Line ("Deleting " & File_Name);
                  Delete_File (File_Name);
                  return;
               when 'n'    => return;
               when others => null;
               end case;
            end loop;
         end if;

      else
         -- either there is no File_Name, or it is a special file
         null;

      end if;
   end Delete_File;

   procedure Delete_Directory (Dir_Name, Prompt : String) is
      C : Character;
   begin
      if File_Exists (Dir_Name) then
         -- Error case : the dir is in fact a file
         Delete_File (Dir_Name, "");

      elsif Dir_Exists (Dir_Name) then

         if BBT.Settings.Assume_Yes then
            Delete_Tree (Dir_Name);
            Ada.Text_IO.Put_Line ("Deleting recursively " & Dir_Name);

         else
            loop
               if Prompt /= "" then
                  Ada.Text_IO.Put_Line (Prompt);
               end if;
               Ada.Text_IO.Put_Line ("Delete existing " & Dir_Name & "? [yn]");
               Ada.Text_IO.Get_Immediate (C);
               Ada.Text_IO.Flush;
               case C is
               when 'y' =>
                  Ada.Text_IO.Put_Line ("Deleting recursively " & Dir_Name);
                  Delete_Tree (Dir_Name);
                  return;
               when 'n'    => return;
               when others => null;
               end case;
            end loop;

         end if;
      else
         -- either there is no Dir_Name, or it is a special file
         null;

      end if;
   end Delete_Directory;

   -- --------------------------------------------------------------------------
   function Get_Expected (Step : Step_Type) return Text is
      use type Text;
   begin
      if Step.File_Content /= Empty_Text then
         -- File content provided in code fenced lines
         return Step.File_Content;

      elsif Step.Object_String /= Null_Unbounded_String then
         if File_Exists (+Step.Object_String) then
            -- The string denotes a file
            return Get_Text (+Step.Object_String);
         else
            -- The string is the content
            return [1 => +Step.Object_String];
         end if;
      else
         IO.Put_Error ("No file or string given", Step.Location);
         return Empty_Text;
      end if;
   end Get_Expected;

   -- --------------------------------------------------------------------------
   procedure Run_Cmd (Cmd         : String;
                      Output_Name : String;
                      Spawn_OK    : out Boolean;
                      Return_Code : out Integer) is
      use GNAT.OS_Lib;
      -- Initial_Dir : constant String  := Current_Directory;
      Spawn_Arg      : constant Argument_List_Access
        := Argument_String_To_List (Cmd);
   begin
      IO.Put_Line ("Run_Cmd " & Cmd, Verbosity => IO.Debug);
      -- Set_Directory (Settings.Run_Dir_Name);

      --  for A of Spawn_Arg.all loop
      --     --     Put_Line ("Arg >" & A.all & "<", Verbosity => Debug);
      --     --                                    , Verbosity => Settings.Debug);
      --     --  end loop;
      Spawn (Program_Name => Spawn_Arg.all (1).all,
             Args         => Spawn_Arg.all (2 .. Spawn_Arg'Last),
             Success      => Spawn_OK,
             Output_File  => Output_Name,
             Return_Code  => Return_Code,
             Err_To_Out   => True);
      -- Set_Directory (Initial_Dir);
   end Run_Cmd;

   -- --------------------------------------------------------------------------
   procedure Create_If_None (Step : Step_Type) is
      Success   : Boolean;
      File_Name : constant String := To_String (Step.Subject_String);
   begin
      IO.Put_Line ("Create_If_None " & File_Name, Verbosity => Debug);
      if Step.File_Type = Ordinary_File then
         Success := Create_File
           (File_Name    => Step.Subject_String,
            With_Content => Step.File_Content);
         Put_Step_Result (Step     => Step,
                          Success  => File_Exists (File_Name),
                          Fail_Msg => "Unable To Create File " &
                            File_Name'Image,
                          Loc      => Step.Location);
         if Success and IO.Is_Authorized (IO.Verbose) then
            Put_Text (Step.File_Content);
         end if;

      elsif Step.File_Type = Directory then
         if not Entry_Exists (File_Name) then
            Create_Directory (File_Name);
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
                            Dir_Name'Image & " doesn't exists",
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
   procedure Delete_File (Step : Step_Type) is
      File_Name : constant String :=
                    +Step.Subject_String & (+Step.Object_String);
   begin
      IO.Put_Line ("Delete_File " & File_Name, Verbosity => Debug);
      Delete_File (File_Name,
                   Prompt => "> " & Image
                     (Step.Location) & (+Step.Step_String));
      Put_Step_Result (Step     => Step,
                       Success  => not File_Exists (File_Name),
                       Fail_Msg => "file " & File_Name'Image &
                         " not deleted",
                       Loc      => Step.Location);
   end Delete_File;

   -- --------------------------------------------------------------------------
   procedure Delete_Dir (Step : Step_Type) is
      Dir_Name : constant String :=
                   +Step.Subject_String & (+Step.Object_String);
   begin
      IO.Put_Line ("Delete_Dir " & Dir_Name, Verbosity => Debug);
      Delete_Directory (Dir_Name,
                        Prompt => "> " & Image
                          (Step.Location) & (+Step.Step_String));
      Put_Step_Result (Step     => Step,
                       Success  => not Dir_Exists (Dir_Name),
                       Fail_Msg => "dir " & Dir_Name'Image &
                         " not deleted",
                       Loc      => Step.Location);
   end Delete_Dir;

   -- --------------------------------------------------------------------------
   procedure Output_Equal_To (Output : Text;
                              Step   : Step_Type) is
      use Texts;
      T2 : constant Text := Get_Expected (Step);
   begin
      IO.Put_Line ("Output_Equal_To ", Verbosity => Debug);
      Put_Step_Result (Step     => Step,
                       Success  => Is_Equal (Output, T2),
                       Fail_Msg => "Output " & Output'Image &
                         "    not equal to expected  " & T2'Image,
                       Loc      => Step.Location);
   end Output_Equal_To;

   -- --------------------------------------------------------------------------
   procedure Output_Contains (Output : Text;
                              Step   : Step_Type) is
      T2  : constant Text := Get_Expected (Step);
   begin
      IO.Put_Line ("Output_Contains ", Verbosity => Debug);
      Put_Step_Result (Step     => Step,
                       Success  => Contains (Output, T2),
                       Fail_Msg => "Output " & Output'Image &
                         "    does not contain expected  " & T2'Image,
                       Loc      => Step.Location);
   end Output_Contains;

   -- --------------------------------------------------------------------------
   procedure Files_Is (Step : Step_Type) is
      File_Name : constant String := +Step.Subject_String;
      T1        : constant Text   := Get_Text (File_Name);
      T2        : constant Text   := Get_Expected (Step);
   begin
      IO.Put_Line ("Files_Is " & File_Name, Verbosity => Debug);
      Put_Step_Result (Step     => Step,
                       Success  => Is_Equal (T1, T2),
                       Fail_Msg => T1'Image &
                         " not equal to expected  " & T2'Image,
                       Loc      => Step.Location);
   end Files_Is;

   -- --------------------------------------------------------------------------
   procedure File_Contains (Step : Step_Type) is
      File_Name : constant String := +Step.Subject_String;
      T1        : constant Text   := Get_Text (File_Name);
      T2        : constant Text   := Get_Expected (Step);
   begin
      IO.Put_Line ("File_Contains " & File_Name, Verbosity => Debug);
      Put_Step_Result (Step     => Step,
                       Success  => Contains (T1, T2),
                       Fail_Msg => "file " & To_String (Step.Subject_String) &
                         " does not contain  " & T2'Image,
                       Loc      => Step.Location);
   end File_Contains;

   -- --------------------------------------------------------------------------
   procedure Create_New (Step : Step_Type) is
      File_Name : constant String := To_String (Step.Subject_String);
   begin
      IO.Put_Line ("Create_New " & File_Name, Verbosity => Debug);
      case Step.File_Type is
         when Ordinary_File =>
            Delete_File (File_Name);
            Put_Text (Get_Expected (Step), File_Name);
            Put_Step_Result (Step     => Step,
                             Success  => File_Exists (File_Name),
                             Fail_Msg => "File " & File_Name'Image &
                               " creation failed",
                             Loc      => Step.Location);
         when Directory =>
            Delete_Directory (File_Name,
                              Prompt => "> " & Image
                                (Step.Location) & (+Step.Step_String));
            Create_Path (File_Name);
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

end BBT.Tests.Actions;
