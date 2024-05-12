with Ada.Exceptions;
with BBT.IO;

with GNAT.OS_Lib;

with Ada.Command_Line;
with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Traceback.Symbolic;

package body BBT.Tests.Actions is

   -- --------------------------------------------------------------------------
   function Is_Success (I : Integer) return Boolean is
     (I = Integer (Ada.Command_Line.Success));
   -- Fixme: can I compare the return status of spawn with
   --        Ada.Command_Line Success or Fail?

   -- --------------------------------------------------------------------------
   function Get_Expected (Step : Step_Type) return Text is
      use type Text;
   begin
      if Step.File_Content /= Empty_Text then
         -- File content provided in code fenced lines
         return Step.File_Content;

      elsif Step.Object_String /= Null_Unbounded_String then
         if Exists (To_String (Step.Object_String))
           and then Kind (Name => To_String (Step.Object_String))
             = Ordinary_File
         then
            -- The string denotes a file
            return Get_Text (To_String (Step.Object_String));
         else
            -- The string is the content
            return [1 => To_String (Step.Object_String)];
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
   procedure Create_File (Step     :        Step_Type;
                          Scenario : in out Scenario_Type) is
      Success : Boolean;
   begin
      if Step.File_Type = Ordinary_File then
         declare
            File_Name : constant String := To_String (Step.Subject_String);
         begin
            Success := Create_File
              (File_Name    => Step.Subject_String,
               With_Content => Step.File_Content);
            Put_Step_Result
              (Step => Step,
               Success     => Exists (File_Name) and
                   Kind (File_Name) = Ordinary_File,
               --  Success_Msg => "File " & Step.Subject_String'Image & " created",
               Fail_Msg    => "Unable To Create File """ & File_Name & """",
               Loc         => Step.Location,
               Scenario    => Scenario);
         end;
         if Success and IO.Is_Authorized (IO.Verbose) then
            Put_Text (Step.File_Content);
         end if;

      elsif Step.File_Type = Directory then
         declare
            Dir_Name : constant String := To_String (Step.Subject_String);
         begin
            if not Exists (Dir_Name) then
               Create_Directory (Dir_Name);
            end if;
            Put_Step_Result
              (Step => Step,
               Success     => Exists (Dir_Name) and Kind (Dir_Name) = Directory,
               --  Success_Msg => "Dir """ & Dir_Name & """ created",
               Fail_Msg    => "Unable to create directory """ & Dir_Name & """",
               Loc         => Step.Location,
               Scenario    => Scenario);
         end;
      end if;

   exception
      when E : others =>
         Put_Error  ("Unable to create """ &
                       Step.Subject_String'Image & """" &
                       Ada.Exceptions.Exception_Message (E) &
                       GNAT.Traceback.Symbolic.Symbolic_Traceback (E),
                     Step.Location);
   end Create_File;

   -- --------------------------------------------------------------------------
   procedure Delete_File (Step     :        Step_Type;
                          Scenario : in out Scenario_Type) is
      pragma Unreferenced (Scenario);
      File : constant String := To_String (Step.Subject_String);
   begin
      if Exists (File) and Kind (File) = Ordinary_File then
         Delete_File (File);
      end if;
   end Delete_File;

   -- --------------------------------------------------------------------------
   procedure Return_Error (Last_Returned_Code :        Integer;
                           Step               :        Step_Type;
                           Scenario           : in out Scenario_Type) is
   begin
      Put_Step_Result
        (Step => Step,
         Success     => not Is_Success (Last_Returned_Code),
         --  Success_Msg => "Got expected error code (" &
         --    Last_Returned_Code'Image & ")",
         Fail_Msg    => "Expected error code, got no error",
         Loc         => Step.Location,
         Scenario    => Scenario);
   end Return_Error;

   -- --------------------------------------------------------------------------
   procedure Return_No_Error (Last_Returned_Code :        Integer;
                              Step               :        Step_Type;
                              Scenario           : in out Scenario_Type) is
   begin
      Put_Step_Result
        (Step => Step,
         Success     => Is_Success (Last_Returned_Code),
         --  Success_Msg => "Got no error as expected",
         Fail_Msg    => "No error expected, but got one (" &
           Last_Returned_Code'Image & ")",
         Loc         => Step.Location,
         Scenario    => Scenario);
   end Return_No_Error;

   -- --------------------------------------------------------------------------
   procedure Check_File_Existence (File_Name :        String;
                                   Step      :        Step_Type;
                                   Scenario  : in out Scenario_Type) is
   begin
      if File_Name /= "" and then Exists (File_Name) then
         Put_Step_Result (Step        => Step,
                          Success     => Kind (File_Name) = Step.File_Type,
                          --  Success_Msg => "Expected file " &
                          --    File_Name'Image & " exists",
                          Fail_Msg    => "File " & File_Name'Image &
                            " exists but isn't a dir/file as expected",
                          Loc         => Step.Location,
                          Scenario    => Scenario);
      else
         Put_Step_Result (Step        => Step,
                          Success     => False,
                          --  Success_Msg => "",
                          Fail_Msg    => "Expected file " &
                            File_Name'Image & " doesn't exists",
                          Loc         => Step.Location,
                          Scenario    => Scenario);
      end if;
   end Check_File_Existence;

   -- --------------------------------------------------------------------------
   procedure Check_No_File (File_Name :        String;
                            Step      :        Step_Type;
                            Scenario  : in out Scenario_Type) is
   begin
      Put_Step_Result (Step        => Step,
                       Success     => File_Name /= "" and then
                         not Exists (File_Name),
                       --  Success_Msg => "file " &
                       --    File_Name'Image & " doesn't exists",
                       Fail_Msg    => "file " &
                         File_Name'Image & " shouldn't exists",
                       Loc         => Step.Location,
                       Scenario    => Scenario);
   end Check_No_File;

   -- --------------------------------------------------------------------------

   procedure Output_Equal_To (Output    : Text;
                              Step      :        Step_Type;
                              Scenario  : in out Scenario_Type) is
      use Texts;
      T2 : constant Text := Get_Expected (Step);
   begin
      Put_Step_Result
        (Step        => Step,
         Success     => Is_Equal (Output, T2),
         --  Success_Msg => "Output equal to expected " &
         --     Text_Image (Get_Text_Head (T2, 3)'Image & "..."),
           -- Get_Text_Head (T2, 3)'Image & "...",
         Fail_Msg    => "Output " & Output'Image &
           "    not equal to expected  " & T2'Image,
         Loc         => Step.Location,
         Scenario    => Scenario);
   end Output_Equal_To;

   -- --------------------------------------------------------------------------
   procedure Output_Contains (Output    : Text;
                              Step      :        Step_Type;
                              Scenario  : in out Scenario_Type) is
      T2  : constant Text := Get_Expected (Step);
   begin
      Put_Step_Result
        (Step        => Step,
         Success     => Contains (Output, T2),
         --  Success_Msg => "Output contains expected " & T2'Image,
         Fail_Msg    => "Output " & Output'Image &
           "    does not contain expected  " & T2'Image,
         Loc         => Step.Location,
         Scenario    => Scenario);
   end Output_Contains;

   -- --------------------------------------------------------------------------
   procedure Files_Equal_To (Step     :        Step_Type;
                             Scenario : in out Scenario_Type) is
      File_Name   : constant String
        := To_String (Step.Subject_String);
      T1          : constant Text := Get_Text (File_Name);
      T2          : constant Text := Get_Expected (Step);
   begin
      Put_Step_Result
        (Step        => Step,
         Success     => Is_Equal (T1, T2),
         --  Success_Msg => "File is equal to expected " & T2'Image,
         Fail_Msg    => T1'Image & " not equal to expected " &
           T2'Image,
         Loc         => Step.Location,
         Scenario    => Scenario);
   end Files_Equal_To;

   -- --------------------------------------------------------------------------
   procedure File_Contains (Step     :        Step_Type;
                            Scenario : in out Scenario_Type) is
      File_Name : constant String := To_String (Step.Subject_String);
      T1        : constant Text   := Get_Text (File_Name);
      T2        : constant Text   := Get_Expected (Step);
   begin
      Put_Step_Result
        (Step        => Step,
         Success     => Contains (T1, T2),
         --  Success_Msg => "File contains to expected " & T2'Image,
         Fail_Msg    => "file " & To_String (Step.Subject_String) &
           " does not contain " & T2'Image,
         Loc         => Step.Location,
         Scenario    => Scenario);
   end File_Contains;

end BBT.Tests.Actions;
