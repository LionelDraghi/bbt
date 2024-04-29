-- SPDX-License-Identifier: Apache-2.0

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure UUT is
   -- uut (Unit under Test) is a fake exe that just interact
   -- with the system, by, as an example, creating
   -- or modifying file, to test bbt.

   -- -------------------------------------------------------------------------
   procedure Put_Usage is
   begin
      Put_Line ("uut -h -v file_name create|read|[append ""Text""]");
      New_Line;
      Put_Line ("uut simulate an ""under test unit"" that can create, or read"
                & " a file, or append a text to a file,");
      Put_Line ("and can exit with an error status");
      New_Line;
      Put_Line ("-v : display aversion string");
      Put_Line ("-h display this message");
      Put_Line ("--return_failure : set the retrun status to Failure");
      Put_Line ("--return_success : set the retrun status to Success");
   end Put_Usage;

   use Ada.Strings.Unbounded;

   -- -------------------------------------------------------------------------
   Arg_Counter : Positive := 1;
   File        : File_Type;
   File_Name   : Unbounded_String;

begin
   if Ada.Command_Line.Argument_Count < 1 then
      Put_Usage;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   else
      File_Name := To_Unbounded_String (Ada.Command_Line.Argument (Arg_Counter));
      Arg_Counter := Arg_Counter + 1;

      while Arg_Counter <= Ada.Command_Line.Argument_Count loop
         declare
            Opt : constant String := Ada.Command_Line.Argument (Arg_Counter);

         begin
            if Opt = "create" then
               Create (File => File,
                       Mode => Out_File,
                       Name => To_String (File_Name));

            elsif Opt = "read" then
               Open (File => File,
                     Mode => In_File,
                     Name => To_String (File_Name));
               while not End_Of_File (File) loop
                  Put_Line (Get_Line (File));
               end loop;

            elsif Opt = "append" then
               Open (File => File,
                     Mode => Append_File,
                     Name => To_String (File_Name));
               Arg_Counter := Arg_Counter + 1;
               Put_Line (File, Ada.Command_Line.Argument (Arg_Counter));

            elsif Opt = "-v" then
               Put_Line ("version 1.0");

            elsif Opt = "-h" then
               Put_Usage;

            elsif Opt = "--return_failure" then
               Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

            elsif Opt = "--return_success" then
               Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);

            else
              Put_Line ("Unknown Option " & Opt);
              Put_Usage;
              Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

            end if;

         end;
         Arg_Counter := Arg_Counter + 1;
      end loop;

      Close (File);
   end if;

exception
   when others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

end UUT;
