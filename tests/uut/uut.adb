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
      Put_Line ("uut -h -v [create|read file_name] [append ""Text"" file_name]");
      Put_Line ("   uut simulate an ""under test unit"" that can create,");
      Put_Line ("   read, or append a text to a file,");
      Put_Line ("Options:");
      Put_Line ("   -v | --version         : display a version string");
      Put_Line ("   -h | --help");
      Put_Line ("      or no command line  : display this message");
      --  Put_Line ("   -rf | --return_failure : set the return status to Failure");
      --  Put_Line ("   -rs | --return_success : set the return status to Success");
      Put_Line ("Return code:");
      Put_Line ("  Return code is set to 1 when :");
      Put_Line ("  - there is a command line error (unknown option for example)");
      Put_Line ("  - there is a file error (unable to open the given file, for example)");
      Put_Line ("  Return code is set to 0 otherwise.");

   end Put_Usage;

   use Ada.Strings.Unbounded;
   use Ada.Command_Line;

   -- -------------------------------------------------------------------------
   Arg_Counter : Positive := 1;
   File        : File_Type;
   File_Name   : Unbounded_String;


begin
   Set_Exit_Status (Success);

   if Argument_Count < 1 then
      Put_Usage;

   else
      while Arg_Counter <= Argument_Count loop
         declare
            Opt : constant String := Argument (Arg_Counter);

         begin
            if Opt = "create" then
               Arg_Counter := Arg_Counter + 1;
               File_Name := To_Unbounded_String (Argument (Arg_Counter));
               Create (File => File,
                       Mode => Out_File,
                       Name => To_String (File_Name));

            elsif Opt = "read" then
               Arg_Counter := Arg_Counter + 1;
               File_Name := To_Unbounded_String (Argument (Arg_Counter));
               Open (File => File,
                     Mode => In_File,
                     Name => To_String (File_Name));
               while not End_Of_File (File) loop
                  Put_Line (Get_Line (File));
               end loop;
               Close (File);

            elsif Opt = "append" then
               -- File_Name := To_Unbounded_String (Argument (Arg_Counter));
               Open (File => File,
                     Mode => Append_File,
                     Name => Argument (Arg_Counter + 2));
               Put_Line (File, Argument (Arg_Counter + 1));
               Close (File);
               Arg_Counter := Arg_Counter + 2;

            elsif Opt = "-v" or Opt = "--version" then
               Put_Line ("version 1.0");

            elsif Opt = "-h" or Opt = "--help" then
               Put_Usage;

            --  elsif Opt = "-rf" or Opt = "--return_failure" then
            --     Set_Exit_Status (Failure);
            --
            --  elsif Opt = Rs or Opt = "--return_success" then
            --     Set_Exit_Status (Success);

            else
              Put_Line ("unknown option " & Opt);
              Set_Exit_Status (Failure);

            end if;

         end;
         Arg_Counter := Arg_Counter + 1;
      end loop;

   end if;

--  exception
--     when others =>
--        Set_Exit_Status (Failure);
--        raise;

end UUT;
