-- SPDX-License-Identifier: Apache-2.0

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure UUT is
   -- uut (Unit under Test) is a fake exe that just interact with the system,
   -- by, as an example, creating or modifying file, to test bbt.

   -- -------------------------------------------------------------------------
   procedure Put_Usage is
   begin
      Put_Line ("uut simulate an ""under test unit"" that can create,");
      Put_Line ("read, or append a text to a file, with typical");
      Put_Line ("file and command line input / output / error situations.");
      New_Line;
      Put_Line ("Usage:");
      Put_Line ("   uut create|read        file_name");
      Put_Line ("   uut append      ""Text"" file_name");
      Put_Line ("   uut -h | --help or no command line : display this message");
      Put_Line ("   uut -v | --version                 : display a version string");
      New_Line;
      Put_Line ("Return code:");
      Put_Line ("  Return code is set to 1 when :");
      Put_Line ("  - there is a command line error (unknown option for example)");
      Put_Line ("  - there is a file error (unable to open the given file, for example)");
      Put_Line ("  Return code is set to 0 otherwise.");
      New_Line;
      Put_Line ("Errors:");
      Put_Line ("  Error messages are output to the Standard_Error output");
      Put_Line ("  To test it, call create or read without file_name");
      Put_Line ("  When calling append with a file_name but without text to");
      Put_Line ("  append, an unhandled exception is raised.");
      New_Line;
   end Put_Usage;

   use Ada.Strings.Unbounded;
   use Ada.Command_Line;

   -- --------------------------------------------------------------------------
   Arg_Index : Positive := 1;
   File      : File_Type;
   File_Name : Unbounded_String;

   function No_More_Arg return Boolean is (Arg_Index = Argument_Count);

begin
   Set_Exit_Status (Failure);
   -- Failure by default, so that in case of an unexpected exception
   -- it will be already set accordingly.

   if Argument_Count < 1 then
      Put_Usage;

   else
      while Arg_Index <= Argument_Count loop
         declare
            Opt : constant String := Argument (Arg_Index);

         begin
            if Opt = "create" then ---------------------------------------------
               if No_More_Arg then
                  Put_Line (Standard_Error, "Missing file name");
               else
                  Arg_Index := Arg_Index + 1;
                  File_Name := To_Unbounded_String (Argument (Arg_Index));
                  Create (File => File,
                          Mode => Out_File,
                          Name => To_String (File_Name));
                  Set_Exit_Status (Success);
               end if;

            elsif Opt = "read" then --------------------------------------------
               if No_More_Arg then
                  Put_Line (Standard_Error, "Missing file name");
               else
                  Arg_Index := Arg_Index + 1;
                  File_Name := To_Unbounded_String (Argument (Arg_Index));
                  Open (File => File,
                        Mode => In_File,
                        Name => To_String (File_Name));
                  while not End_Of_File (File) loop
                     Put_Line (Get_Line (File));
                  end loop;
                  Close (File);
                  Set_Exit_Status (Success);
               end if;

            elsif Opt = "append" then ------------------------------------------
               if No_More_Arg then
                  Put_Line (Standard_Error, "Missing file name");
                  -- We do not check for the text to append,
                  -- let's there be exception!
               else
                  Open (File => File,
                        Mode => Append_File,
                        Name => Argument (Arg_Index + 2));
                  Put_Line (File, Argument (Arg_Index + 1));
                  Close (File);
                  Arg_Index := Arg_Index + 2;
                  Set_Exit_Status (Success);
               end if;

            elsif Opt = "-v" or Opt = "--version" then  ------------------------
               Put_Line ("uut version 1.0");
               Set_Exit_Status (Success);

            elsif Opt = "-h" or Opt = "--help" then ----------------------------
               Put_Usage;
               Set_Exit_Status (Success);

            else ---------------------------------------------------------------
              Put_Line ("unknown option " & Opt);
              Set_Exit_Status (Failure);

            end if;

         end;
         Arg_Index := Arg_Index + 1;
      end loop;

   end if;

end UUT;
