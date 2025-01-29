-- SPDX-License-Identifier: Apache-2.0

with Ada.Command_Line,
     Ada.Directories,
     Ada.Environment_Variables,
     Ada.Strings.Unbounded,
     Ada.Text_IO;

use Ada.Text_IO;

procedure sut is
   -- sut (Unit under Test) is a fake exe that just interact with the system,
   -- by, as an example, creating or modifying a file, to test bbt.

   -- -------------------------------------------------------------------------
   procedure Put_Usage is
   begin
      Put_Line ("sut simulate an ""under test unit"" that can create,");
      Put_Line ("read, or append a text to a file, with typical");
      Put_Line ("file and command line input / output / error situations.");
      New_Line;
      Put_Line ("Usage:");
      Put_Line ("   sut create|read        file_name");
      Put_Line ("   sut append      ""Text"" file_name");
      Put_Line ("   sut delete             file_name   : prompt user to confirm deletion");
      Put_Line ("   sut read_env           var_name    : display environment variable");
      Put_Line ("   sut -h | --help or no command line : display this message");
      Put_Line ("   sut -v | --version                 : display a version string");
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

            elsif Opt = "delete" then --------------------------------------------
               declare
                  use Ada.Directories;
                  C : Character;
               begin
                  if No_More_Arg then
                     Put_Line (Standard_Error, "Missing file name");
                  else
                     Arg_Index := Arg_Index + 1;
                     File_Name := To_Unbounded_String (Argument (Arg_Index));
                     if Exists (To_String (File_Name)) and then
                       Kind (To_String (File_Name)) = Ordinary_File
                     then
                        Get_Answer : loop
                           Ada.Text_IO.Put_Line ("Delete " &
                                                   To_String (File_Name) &
                                                   "? [Y]es/[N]o");
                           Ada.Text_IO.Get_Immediate (C);
                           case C is
                              when 'N' | 'n' =>
                                 exit Get_Answer;
                              when 'Y' | 'y' =>
                                 Ada.Text_IO.Put_Line
                                   ("Deleting " & To_String (File_Name));
                                 Delete_File (To_String (File_Name));
                                 Set_Exit_Status (Success);
                                 exit Get_Answer;
                              when others    => null;
                           end case;
                        end loop Get_Answer;

                     else
                        Put_Line (Standard_Error, "No " &
                                    To_String (File_Name) & " file");
                     end if;
                  end if;
               end;

            elsif Opt = "read_env" then ----------------------------------------
               declare
                  use Ada.Environment_Variables;
                  Var_Name : Unbounded_String;

               begin
                  if No_More_Arg then
                     Put_Line (Standard_Error,
                               "Missing environment variable name");
                  else
                     Arg_Index := Arg_Index + 1;
                     Var_Name := To_Unbounded_String (Argument (Arg_Index));
                     if Exists (To_String (Var_Name)) then
                        Put_Line (Value (To_String (Var_Name)));
                        Set_Exit_Status (Success);
                     else
                        Put_Line (Standard_Error, "No " &
                                    To_String (Var_Name) &
                                    " environment variable");
                     end if;
                  end if;
               end;

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
               Put_Line ("sut version 1.0");
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

end sut;
