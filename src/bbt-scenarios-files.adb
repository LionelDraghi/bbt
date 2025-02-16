-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings;
with Ada.Text_IO;

with BBT.Documents;     use BBT.Documents;
with BBT.IO;
with BBT.Scenarios.MDG_Lexer;
with BBT.Scenarios.Step_Parser;
with BBT.Settings;      use BBT.Settings;
with BBT.Tests.Builder; use BBT.Tests.Builder;

with GNAT.Traceback.Symbolic;

with List_Image;

package body BBT.Scenarios.Files is

   -- --------------------------------------------------------------------------
   function "+" (Name : File_Name) return String is
     (To_String (Name));
   function "+" (Name : String) return File_Name is
     (File_Name'(To_Unbounded_String (Name)));

   -- --------------------------------------------------------------------------
   --  function Is_Dir (File_Name : String) return Boolean is
   --     use Ada.Directories;
   --  begin
   --     return Exists (File_Name) and then Kind (File_Name) = Directory;
   --  end Is_Dir;

   The_List : File_List.Vector;
   package File_List_Sorting is new File_List.Generic_Sorting;

   -- --------------------------------------------------------------------------
   procedure Append_File (File_Name : String) is
      use Ada.Directories;
      Name : constant String := Full_Name (File_Name);
   begin
      -- Ada.Text_IO.Put_Line ("--- File_Name = " & File_Name);
      -- Ada.Text_IO.Put_Line ("--- Full_Name (File_Name) = " & Name);

      if Name /= Full_Name (Settings.Template_Name)
        and then Name /= Result_File_Name
      then
         --  Filters the md file created with --create-template, that is
         --  not supposed to be executed, and the output file if any.
         The_List.Append (File_Name);
      end if;
   end Append_File;

   -- --------------------------------------------------------------------------
   procedure Find_BBT_Files
     (Recursive   : Boolean;
      Start_In    : String := "./";
      Remove_Root : String := Settings.Launch_Directory)

   is
      use Ada.Directories;

      -- -----------------------------------------------------------------------
      procedure Walk (Name : String) is
         -- code mostly from :
         -- https://rosettacode.org/wiki/Walk_a_directory/Recursively#Ada

         -- --------------------------------------------------------------------
         procedure Process_File (Item : Directory_Entry_Type) is
            Name : constant String := Full_Name (Item);
         begin
            if Name'Length > Remove_Root'Length and then
              Name (Name'First .. Name'First + Remove_Root'Length - 1)
              = Remove_Root
            -- Simple optimization : if the long path is a subdir of the
            -- current one, we only print the subdir
            then
               Append_File
                 ((Name (Name'First + Remove_Root'Length + 1 .. Name'Last)));
            else
               Append_File ((Name));
            end if;
         end Process_File;

         -- --------------------------------------------------------------------
         procedure Process_Dir (Item : Directory_Entry_Type) is
         begin
            if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".."
            then
               -- This is OK with Unix and Windows dir, so I consider
               -- it as portable.
               Walk (Full_Name (Item));
            end if;
         exception when Name_Error => null;
         end Process_Dir;

         Extension : constant String := "*.md";

      begin
         IO.Put_Line ("Walking in " & Name,
                      Location  => IO.No_Location,
                      Verbosity => IO.Debug);
         Search (Directory => Name,
                 Pattern   => Extension,
                 Filter    => [Directory => False, others => True],
                 Process   => Process_File'Access);
         if Recursive then
            Search (Directory => Name,
                    Pattern   => "",
                    Filter    => [Directory => True, others => False],
                    Process   => Process_Dir'Access);
         end if;
      end Walk;

   begin
      Walk (Start_In);
      File_List_Sorting.Sort (The_List);
      IO.Put_Line ("Found " & The_List'Image,
                   Location  => IO.No_Location,
                   Verbosity => IO.Debug);

   end Find_BBT_Files;

   -- --------------------------------------------------------------------------
   function No_bbt_File return Boolean is (The_List.Is_Empty);
   function bbt_Files return File_List.Vector is (The_List);

   use File_List;
   package File_List_Cursors is new List_Image.Cursors_Signature
     (Container => File_List.Vector,
      Cursor    => File_List.Cursor);

   function Image (C : Cursor) return String is (Element (C));

   function Id_Set_Image is new List_Image.Image
     (Cursors => File_List_Cursors,
      Style   => List_Image.Bracketed_List_Style);

   function One_Line_Image (Files : File_List.Vector) return String renames
     Id_Set_Image;

   -- --------------------------------------------------------------------------
   procedure Analyze_MDG_File (File_Name : String) is

      Input : Ada.Text_IO.File_Type;
      use BBT.IO;

      use Ada.Text_IO;

      use BBT.Scenarios.MDG_Lexer;
      MDG_Lexer_Context : Parsing_Context := Initialize_Context;
      Loc               : Location_Type   := Location (Input);

   begin
      IO.Put_Line ("==== Loading " & File_Name, IO.No_Location, IO.Debug);
      -- Ada.Text_IO.Put_Line ("==== Loading                        " & File_Name);

      Open (Input,
            Mode => In_File,
            Name => File_Name);

      -- Ada.Text_IO.Put_Line ("==== Ada.Text_IO.Name (File) return " & Ada.Text_IO.Name (Input));

      Tests.Builder.Add_Document (File_Name);
      -- The doc name is not in the file content

      while not End_Of_File (Input) loop
         Loc := Location (Input);
         -- to be done before the Get_Line, otherwise Line is already on the
         -- next one.

         Line_Processing : declare
            Line     : aliased constant String  := Get_Line (Input);
            Attrib   : constant Line_Attributes := Parse_Line
              (Line'Access, MDG_Lexer_Context, Loc);
            S        : Step_Type;
            Cmd_List : Cmd_Lists.Vector;
            Filler   : constant String := (if BBT.IO.Line (Loc) in 1 .. 9
                                           then "  | "
                                           elsif BBT.IO.Line (Loc) in 10 .. 99
                                           then " | "
                                           else "|�");
            Code_Block_Expected : Boolean;
         begin

            case Attrib.Kind is
            when Feature_Line =>
               Tests.Builder.Add_Feature (To_String (Attrib.Name), Loc);
               IO.Put_Line ("Feature     " & Filler & Line,
                            Location  => Loc,
                            Verbosity => IO.Debug);

            when Scenario_Line =>
               IO.Put_Line ("Scenario    " & Filler & Line,
                            Location  => Loc,
                            Verbosity => IO.Debug);
               Tests.Builder.Add_Scenario (To_String (Attrib.Name), Loc);

            when Background_Line =>
               IO.Put_Line ("Background  " & Filler & Line,
                            Location  => Loc,
                            Verbosity => IO.Debug);
               Tests.Builder.Add_Background (To_String (Attrib.Name), Loc);

            when Step_Line =>
               IO.Put_Line ("Step        " & Filler & Line,
                            Location  => Loc,
                            Verbosity => IO.Debug);
               S := Scenarios.Step_Parser.Parse (Attrib.Step_Ln,
                                                 Loc,
                                                 Code_Block_Expected,
                                                 Cmd_List);
               IO.Put_Line ("            " & Filler & "  "
                            & Short_Line_Image (S),
                            Location  => Loc,
                            Verbosity => IO.Debug);

               Tests.Builder.Add_Step (S, Code_Block_Expected, Cmd_List);

            when Code_Fence =>
               IO.Put_Line ("Code fence  " & Filler & Line,
                            Location  => Loc,
                            Verbosity => IO.Debug);
               Tests.Builder.Add_Code_Fence (Loc);

            when Text_Line =>
               if Tests.Builder.In_File_Content then
                  IO.Put_Line ("File content" & Filler & Line,
                               Location  => Loc,
                               Verbosity => IO.Debug);
               else
                  IO.Put_Line ("Ignored     " & Filler & Line,
                               Location  => Loc,
                               Verbosity => IO.Debug);
               end if;
               Tests.Builder.Add_Line (To_String (Attrib.Line));

            when Empty_Line =>
               Tests.Builder.Add_Line (Line);

            end case;

         end Line_Processing;

         if Some_Error then exit; end if;

      end loop;

      Tests.Builder.End_Of_Scenario (Loc);

      -- and finally, let's record the document
      Close (Input);
      --  Put_Line ("Doc_List = " & Tests.Builder.The_Tests_List.all'Image,
      --            Verbosity => IO.Debug);

   exception
      when E : others =>
         IO.Put_Exception (Ada.Exceptions.Exception_Message (E)
                           & ASCII.LF
                           & GNAT.Traceback.Symbolic.Symbolic_Traceback (E),
                           Loc);
         if not Settings.Keep_Going then raise;
         end if;

   end Analyze_MDG_File;

end BBT.Scenarios.Files;
