-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

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

with Ada.Directories;

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
   begin
      if Full_Name (File_Name) /= Full_Name (Settings.Template_Name)
        and then Full_Name (File_Name) /= Result_File_Name
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

   -- --------------------------------------------------------------------------
   procedure Analyze_MDG_File (File_Name : String) is

      Input : Ada.Text_IO.File_Type;
      use BBT.IO;

      use Ada.Text_IO;

      use BBT.Scenarios.MDG_Lexer;
      MDG_Lexer_Context : Parsing_Context := Initialize_Context;
      Loc               : Location_Type   := Location (Input);

   begin
      Open (Input,
            Mode => In_File,
            Name => File_Name);

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

         begin
            case Attrib.Kind is
            when Feature_Line =>
               Tests.Builder.Add_Feature (To_String (Attrib.Name), Loc);

            when Scenario_Line =>
               Tests.Builder.Add_Scenario (To_String (Attrib.Name), Loc);

            when Background_Line =>
               Tests.Builder.Add_Background (To_String (Attrib.Name), Loc);

            when Step_Line =>
               S := Scenarios.Step_Parser.Parse (Attrib.Step_Ln, Loc, Cmd_List);
               Tests.Builder.Add_Step (S, Cmd_List);

            when Code_Fence =>
               Tests.Builder.Add_Code_Fence (Loc);

            when Text_Line =>
               Tests.Builder.Add_Line (To_String (Attrib.Line), Loc);

            when Empty_Line =>
               Tests.Builder.Add_Line (Line, Loc);

            end case;

         end Line_Processing;

      end loop;

      -- and finally, let's record the document
      Close (Input);
      Put_Line ("Doc_List = " & Tests.Builder.The_Tests_List.all'Image,
                Verbosity => IO.Debug);

   exception
      when E : others =>
         -- Missing_Scenario
         IO.Put_Exception (Ada.Exceptions.Exception_Message (E)
                           & " while processing " & File_Name
                           & GNAT.Traceback.Symbolic.Symbolic_Traceback (E),
                           Loc);

   end Analyze_MDG_File;

end BBT.Scenarios.Files;
