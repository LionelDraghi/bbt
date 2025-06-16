-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;

with BBT.Model.Steps;
with BBT.Model;                         use BBT.Model;
with BBT.IO;
with BBT.Scenarios.Readers;             use BBT.Scenarios.Readers;
with BBT.Scenarios.Step_Parser;
with BBT.Settings;                      use BBT.Settings;
with BBT.Tests.Builder;                 use BBT.Tests.Builder;

with List_Image;

with GNAT.Traceback.Symbolic;

package body BBT.Scenarios.Files is

   -- --------------------------------------------------------------------------
   use BBT.IO;
   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := Scen_Files)
                             renames BBT.IO.Put_Line;

   -- --------------------------------------------------------------------------
   The_List : File_List.Vector;
   package File_List_Sorting is new File_List.Generic_Sorting;

   -- --------------------------------------------------------------------------
   procedure Add_Document (File_Name : String) is
      use Ada.Directories;
      Name : constant String := Full_Name (File_Name);
   begin
      if Name = Full_Name (Settings.Template_Name)
        or Name = Result_File_Name
      then
         --  Filters the md file created with --create-template, that is
         --  not supposed to be executed, and the output file if any.
         Put_Warning ("Ignoring file " & Name'Image);
         -- Fixme: need a test
         
      else
         The_List.Append (File_Name);

      end if;
   end Add_Document;

   -- --------------------------------------------------------------------------
   procedure Find_Documents
     (Dir         : String;
      Recursive   : Boolean;
      Remove_Root : String := Settings.Launch_Directory)
   is
      -- -----------------------------------------------------------------------
      Src_Count : array (Input_Format) of Natural := [others => 0];
      Dir_Count : array (Input_Format) of Natural := [others => 1];

      use Ada.Directories;

      -- -----------------------------------------------------------------------
      procedure Walk (Name : String;
                      L    : Input_Format) is
      -- code mostly from :
      -- https://rosettacode.org/wiki/Walk_a_directory/Recursively#Ada
         Extension_Regexp : constant String
           := File_Pattern (For_Format => L);

         -- --------------------------------------------------------------------
         procedure Process_File (Item : Directory_Entry_Type) is
            Name : constant String := Full_Name (Item);
         begin
            if Name'Length > Remove_Root'Length
              and then Name (Name'First .. Name'First + Remove_Root'Length - 1)
              = Remove_Root
            -- Simple optimization : if the long path is a subdir of the
            -- current one, we only print the subdir
            then
               Add_Document
                 (File_Name =>
                    (Name (Name'First + Remove_Root'Length + 1 .. Name'Last)));
            else
               Add_Document (File_Name => (Name));
            end if;
            Src_Count (L) := Src_Count (L) + 1;
         end Process_File;

         -- --------------------------------------------------------------------
         procedure Walk (Item : Directory_Entry_Type) is
         begin
            if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".."
            then
               -- This is OK with Unix and Windows dir, so I consider
               -- it as portable.
               Dir_Count (L) := Dir_Count (L) + 1;
               Walk (Full_Name (Item), L);
            end if;
         exception when Name_Error => null;
         end Walk;

      begin
         Put_Debug_Line ("Walking in " & Name & ", regexp = " & Extension_Regexp);
         Search (Directory => Name,
                 Pattern   => Extension_Regexp,
                 Filter    => [Directory => False, others => True],
                 Process   => Process_File'Access);
         if Recursive then
            Search (Directory => Name,
                    Pattern   => "",
                    Filter    => [Directory => True, others => False],
                    Process   => Walk'Access);
         end if;
      end Walk;

   begin
      for F in Valid_Input_Format'Range loop -- when Is_Enabled (F) loop
         Put_Debug_Line ("Analyzing directory " & Dir
                         & " for language : " & Input_Format'Image (F));
         Walk (Dir, F);
         if Src_Count (F) /= 0 then
            File_List_Sorting.Sort (The_List);
            Put_Debug_Line ("Found " & Integer'Image (Src_Count (F)) &
                              " "  & Input_Format'Image (F) & " src in" &
                              Natural'Image (Dir_Count (F)) & " dir");
         end if;
      end loop;

   end Find_Documents;

   -- --------------------------------------------------------------------------
   function No_Document_Found return Boolean is (The_List.Is_Empty);
   function Document_List return File_List.Vector is (The_List);

   use File_List;
   package File_List_Cursors is new List_Image.Cursors_Signature
     (Container => File_List.Vector,
      Cursor    => File_List.Cursor);

   function Image (C : Cursor) return String is (Element (C));

   function File_List_Image is new List_Image.Image
     (Cursors => File_List_Cursors,
      Style   => List_Image.Bracketed_List_Style);

   function One_Line_Image (Files : File_List.Vector) return String renames
     File_List_Image;

   -- --------------------------------------------------------------------------
   procedure Analyze_Document (File_Name : String) is

      Input : Ada.Text_IO.File_Type;
      use Ada.Text_IO;

      Lexer_Context : Parsing_Context := Initialize_Context;
      Loc           : Location_Type   := Location (File_Name, 0);

      File_Format : constant Valid_Input_Format := Readers.Format (File_Name);

   begin
      Open (Input,
            Mode => In_File,
            Name => File_Name);

      Put_Debug_Line ("Loading " & File_Format'Image
                      & " file " & File_Name'Image,
                      Location (Input));

      Tests.Builder.Add_Document (File_Name);
      -- The doc name is the file name, it is not in the file content
      -- so we have to pass it.

      while not End_Of_File (Input) loop
         Reset_Error_Counts;

         Loc := Location (Input);
         -- To be done before the Get_Line, otherwise Line is already
         -- on the next one.

         Line_Processing :
         declare
            Line                : aliased constant String := Get_Line (Input);
            Cmd_List            : Steps.Cmd_List;
            Code_Block_Expected : Boolean;
            Attrib              : constant Line_Attributes := Parse_Line
              (Line'Access, File_Format, Lexer_Context, Loc);

            generic
               type Enum_Type is (<>);
            function Padding (E : Enum_Type) return String;
            function Padding (E : Enum_Type) return String is
              (String'(1 .. Enum_Type'Width - E'Image'Length => ' '));

            function Attrib_Padding is new Padding (Line_Kind);

         begin
            Put_Debug_Line (Attrib.Kind'Image & Attrib_Padding (Attrib.Kind)
                            & Line'Image, Loc);
            case Attrib.Kind is
               when Feature_Line =>
                  Tests.Builder.Add_Feature (To_String (Attrib.Name), Loc);

               when Scenario_Line =>
                  Tests.Builder.Add_Scenario (To_String (Attrib.Name), Loc);

               when Background_Line =>
                  Tests.Builder.Add_Background (To_String (Attrib.Name), Loc);

               when Step_Line =>
                  declare
                     S : Steps.Step_Data := Scenarios.Step_Parser.Parse
                       (Attrib.Step_Ln,
                        Loc,
                        Code_Block_Expected,
                        Cmd_List);
                  begin
                     Tests.Builder.Add_Step
                       (S, Code_Block_Expected, Cmd_List, Loc,
                        Syntax_Error => Some_Error);
                  end;

               when Code_Fence =>
                  Tests.Builder.Add_Code_Fence (Loc);

               when Text_Line =>
                  Tests.Builder.Add_Line (To_String (Attrib.Line), Loc);

               when Empty_Line =>
                  Tests.Builder.Add_Line (Line, Loc);

            end case;

            exit when IO.Some_Error and Settings.Stop_On_Error;

         end Line_Processing;

      end loop;

      Tests.Builder.Close_Document (Loc);

      -- And finally, let's record the document
      Close (Input);

   exception
      when E : others =>
         IO.Put_Exception (Ada.Exceptions.Exception_Message (E)
                           & ASCII.LF
                           & GNAT.Traceback.Symbolic.Symbolic_Traceback (E),
                           Loc);
         if Settings.Stop_On_Error then raise; end if;

   end Analyze_Document;

end BBT.Scenarios.Files;
