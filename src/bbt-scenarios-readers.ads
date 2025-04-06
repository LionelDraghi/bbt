-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO;        use BBT.IO;

package BBT.Scenarios.Readers is
   -- Common declarations for scenario readers

   -- --------------------------------------------------------------------------
   type Input_Format is (MDG, Adoc, Unknown);
   subtype Valid_Input_Format is
     Input_Format range Input_Format'First .. Input_Format'Pred (Unknown);

   function Format (File_Name : String) return Valid_Input_Format;

   function File_Pattern (For_Format : Valid_Input_Format) return String;
   -- Returns the regexp that will be used to identify sources files
   -- of this format.

   type Parsing_Context is limited private;

   -- --------------------------------------------------------------------------
   function Initialize_Context return Parsing_Context;

   -- --------------------------------------------------------------------------
   function Parse_Line (Line       : access constant String;
                        For_Format : Valid_Input_Format;
                        Context    : in out Parsing_Context;
                        Loc        : Location_Type)
                        return Line_Attributes;

   function Code_Fence_Line (Line             : String;
                             For_Format       : Valid_Input_Format;
                             Look_For_Closing : Boolean) return Boolean;

private
   type Parsing_Context is record
      In_Code_Fence : Boolean;
      In_Scenario   : Boolean;
   end record;

   -- -------------------------------------------------------------------------
   type Abstract_Reader is abstract tagged limited null record;

   function File_Pattern
     (Reader : Abstract_Reader) return String is abstract;
   -- Regexp of files of this format

   function Format
     (Reader : Abstract_Reader) return Valid_Input_Format is abstract;

   function Is_Of_The_Format
     (Reader    : Abstract_Reader;
      File_Name : String) return Boolean is abstract;
   -- Returns True if the given file is processed by this reader.

   function Remove_Emphasis (Reader    : Abstract_Reader;
                             S         : String) return String is abstract;
   -- Keywords may be surrounded by Bold or Underline marks, like '*' or '_'
   -- for Markdown. This function return S without those marks if any.

   -- --------------------------------------------------------------------------
   function Find_Heading_Mark
     (Reader      : Abstract_Reader;
      Line        : String;
      First       : out Natural;
      Last        : out Natural;
      Title_First : out Natural;
      Title_Last  : out Natural;
      Location    : Location_Type) return Boolean is abstract;
   -- In Markdown, for example :
   -- ### **Header** : xy z
   -- First will point 'H'
   -- Last  will point 'r'
   -- Title_First will point 'x'
   -- Title_Last  will point 'z'

   -- -------------------------------------------------------------------------
   function Code_Fence_Line
     (Reader           : Abstract_Reader;
      Line             : String;
      Look_For_Closing : Boolean) return Boolean is abstract;

   -- -------------------------------------------------------------------------
   type Interface_Access is access all Abstract_Reader'class;
   procedure Register (Reader     : Interface_Access;
                       For_Format : Valid_Input_Format);

end BBT.Scenarios.Readers;
