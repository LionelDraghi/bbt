-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with List_Image;                        use List_Image;
with List_Image.Unix_Predefined_Styles;

with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

package Text_Utilities is

   -- --------------------------------------------------------------------------
   package Texts is new Ada.Containers.Indefinite_Vectors (Positive,
                                                           String);
   subtype Text is Texts.Vector;
   Empty_Text : Text renames Texts.Empty_Vector;
   use type Texts.Vector;
   function Is_Empty (Item : Text) return Boolean is (Item = Empty_Text);

   package Texts_Sorting is new Texts.Generic_Sorting;

   -- --------------------------------------------------------------------------
   procedure Create_File (File_Name    : String;
                          With_Content : Text;
                          Executable   : Boolean);
   function Create_File (File_Name    : Unbounded_String;
                         With_Content : Text;
                         Executable   : Boolean) return Boolean;
   -- Any existing files with the same name is overwritten.
   -- The file is closed when the call ends.

   -- --------------------------------------------------------------------------
   procedure Put_Text (File : File_Type := Standard_Output;
                       Item : Text);
   procedure Put_Text_Head (Item       : Text;
                            File       : File_Type := Standard_Output;
                            Line_Count : Positive);
   procedure Put_Text_Tail (Item       : Text;
                            File       : File_Type := Standard_Output;
                            Line_Count : Positive);
   --  procedure Put_Text (Item      : Text;
   --                      File_Name : String);
   --  procedure Put_Text_Head (Item       : Text;
   --                           File_Name  : String;
   --                           Line_Count : Positive);
   --  procedure Put_Text_Tail (Item       : Text;
   --                           File_Name  : String;
   --                           Line_Count : Positive);
   function Get_Text (File : File_Type)   return Text;
   function Get_Text (File_Name : String) return Text;
   --  function Get_Text (File_Name : Unbounded_String) return Text;
   --  function Get_Text_Head (From       : Text;
   --                          Line_Count : Positive) return Text;
   --  function Get_Text_Tail (From       : Text;
   --                          Line_Count : Positive) return Text;
   --
   --  subtype Min_Shrinked_Length is Positive range 2 .. Positive'Last;
   --  function Shrink (The_Text   : Text;
   --                   Line_Count : Min_Shrinked_Length;
   --                   Cut_Mark   : String := "...") return Text;
   -- If Line_Count = 5 and Cut_Mark = "...", shrink a long text to
   --   line 1
   --   line 2
   --   ...
   --   line last -1
   --   line last
   -- If the Text is shorter or equal to Line_Count, output is the Text,
   -- obviously without Cut_Mark.

   -- --------------------------------------------------------------------------
   procedure Sort (The_Text : in out Text);

   -- --------------------------------------------------------------------------
   procedure Compare (Text1, Text2       : Text;
                      Case_Insensitive   : Boolean := True;
                      Ignore_Blanks      : Boolean := True;
                      Ignore_Blank_Lines : Boolean := True;
                      Sort_Texts         : Boolean := False;
                      Identical          : out Boolean);

   -- --------------------------------------------------------------------------
   function Is_Equal (Text1, Text2     : Text;
                      Case_Insensitive : Boolean := True;
                      Ignore_Blanks    : Boolean := True) return Boolean;

   function Is_Equal (Text1, Text2       : Text;
                      Case_Insensitive   : Boolean := True;
                      Ignore_Blanks      : Boolean := True;
                      Ignore_Blank_Lines : Boolean := True;
                      Sort_Texts         : Boolean := False) return Boolean;

   -- --------------------------------------------------------------------------
   function Contains (Text1, Text2       : Text;
                      Case_Insensitive   : Boolean := True;
                      Ignore_Whitespaces : Boolean := True;
                      Ignore_Blank_Lines : Boolean := True;
                      Sort_Texts         : Boolean := False) return Boolean;
   -- Return True if Text1 contains Text2.
   function Contains_Line (The_Text           : Text;
                           The_Line           : String;
                           Case_Insensitive   : Boolean := True;
                           Ignore_Whitespaces : Boolean := True) return Boolean;
   function Contains_String (The_Text           : Text;
                             The_String         : String;
                             Case_Insensitive   : Boolean := True;
                             Ignore_Whitespaces : Boolean := True) return Boolean;
   function Contains_Line (File_Name          : String;
                           The_Line           : String;
                           Case_Insensitive   : Boolean := True;
                           Ignore_Whitespaces : Boolean := True) return Boolean;
   function Contains_String (File_Name          : String;
                             The_String         : String;
                             Case_Insensitive   : Boolean := True;
                             Ignore_Whitespaces : Boolean := True) return Boolean;

   -- --------------------------------------------------------------------------
   function Matches (In_Text    : Text;
                     Regexp     : String)
                     return Boolean;

   -- --------------------------------------------------------------------------
   function First_Non_Blank_Line (In_Text : Text;
                                  From    : Positive := 1) return Natural;
   -- Start looking at index From
   -- Returns the index of the first non blank line if any, 0 otherwise

   -- --------------------------------------------------------------------------
   function Remove_Blank_Lines (From_Text : Text) return Text;
   function Join_Spaces (From : String) return String;
   -- Reduce multiple consecutive blanks to a single space character
   function Is_Blank (S : String) return Boolean is
     (Ada.Strings.Fixed.Index_Non_Blank (S) = 0);

   use Texts;
   package Text_Cursors is new List_Image.Cursors_Signature
     (Container => Vector,
      Cursor    => Cursor);

   function Image (C : Cursor) return String is (Element (C));

   -- To get a normal image, and not all lines concatenated
   -- on the same line as when using 'Image
   function Text_Image is new List_Image.Image
     (Cursors => Text_Cursors,
      Style   => List_Image.Unix_Predefined_Styles.Simple_One_Per_Line_Style);

   -- Return the text prefixed and postfixed by (one of) the MarkDown code
   -- fence marks, here "~~~".
   package Code_Fenced_Style is new List_Image.Image_Style
     (Prefix           => Unix_EOL & "~~~" & Unix_EOL,
      Separator        => "  " & Unix_EOL,
      Postfix          => Unix_EOL & "~~~" & Unix_EOL,
      Postfix_If_Empty => "~~~" & Unix_EOL);
   function Code_Fenced_Image is new List_Image.Image
     (Cursors => Text_Cursors,
      Style   => Code_Fenced_Style);
   -- Fixme : this, and all dependencies to List_Image, should be moved
   -- to a child package Markdown_Images

end Text_Utilities;
