with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;                       use Ada.Text_IO;

package Text_Utilities is

   -- --------------------------------------------------------------------------
   package Texts is new Ada.Containers.Indefinite_Vectors (Positive,
                                                           String);
   subtype Text is Texts.Vector;
   Empty_Text : Text renames Texts.Empty_Vector;

   -- --------------------------------------------------------------------------
   function Create_File (File_Name    : String;
                         With_Content : Text) return Boolean;
   function Create_File (File_Name    : Unbounded_String;
                         With_Content : Text) return Boolean;
   -- Return true if the file was created as expected
   -- Any existing files with the same name is overwritten.
   -- The file is closed when the call ends.

   -- --------------------------------------------------------------------------
   procedure Put_Text (File : File_Type := Standard_Output;
                       Item : Text);
   procedure Put_Text_Head (File       : File_Type := Standard_Output;
                            Item       : Text;
                            Line_Count : Positive);
   procedure Put_Text_Tail (File       : File_Type := Standard_Output;
                            Item       : Text;
                            Line_Count : Positive);
   procedure Put_Text (File_Name : String;
                       Item : Text);
   procedure Put_Text_Head (File_Name : String;
                            Item       : Text;
                            Line_Count : Positive);
   procedure Put_Text_Tail (File_Name : String;
                            Item       : Text;
                            Line_Count : Positive);
   function Get_Text (File : File_Type)   return Text;
   function Get_Text (File_Name : String) return Text;
   function Get_Text (File_Name : Unbounded_String) return Text;

   -- --------------------------------------------------------------------------
   procedure Compare (Text1, Text2       : Text;
                      Ignore_Blank_Lines : Boolean := True;
                      Case_Insensitive   : Boolean := True;
                      Identical          : out Boolean;
                      Diff_Index         : out Natural);
   -- If Test1 = Text2, return Identical = True and Diff_Index = 0
   -- Otherwise, return False and Index of the first different line in Text2

   function Is_Equal (Text1, Text2       : Text;
                      Ignore_Blank_Lines : Boolean := True;
                      Case_Insensitive   : Boolean := True) return Boolean;

   -- --------------------------------------------------------------------------
   function Contains (Text1, Text2     : Text;
                      Case_Insensitive : Boolean := True) return Boolean;
   -- Return True if Text1 contains Text2.
   function Contains_Line (The_Text         : Text;
                           The_Line         : String;
                           Case_Insensitive : Boolean := True) return Boolean;
   function Contains_String (The_Text         : Text;
                             The_String       : String;
                             Case_Insensitive : Boolean := True) return Boolean;
   function Contains_Line (File_Name        : String;
                           The_Line         : String;
                           Case_Insensitive : Boolean := True) return Boolean;
   function Contains_String (File_Name        : String;
                             The_String       : String;
                             Case_Insensitive : Boolean := True) return Boolean;

   -- --------------------------------------------------------------------------
   function First_Non_Blank_Line (In_Text : Text;
                                  From    : Positive := 1) return Natural;
   -- Start looking at index From
   -- Returns the index of the first non blank line if any, 0 otherwise

end Text_Utilities;
