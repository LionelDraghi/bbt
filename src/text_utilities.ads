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

   -- --------------------------------------------------------------------------
   procedure Compare (Text1, Text2 : Text;
                      Identical    : out Boolean;
                      Diff_Index   : out Natural);
   -- If Test1 = Text2, return Identical = True and Diff_Index = 0
   -- Otherwise, return False and Index of the first different line in Text2

   -- --------------------------------------------------------------------------
   function Contains (Text1, Text2 : Text) return Boolean;
   -- Return True Text1 contains Text2.

end Text_Utilities;
