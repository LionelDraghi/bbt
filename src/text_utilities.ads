-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with List_Image;                        use List_Image;
with List_Image.Unix_Predefined_Styles;

with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;                       use Ada.Text_IO;

package Text_Utilities is

   -- --------------------------------------------------------------------------
   type Line_Length is new Natural; -- length of a line in characters
   type Line_Index  is new Positive; -- index of a line in a text, starting at 1
   subtype Line_Count is Ada.Containers.Count_Type; -- number of lines in a text

   -- --------------------------------------------------------------------------
   package Texts is new Ada.Containers.Indefinite_Vectors (Line_Index,
                                                           String);
   subtype Text is Texts.Vector;
   Empty_Text : Text renames Texts.Empty_Vector;
   use type Texts.Vector;
   function Is_Empty (Item : Text) return Boolean is (Item = Empty_Text);
   function "&" (Left : Text; Right : String) return Text renames Texts."&";

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
   --  procedure Put_Text_Head (Item       : Text;
   --                           File       : File_Type := Standard_Output;
   --                           Line_Count : Positive);
   --  procedure Put_Text_Tail (Item       : Text;
   --                           File       : File_Type := Standard_Output;
   --                           Line_Count : Positive);
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
   function To_String (T : Text) return String;
   -- Converts a Text to a String with lines separated by LF.

   -- --------------------------------------------------------------------------
   function Side_By_Side (T1, T2            : Text;
                          Case_Insensitive   : Boolean := True;
                          Ignore_Whitespaces : Boolean := True) return Text;
   -- Returns a side-by-side comparison of T1 and T2.
   -- T1 lines are on the left, T2 lines on the right.
   -- Lines considered equal regarding the Case_Insensitive and
   -- Ignore_Whitespaces parameters are suppressed.
   -- The output format is inspired by sdiff with options:
   -- -l, --left-column : Output only the left column of common lines.
   -- -s, --suppress-common-lines : Do not output common lines.
   -- Each group of consecutive differing lines is preceded by a hunk header
   -- in the git diff style: "@@ -start,count +start,count @@" where count
   -- is the number of lines of the hunk on each side. The count is omitted
   -- when it is 1, as in git diff.
   -- Unlike in git diff, start is the line number of the first differing
   -- line, not the line number of the first context line, and the counts
   -- do not include the context lines. On a side where the hunk has no
   -- line (a pure insertion or deletion), start is, as in git diff, the
   -- line number preceding the insertion point, and the count is 0.
   -- Hunks separated by more than twice the context are kept apart, and
   -- closer hunks are merged, as in git diff.
   -- Each hunk is surrounded by up to 3 common lines of context, displayed
   -- in the left column only, as sdiff does with the -l option.
   -- Separators between left and right parts of are:
   -- - ' | ' : line differs
   -- - ' < ' : only the first file contains the line
   -- - ' > ' : only the second file contains the line
   -- Separators are aligned, the column of the separator is the column of
   -- the last character of the longest line in T1, plus one space.
   -- T1 and T2 are first aligned on their longest common subsequence of
   -- lines, computed with the Myers algorithm, as diff and git diff do:
   -- an insertion or deletion is reported as such, and does not shift
   -- the following pairings. Lines only in one text that occur at the
   -- same place are paired, and shown as differing lines. When the
   -- alignment cost is above Max_Alignment_Cost, the texts are paired
   -- positionally instead, to keep a bounded execution time on
   -- pathological inputs.

   --  -- --------------------------------------------------------------------------
   --  procedure Compare (Text1, Text2       : Text;
   --                     Case_Insensitive   : Boolean := True;
   --                     Ignore_Blanks      : Boolean := True;
   --                     Ignore_Blank_Lines : Boolean := True;
   --                     Sort_Texts         : Boolean := False;
   --                     Identical          : out Boolean);

   -- --------------------------------------------------------------------------
   procedure Get_Text_Diff (Text1, Text2       : Text;
                            Case_Insensitive   : Boolean := True;
                            Ignore_Blanks      : Boolean := True;
                            Ignore_Blank_Lines : Boolean := True;
                            Sort_Texts         : Boolean := False;
                            T1_Diff            : out Text;
                            T2_Diff            : out Text;
                            Diff_Start_In_T1   : out Line_Index);

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
   function Index_Of (T1, T2              : Text;
                      Case_Insensitive   : Boolean := True;
                      Ignore_Whitespaces : Boolean := True) return Line_Index'Base;
   -- Returns the index in T1 of the first line containing the first line of
   -- T2, with the same search criteria as Contains, or 0 if not found.
   -- Useful to locate where an unexpected text was found.
   -- Note: when T2 is multi-line, the returned index is the first line
   -- where T2 first line is found, which may differ from the position
   -- where the whole of T2 matches.
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
   type Ignore_Pattern is
     (No_Pattern, Time, Dir, Version, Location, Path_Syntax);
   -- The patterns that neutralize the volatile parts of a comparison
   -- before it happens, as stated by the (ignoring ...) clause of the
   -- comparison steps (cf. docs/features/A300_Ignoring.md).
   -- No_Pattern is only the neutral value used to iterate on the real
   -- patterns.
   -- All patterns but Path_Syntax mask the matching substrings;
   -- Path_Syntax rewrites the path syntax instead (backslashes to
   -- slashes, drive prefix removed), keeping the path itself checked.

   type Ignore_Pattern_Set is array (Ignore_Pattern) of Boolean;
   type Pattern_Counts     is array (Ignore_Pattern) of Natural;
   No_Ignore_Pattern : constant Ignore_Pattern_Set := [others => False];

   function Image (P : Ignore_Pattern) return String;
   -- Returns the pattern name as used in the clause: "time", "dir",
   -- and an empty string for No_Pattern

   function Image (Patterns : Ignore_Pattern_Set) return String;
   -- Returns the pattern list in the clause syntax: "time",
   -- "time and dir", ...

   function Mask (T        : Text;
                  Patterns : Ignore_Pattern_Set;
                  Counts   : out Pattern_Counts) return Text;
   -- Returns a copy of T where every substring matching one of the
   -- selected Patterns is replaced by a fixed marker, and gives in
   -- Counts the number of matches found for each Pattern. Patterns
   -- that are not selected are ignored. A pattern that matches
   -- nothing on the actual side of a comparison most probably declares
   -- volatile something the tested program does not print.

   function Mask (T        : Text;
                  Patterns : Ignore_Pattern_Set) return Text;

   -- --------------------------------------------------------------------------
   package Ignore_Regexp_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, String);
   subtype Ignore_Regexp_List is Ignore_Regexp_Lists.Vector;
   No_Ignore_Regexp : Ignore_Regexp_List renames Ignore_Regexp_Lists.Empty_Vector;
   -- The custom patterns of the (ignoring ...) clause: any word of the
   -- clause that is not a named pattern is a regexp, masking the
   -- matching substrings of both sides of the comparison
   -- (cf. docs/features/A300_Ignoring.md)

   function Mask (T      : Text;
                  Regexp : String;
                  Count  : out Natural) return Text;
   -- Returns a copy of T where every substring matching Regexp is
   -- replaced by the fixed marker, and gives in Count the number of
   -- matches found. The regexp is compiled: the caller is expected to
   -- have validated it beforehand, as the parsing of the clause does.

   function Mask (T      : Text;
                  Regexp : String) return Text;

   -- --------------------------------------------------------------------------
   function Max_Line_Length (In_Text : Text) return Line_Length is
     (Line_Length ([for Line of In_Text => Line'Length]'Reduce (Natural'Max, 0)));

   -- --------------------------------------------------------------------------
   function First_Non_Blank_Line (In_Text : Text;
                                  From    : Line_Index := 1) return Natural;
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

   -- Markdown stuff -----------------------------------------------------------

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

   function Wrap_In_Backticks (S : String) return String is
     ('`' & S & '`');

end Text_Utilities;
