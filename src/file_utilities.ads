-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with GNAT.Directory_Operations;

package File_Utilities is

   Separator : constant Character := GNAT.Directory_Operations.Dir_Separator;
   -- To remove the dependency to GNAT, set it explicitly to '\' or '/'

   -- -----------------------------------------------------------------------
   function On_Windows return Boolean is (Separator = '\');

   -- --------------------------------------------------------------------------
   function Short_Path (From_Dir : String;
                        To_File  : String;
                        Prefix   : String := "") return String;

   -- Short_Path gives a relative Path from From_Dir to To_File.
   --   If  From_Dir => "/home/tests/",
   --   and To_File  => "/home/tests/mysite/site/idx.txt"
   --   then Short_Path returns     "mysite/site/idx.txt"
   --
   --   If  From_Dir => "../tests/",
   --   and To_File  => "../tests/mysite/site/idx.txt"
   --   then Short_Path returns  "mysite/site/idx.txt"
   --
   -- - NOTE that if both dir & file are not absolute path, then we assume
   --   that both are rooted in the same directory, and both are resolved
   --   against the current directory before the walk: a relative ".." is a
   --   level only once resolved. So the two paths may carry ".." chains of
   --   different depths, e.g. From_Dir => "../../../docs/results" and
   --   To_File => "../docs/idx.txt".
   --
   -- - From_Dir may ends with a Separator or not, meaning that
   --   both "/usr" and "/usr/" are OK.
   --
   --   On Windows, DOS path like "C:" in "C:\Users" are supported,
   --   but UNC path (like \\host\server\path) are only tested for the simple
   --   case where both From_Dir and To_File starts the same.
   --
   -- - Prefix may be used if you want a specific current directory prefix.
   --   For instance, it may be set to '.' & Separator if you want a "./"
   --   prefix, or set to "$PWD" & Separator.
   --
   -- - From_Dir may be a parent, a sibling or a child of the To_File dir.
   --   If  From_Dir => "/home/tests/12/34",
   --   and To_File  => "/home/tests/idx.txt"
   --   then Short_Path returns "../../idx.txt"
   --
   -- - In short: the walk below is textual, it compares the two paths
   --   component by component. A pair of relative paths is resolved first
   --   (see above), which handles their ".." chains. What is not supported:
   --     * a pair mixing an absolute and a relative path, walked as given:
   --       only two relative paths are resolved, both or neither, since
   --       comparing a resolved path with an unresolved one would leave
   --       them with nothing in common;
   --     * a Windows drive-qualified relative name ("x:foo"), that the
   --       runtime cannot resolve either;
   --     * a ".." embedded in the middle of a path ("../lionel/../lionel/"),
   --       walked component by component like anything else.
   --
   -- Exceptions:
   --   If From_Dir is not a To_File's parent, function
   --   Ada.Directories.Containing_Directory is used, and so Name_Error
   --   is raised if From_Dir does not allow the identification of
   --   an external file, and Use_Error is raised if From_Dir
   --   does not have a containing Directory.
   --
   -- Interesting reference on DOS / Windows PATH:
   -- https://learn.microsoft.com/en-us/dotnet/standard/io/file-path-formats
   --
   -- It's for .Net, but starts with a short explanation on DOS and UNC
   -- Path format.
   -- Summary :
   -- - on DOS : C:\Documents\Newsletters\Summer2018.pdf
   --                                     ^^^^^^^^^^^^^^ : file name
   --              ^^^^^^^^^^^^^^^^^^^^^^^               : path
   --            ^^                                      : Volume (or Drive),
   --                                                      one character
   --   Note that path may be relative, even if the Volume is specified :
   --   C:Documents  : relative path (relative name in Ada parlance)
   --   C:\Documents : absolute path (full name     in Ada parlance)
   --
   -- - UNC :
   --   \\Server2\Share\Test\Foo.txt
   --                  ^^^^^^^^^^^^^ : path
   --             ^^^^^              : share name
   --     ^^^^^^^                    : server (or host) name
   --   ^^^^^^^^^^^^^^^              : volume
   --   UNC expression are always "fully qualified", meaning that the path is
   --   independent of the current directory and does not change when the
   --   current directory changes (you always have a '\' between the volume and
   --   the path.
   --   Another example :
   --   \\system07\C$\ designate the root of the C drive
   --
   -- On legal character :
   --   From https://stackoverflow.com/questions/1976007/what-characters-are-forbidden-in-windows-and-linux-directory-names
   --   I understand that :
   --     < (less than)
   --     > (greater than)
   --     : (colon - sometimes works, but is actually NTFS Alternate Data Streams)
   --     " (double quote)
   --     / (forward slash)
   --     \ (backslash)
   --     | (vertical bar or pipe)
   --     ? (question mark)
   --     * (asterisk)
   --   are illegal character in Windows filenames, and only '/' is illegal on
   --   Linux/Unix.
   --
   --   Meaning that "C:\Foo\Bar" could be a legal file name on Linux.
   --   Because of this, '/' is the only separator considered on Unix, instead
   --   of both '/' and '\' on Windows.

   -- --------------------------------------------------------------------------
   function Escape (Text : in String) return String;
   -- bash specific function that escape characters
   -- ' '
   -- & '"' & '#' & '$'
   -- & '&' & ''' & '('
   -- & ')' & '*' & ','
   -- & ';' & '<' & '>'
   -- & '?' & '[' & '\'
   -- & ']' & '^' & '`'
   -- & '{' & '|' & '}'
   -- in command lines pushed to the shell.
   -- Refer to the "Which characters need to be escaped when using Bash?"
   -- discussion on stackoverflow.com

end File_Utilities;
