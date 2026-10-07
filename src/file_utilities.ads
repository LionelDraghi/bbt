-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
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
   --   that both are rooted in the same directory.
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
   -- - WARNING : this function strongly rely on the fact that both parameter
   --             are straight path. It's not going to work with path
   --             embedding for example "../lionel/../lionel/../lionel/"
   --             instead of "../lionel".
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
