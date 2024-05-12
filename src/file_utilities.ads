-- -----------------------------------------------------------------------------
-- smk, the smart make (http://lionel.draghi.free.fr/smk/)
-- © 2018, 2019 Lionel Draghi <lionel.draghi@free.fr>
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- http://www.apache.org/licenses/LICENSE-2.0
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-- -----------------------------------------------------------------------------

package File_Utilities is

   Separator : constant Character := '/'; -- OS dependent!

   -- --------------------------------------------------------------------------
   function Short_Path (From_Dir : String;
                        To_File  : String;
                        Prefix   : String := "") return String
     with Pre => From_Dir (From_Dir'First) = Separator;
   --
   -- Short_Path gives a relative Path from From_Dir to To_File.
   --   If  From_Dir => "/home/tests/",
   --   and To_File  => "/home/tests/mysite/site/idx.txt"
   --   then Short_Path returns     "mysite/site/idx.txt"
   --
   -- - From_Dir must be an absolute Path, that is starting with a
   --   Separator.
   --   From_Dir may ends with a Separator or not, meaning that
   --   both "/usr" and "/usr/" are OK.
   --   NB : Devices like "C:" in "C:\Users" are not permitted.
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
   -- Exceptions:
   --   If From_Dir is not a To_File's parent, function
   --   Ada.Directories.Containing_Directory is used, and so Name_Error
   --   is raised if From_Dir does not allow the identification of
   --   an external file, and Use_Error is raised if From_Dir
   --   does not have a containing Directory.
   --

   -- --------------------------------------------------------------------------
   function Escape (Text : String) return String;
   -- Linux/bash specific function that escape characters
   -- ' '
   -- & '"' & '#' & '$'
   -- & '&' & ''' & '('
   -- & ')' & '*' & ','
   -- & ';' & '<' & '>'
   -- & '?' & '[' & '\'
   -- & ']' & '^' & '`'
   -- & '{' & '|' & '}'
   -- in command lines pushed to bash.
   -- Refer to the "Which characters need to be escaped when using Bash?"
   -- discussion on stackoverflow.com
   -- Fixme: this function is not portable!

end File_Utilities;
