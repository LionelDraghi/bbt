-- -----------------------------------------------------------------------------
-- bbt, the BlackBox tester (http://lionel.draghi.free.fr/bbt/)
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

with Ada.Directories;
-- with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body BBT.Settings is

   -- --------------------------------------------------------------------------
   -- Most of the variable here are "write once, read more".
   -- To avoid the cost of Unbounded strings manipulation,
   -- they are implemented as access to String
   -- Runfl_Name      : access String := null;
   -- Cmd_Line        : Unbounded_String := Null_Unbounded_String;
   WD : constant access String :=
          new String'(Ada.Directories.Current_Directory);

   Enabled_Topics : array (Extended_Topics) of Boolean := [others => False];

   -- --------------------------------------------------------------------------
   procedure Enable_Topic (Topic : Topics) is
   begin
      Enabled_Topics (Topic) := True;
   end Enable_Topic;

   function Is_Enabled (Topic : Extended_Topics) return Boolean is
     (Topic in Topics and then Enabled_Topics (Topic));

   --  -- --------------------------------------------------------------------------
   --  function Is_File_In (File, Dir : String) return Boolean is
   --     Compared_Length : constant Natural := (if Dir (Dir'Last) = '*'
   --                                            then Dir'Length - 1
   --                                            else Dir'Length);
   --     -- return True if File is in Dir, supposing that both are full name.
   --     -- e.g. (Dir => /usr/*, File => /usr/lib/locale) return True
   --     -- e.g. (Dir => /usr/*, File => locale)          return False
   --  begin
   --     return (File'Length >= Compared_Length and then
   --             File (File'First .. File'First - 1 + Compared_Length)
   --             = Dir (Dir'First .. Dir'First  - 1 + Compared_Length));
   --  end Is_File_In;

   -- --------------------------------------------------------------------------
   function Initial_Directory return String is (WD.all);
   function Run_Dir_Name return String is
      --     (Ada.Directories.Containing_Directory (Smkfile_Name));
     (Initial_Directory); -- for now : in the future, should run where the
                          -- scenario file is

   -- --------------------------------------------------------------------------
   function Is_Authorised (Level : Print_Out_Level) return Boolean is
   begin
      case Level is
         when Debug =>
            return True;
         when Verbose =>
            return
              Verbosity = Verbose or
              Verbosity = Normal or
              Verbosity = Quiet;
         when Normal =>
            return
              Verbosity = Normal or
              Verbosity = Quiet;
         when Quiet =>
            return Verbosity = Quiet;
      end case;
   end Is_Authorised;

end BBT.Settings;
