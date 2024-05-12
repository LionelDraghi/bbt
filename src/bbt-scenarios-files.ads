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

with BBT.Settings;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;

package BBT.Scenarios.Files is
-- Provide services related to the Scenario files list, that is mainly to :
-- - identify (possibly recursively) Scenario files in a directory;
-- - manually add files to the list.

   -- --------------------------------------------------------------------------
   type File_Name is new Unbounded_String;
   function "+" (Name : File_Name) return String;
   function "+" (Name : String)    return File_Name;
   No_File : constant File_Name
     := File_Name (Ada.Strings.Unbounded.Null_Unbounded_String);

   -- --------------------------------------------------------------------------
   package File_List is new Ada.Containers.Indefinite_Vectors (Positive,
                                                               String);
   procedure Append_File (File_Name : String);

   -- --------------------------------------------------------------------------
   procedure Find_BBT_Files
     (Recursive   : Boolean;
      Start_In    : String := "./";
      Remove_Root : String := Settings.Initial_Directory);
   function No_BBT_File return Boolean;
   function BBT_Files return File_List.Vector;

   -- --------------------------------------------------------------------------
   procedure Analyze_MDG_File (File_Name : String);

end BBT.Scenarios.Files;
