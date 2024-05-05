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

with Ada.Calendar;

with BBT.Settings; use BBT.Settings;

private package BBT.IO is

   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : String;
                             Debug  : Boolean;
                             Prefix : String;
                             File   : String  := "";
                             Line   : Integer := 0);

   -- --------------------------------------------------------------------------
   subtype Print_Out_Level is Settings.Print_Out_Level;
   Debug   : constant Print_Out_Level := Settings.Debug;
   Verbose : constant Print_Out_Level := Settings.Verbose;
   Normal  : constant Print_Out_Level := Settings.Normal;
   Quiet   : constant Print_Out_Level := Settings.Quiet;

   -- --------------------------------------------------------------------------
   -- Mimics eponym Text_IO functions, except that :
   --   - if --quiet is set on command line, they have no effect,
   --     unless Even_In_Quiet_Mode is set.
   --   - if Only_When_Verbose is False, they have no effect
   --     unless --verbose is set on command line.
   --   - if a Topic is given, and this topic is enable, previous parameter
   --     are ignored and the Item is printed.
   procedure Put_Line (Item  : String;
                       File  : String  := "";
                       Line  : Integer := 0;
                       Level : Print_Out_Level := Normal;
                       Topic : Extended_Topics := None);
   procedure Put (Item  : String;
                  File  : String  := "";
                  Line  : Integer := 0;
                  Level : Print_Out_Level := Normal;
                  Topic : Extended_Topics := None);
   procedure New_Line (Level : Print_Out_Level := Normal;
                       Topic : Extended_Topics := None);

   -- --------------------------------------------------------------------------
   procedure Put_Warning   (Msg  : String;
                            File : String  := "";
                            Line : Integer := 0);
   procedure Put_Error     (Msg  : String;
                            File : String  := "";
                            Line : Integer := 0);
   procedure Put_Exception (Msg  : String;
                            File : String  := "";
                            Line : Integer := 0);

   -- --------------------------------------------------------------------------
   procedure Enable_Tee (File_Name : String;
                         Level     : Print_Out_Level := Normal);
   -- Enable the duplication af all Put/Put_Line/etc. in a file.
   -- The file should contains exactly what would be the standard output with
   -- the same Level settings.
   -- This is a simple way to have (for example) a terse standard output,
   -- and a verbose log file.

   -- --------------------------------------------------------------------------
   -- Error_Count and Warning_Count return the number of call to Put_Error
   -- and Put_Warning.
   function Error_Count   return Natural;
   function Warning_Count return Natural;

   -- --------------------------------------------------------------------------
   -- Some_Error return True if some error occured, or if some Warning
   -- occured and option to treat warning as error is set.
   function Some_Error return Boolean is
     (Error_Count /= 0 or
        (Settings.Warnings_As_Errors and Warning_Count /= 0));

   -- --------------------------------------------------------------------------
   function Image (Time : Ada.Calendar.Time) return String;
   -- To ensure common options to call to Ada.Calendar.Formatting.Image,
   -- that is:
   -- 1. Include_Time_Fraction => True,
   --    to get hundredths of a second printed in the hope that
   --    it will be useful in future version, or on other OSes.
   -- 2. Time_Zone             => Ada.Calendar.Time_Zones.UTC_Time_Offset
   --    to get the same file time tag printed in local time that the user
   --     would see by making a simple ls -l

end BBT.IO;
