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

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body BBT.IO is

   Warnings : Natural := 0;

   -- --------------------------------------------------------------------------
   -- Function: GNU_Prefix
   --
   -- Purpose:
   --    This function return a source/line/column prefix to messages compatible
   --    whith GNU Standard
   --    (refer to <https://www.gnu.org/prep/standards/html_node/Errors.html>),
   --    That is :
   --       > program:sourcefile:lineno: message
   --    when there is an appropriate source file, or :
   --       > program: message
   --    otherwise.
   --
   -- --------------------------------------------------------------------------
   function GNU_Prefix (File   : String;
                        Line   : Integer := 0) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Trimed_File   : constant String := Trim (File, Side => Both);
      Trimed_Line   : constant String := Trim (Positive'Image (Line),
                                               Side => Both);
      Common_Part : constant String := "bbt:" & Trimed_File;
   begin
      if File = "" then
         return "";
      elsif Line = 0 then
         return Common_Part & " ";
      else
         return Common_Part & ":" & Trimed_Line & ": ";
      end if;
   end GNU_Prefix;

   -- --------------------------------------------------------------------------
   function Prefix (Topic : Extended_Topics) return String is
     (if Topic = None then "" else Topic'Image & " ");

   -- --------------------------------------------------------------------------
   procedure Put_Warning (Msg  : String;
                          File : String  := "";
                          Line : Integer := 0) is
   begin
      Warnings := Warnings + 1;
      Put_Line ("Warning : " & Msg, File, Line);
      -- use the local version of Put_Line, and not the Ada.Text_IO one,
      -- so that Warning messages are also ignored when --quiet.
   end Put_Warning;

   Errors : Natural := 0;

   -- --------------------------------------------------------------------------
   procedure Put_Error (Msg  : String;
                        File : String  := "";
                        Line : Integer := 0) is
   begin
      Errors := Errors + 1;
      Put_Line ("Error : " & Msg, File, Line, Level => Quiet);
      -- Quiet because Error Msg should not be ignored
   end Put_Error;

   -- --------------------------------------------------------------------------
   procedure Put_Exception (Msg  : String;
                            File : String  := "";
                            Line : Integer := 0) is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            GNU_Prefix (File, Line) & "Exception : " & Msg);
   end Put_Exception;

   -- --------------------------------------------------------------------------
   function Error_Count   return Natural is (Errors);
   function Warning_Count return Natural is (Warnings);

   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : String;
                             Debug  : Boolean;
                             Prefix : String;
                             File   : String  := "";
                             Line   : Integer := 0) is
   begin
      if Debug then
         Ada.Text_IO.Put_Line (GNU_Prefix (File, Line) & Prefix & Msg);
      end if;
   end Put_Debug_Line;

   -- --------------------------------------------------------------------------
   procedure Put_Line (Item  : String;
                       File  : String       := "";
                       Line  : Integer      := 0;
                       Level : Print_Out_Level := Normal;
                       Topic : Extended_Topics := None) is
   begin
      if Level >= Settings.Verbosity or else Is_Enabled (Topic) then
         Ada.Text_IO.Put_Line (GNU_Prefix (File, Line) & Prefix (Topic) & Item);
      end if;
   end Put_Line;

   -- --------------------------------------------------------------------------
   procedure Put (Item  : String;
                  File  : String       := "";
                  Line  : Integer      := 0;
                  Level : Print_Out_Level := Normal;
                  Topic : Extended_Topics := None) is
   begin
      if Level >= Settings.Verbosity or else Is_Enabled (Topic) then
         Ada.Text_IO.Put (GNU_Prefix (File, Line) & Prefix (Topic) & Item);
      end if;
   end Put;

   -- --------------------------------------------------------------------------
   procedure New_Line (Level : Print_Out_Level := Normal;
                       Topic : Extended_Topics := None) is
   begin
      if Level >= Settings.Verbosity or else Is_Enabled (Topic) then
         Ada.Text_IO.New_Line;
      end if;
   end New_Line;

   -- --------------------------------------------------------------------------
   function Image (Time : Ada.Calendar.Time) return String is
   begin
      return Ada.Calendar.Formatting.Image
        (Date                  => Time,
         Include_Time_Fraction => True,
         Time_Zone             => Ada.Calendar.Time_Zones.UTC_Time_Offset);
   end Image;

end BBT.IO;
