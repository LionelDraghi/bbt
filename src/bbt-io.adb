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

with File_Utilities;       use File_Utilities;

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

package body BBT.IO is

   -- --------------------------------------------------------------------------
   Errors_Count       : Natural         := 0;
   Warnings_Count     : Natural         := 0;
   Tee_Enabled        : Boolean         := False;
   Tee_File_Verbosity : Verbosity_Levels;
   Tee_File           : Ada.Text_IO.File_Type;
   Current_Level      : Verbosity_Levels := Normal;

   -- --------------------------------------------------------------------------
   function Is_Authorized (Verbosity : Verbosity_Levels) return Boolean is
     (Verbosity <= Current_Level);

   function Current_Verbosity return Verbosity_Levels is (Current_Level);
   procedure Set_Verbosity (To : Verbosity_Levels) is
   begin
      Current_Level := To;
   end Set_Verbosity;

   -- --------------------------------------------------------------------------
   Enabled_Topics : array (Extended_Topics) of Boolean := [others => False];

   -- --------------------------------------------------------------------------
   procedure Enable_Topic (Topic : Topics) is
   begin
      Enabled_Topics (Topic) := True;
   end Enable_Topic;

   function Is_Enabled (Topic : Extended_Topics) return Boolean is
     (Topic in Topics and then Enabled_Topics (Topic));

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
   function GNU_Prefix (File : String;
                        Line : Ada.Text_IO.Count := 0) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Trimmed_File   : constant String := Trim (File, Side => Both);
      Trimmed_Line   : constant String := Trim (Ada.Text_IO.Count'Image (Line),
                                                Side => Both);
      Common_Part   : constant String := "bbt:" & Trimmed_File;
   begin
      if File = "" then
         return "";
      elsif Line = 0 then
         return Common_Part & " ";
      else
         return Common_Part & ":" & Trimmed_Line & ": ";
      end if;
   end GNU_Prefix;

   -- --------------------------------------------------------------------------
   function Image (Loc : Location_Type) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Trimmed_Line   : constant String := Trim (Loc.Line'Image,   Side => Left);
      Trimmed_Column : constant String := Trim (Loc.Column'Image, Side => Left);
   begin
      if Loc.File = "" then
         return "";
      elsif Loc.Column = 0 or Loc.Column = 1 then
         return +Loc.File & ":" & Trimmed_Line & ":";
      else
         return +Loc.File & ":" & Trimmed_Line & "." & Trimmed_Column & ":";
      end if;
   end Image;

   -- --------------------------------------------------------------------------
   procedure Put_Warning (Msg  : String;
                          File : String := "";
                          Line :  Ada.Text_IO.Count  := 0) is
   begin
      Warnings_Count := @ + 1;
      Put_Line ("Warning : " & Msg, File, Line);
      -- use the local version of Put_Line, and not the Ada.Text_IO one,
      -- so that Warning messages are also ignored when --quiet.
   end Put_Warning;

   -- --------------------------------------------------------------------------
   procedure Put_Error (Msg  : String;
                        File : String := "";
                        Line :  Ada.Text_IO.Count  := 0) is
   begin
      Errors_Count := @ + 1;
      Put_Line ("Error : " & Msg, File, Line, Verbosity => Quiet);
      -- Quiet because Error Msg should not be ignored
   end Put_Error;

   -- --------------------------------------------------------------------------
   procedure Put_Exception (Msg  : String;
                            File : String  := "";
                            Line :  Ada.Text_IO.Count := 0) is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            GNU_Prefix (File => File,
                                        Line => Line)
                            & "Exception : " & Msg);
   end Put_Exception;

   procedure Put_Warning   (Msg      : String;
                            Location : Location_Type) is
   begin
      Put_Warning (Msg, +Location.File, Location.Line);
   end Put_Warning;

   procedure Put_Error     (Msg      : String;
                            Location : Location_Type) is
   begin
      Put_Error (Msg, +Location.File, Location.Line);
   end Put_Error;

   procedure Put_Exception (Msg      : String;
                            Location : Location_Type) is
   begin
      Put_Exception (Msg, +Location.File, Location.Line);
   end Put_Exception;

   -- --------------------------------------------------------------------------
   procedure Enable_Tee (File_Name     : String;
                         Verbosity     : Verbosity_Levels := Normal) is
   begin
      Tee_Enabled := True;
      Tee_File_Verbosity   := Verbosity;
      Create (Tee_File, Name => File_Name, Mode => Out_File);
   end Enable_Tee;

   -- --------------------------------------------------------------------------
   function Error_Count   return Natural is (Errors_Count);
   function Warning_Count return Natural is (Warnings_Count);
   function Some_Error return Boolean is
     (Error_Count /= 0 or (Settings.Warnings_As_Errors and Warning_Count /= 0));

    -- --------------------------------------------------------------------------
   type String_Access is access String;
   procedure Dealloc_String is new Ada.Unchecked_Deallocation
     (Object => String, Name => String_Access);
   Ref_Dir : String_Access := null;

   -- --------------------------------------------------------------------------
   procedure Location_GNU_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Location_Type) is
   begin
      Output.Put (Image (Value));
   end Location_GNU_Image;

   -- --------------------------------------------------------------------------
   function Location (Name   : String;
                      Line   : Ada.Text_IO.Count;
                      Column : Ada.Text_IO.Count := 0) return Location_Type is
   begin
      if Name = "" then
         return No_Location;
      else
         return (File   => +Short_Path (From_Dir => Ref_Dir.all,
                                        To_File  => Name),
                 --                     Prefix   => "./"),
                 Line   => Line,
                 Column => Column);
      end if;
   end Location;

   -- --------------------------------------------------------------------------
   function Location (File : Ada.Text_IO.File_Type) return Location_Type is
   begin
      if Is_Open (File) then
         return (File   => +Short_Path (From_Dir => Ref_Dir.all,
                                        To_File  => Name (File)),
                 --                     Prefix   => "./"),
                 Line   => Line (File),
                 Column => Col  (File));
      else
         return No_Location;
      end if;
   end Location;

   -- --------------------------------------------------------------------------
   procedure Set_Reference_Directory (Dir_Name : String) is
   begin
      -- Not supposed to be called more than once, but who knows,
      -- lets clean up first!
      Dealloc_String (Ref_Dir);
      Ref_Dir := new String'(Dir_Name);
   end Set_Reference_Directory;

   -- --------------------------------------------------------------------------
   procedure Put_Line (Item      : String;
                       File      : String           := "";
                       Line      : Ada.Text_IO.Count;
                       Verbosity : Verbosity_Levels := Normal;
                       Topic     : Extended_Topics  := None) is
   begin
      Put_Line (Item, Location (File, Line), Verbosity, Topic);
   end Put_Line;

   -- --------------------------------------------------------------------------
   procedure Put (Item      : String;
                  File      : String           := "";
                  Line      : Ada.Text_IO.Count;
                  Verbosity : Verbosity_Levels := Normal;
                  Topic     : Extended_Topics  := None) is
   begin
      Put (Item, Location (File, Line), Verbosity, Topic);
   end Put;

   -- --------------------------------------------------------------------------
   procedure New_Line (Verbosity : Verbosity_Levels := Normal;
                       Topic     : Extended_Topics := None)
   is
      Print_On_Standard_Output : constant Boolean
        := Verbosity <= Current_Verbosity or else Is_Enabled (Topic);
      Print_In_Tee_File        : constant Boolean
        := Tee_Enabled and then
            (Verbosity <= Tee_File_Verbosity or else Is_Enabled (Topic));
   begin
      if Print_On_Standard_Output then
         Ada.Text_IO.New_Line;
      end if;
      if Print_In_Tee_File then
         Ada.Text_IO.New_Line (Tee_File);
      end if;
   end New_Line;

   -- --------------------------------------------------------------------------
   procedure Put_Line (Item      : String;
                       Location  : Location_Type    := No_Location;
                       Verbosity : Verbosity_Levels := Normal;
                       Topic     : Extended_Topics  := None) is
      Print_On_Standard_Output : constant Boolean
        := Verbosity <= Current_Verbosity or else Is_Enabled (Topic);
      Print_In_Tee_File        : constant Boolean
        := Tee_Enabled and then
            (Verbosity <= Tee_File_Verbosity or else Is_Enabled (Topic));
      Prefix                   : constant String :=
                                   (if Location'Image = "" then ""
                                    else Location'Image & " ");
   begin
      if Print_On_Standard_Output then
         Ada.Text_IO.Put_Line (Prefix & Item);
      end if;
      if Print_In_Tee_File then
         Ada.Text_IO.Put_Line (Tee_File, Prefix & Item);
      end if;
   end Put_Line;

   -- --------------------------------------------------------------------------
   procedure Put (Item      : String;
                  Location  : Location_Type    := No_Location;
                  Verbosity : Verbosity_Levels := Normal;
                  Topic     : Extended_Topics  := None) is
      Print_On_Standard_Output : constant Boolean
        := Verbosity <= Current_Verbosity or else Is_Enabled (Topic);
      Print_In_Tee_File        : constant Boolean := Tee_Enabled
        and then (Verbosity <= Tee_File_Verbosity or else Is_Enabled (Topic));
      Prefix                   : constant String :=
                                   (if Location'Image = "" then ""
                                    else Location'Image & " ");
   begin
      if Print_On_Standard_Output then
         Ada.Text_IO.Put (Prefix & Item);
      end if;
      if Print_In_Tee_File then
         Ada.Text_IO.Put (Tee_File, Prefix & Item);
      end if;
   end Put;

   -- --------------------------------------------------------------------------
   function Image (Time : Ada.Calendar.Time) return String is
   begin
      return Ada.Calendar.Formatting.Image
        (Date                  => Time,
         Include_Time_Fraction => True,
         Time_Zone             => Ada.Calendar.Time_Zones.UTC_Time_Offset);
   end Image;

end BBT.IO;
