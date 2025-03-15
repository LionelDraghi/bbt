-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Strings.Text_Buffers;
with Ada.Strings.Unbounded;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Calendar;

private package BBT.IO is

   -- --------------------------------------------------------------------------
   type Verbosity_Levels is (Quiet, Normal, Verbose, Debug);
   -- NB : order is significant, X is more verbose thant X'Pred!
   -- Default: Normal messages are displayed, verbose messages are not
   --          displayed.
   -- Quiet:   Neither normal messages nor verbose messages are displayed.
   --          This mode can be achieved using option --quiet.
   -- Verbose: Both normal messages and verbose messages are displayed.
   --          This mode can be achieved using option --verbose.
   function Is_Authorized (Verbosity : Verbosity_Levels) return Boolean;
   -- return True if Verbosity is >= to current setting.

   function Current_Verbosity return Verbosity_Levels;
   procedure Set_Verbosity (To : Verbosity_Levels);
   function Debug_Mode   return Boolean is (Current_Verbosity = Debug);
   function Verbose_Mode return Boolean is (Current_Verbosity = Verbose);
   function Normal_Mode  return Boolean is (Current_Verbosity = Normal);
   function Quiet_Mode   return Boolean is (Current_Verbosity = Quiet);

   -- --------------------------------------------------------------------------
   -- Observability! (kind of a tentative of...)
   type Extended_Topics is (None, -- keep as first enum (see Topics declaration)
                            Created_Files,
                            FSM,
                            Lexer,
                            MD_Writer,
                            -- Output,
                            Runner,
                            Scen_Files,
                            Spawn,
                            Step_Actions,
                            Step_Lexer,
                            Tests_Builder,
                            Writers);
   subtype Topics is Extended_Topics range
     Extended_Topics'Succ (None) .. Extended_Topics'Last;
   -- None is the default parameter for IO operation, but is not in Topics
   -- range, used when setting what should be printed.
   procedure Enable_Topic (Topic : Topics);
   function Is_Enabled (Topic : Extended_Topics) return Boolean;
   -- return always false for None

   -- --------------------------------------------------------------------------
   type Location_Type is private with Put_Image => Location_GNU_Image;
   procedure Location_GNU_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Location_Type);
   No_Location : constant Location_Type;

   function File   (Loc : Location_Type) return String;
   function Line   (Loc : Location_Type) return Ada.Text_IO.Count;
   function Column (Loc : Location_Type) return Ada.Text_IO.Count;

   -- --------------------------------------------------------------------------
   function Image (Loc : Location_Type) return String;
   --  Purpose:
   --    returns a source/line/column prefix to messages compatible with
   --    GNU Standard "sourcefile:lineno:column: message" format.
   --    (refer to <https://www.gnu.org/prep/standards/html_node/Errors.html>),
   --
   --    That is :
   --       > "sourcefile:lineno:"
   --    if column = 0 or 1,
   --    or
   --       > "sourcefile:lineno.column:"
   --    otherwise.

   -- --------------------------------------------------------------------------
   function Location (Name   : String;
                      Line   : Ada.Text_IO.Count;
                      Column : Ada.Text_IO.Count := 0) return Location_Type;
   -- When manual adjustment is needed.

   function Location (File : Ada.Text_IO.File_Type) return Location_Type;
   -- Automatic Line/Col through Text_IO calls

   -- --------------------------------------------------------------------------
   procedure Set_Reference_Directory (Dir_Name : String);
   --  Register a path that will be removed from Image, to avoid long absolute
   --  Paths.

   -- --------------------------------------------------------------------------
   -- Mimics eponym Text_IO functions, except that :
   --   - if --quiet is set on command line, they have no effect,
   --     unless Even_In_Quiet_Mode is set.
   --   - if Only_When_Verbose is False, they have no effect
   --     unless --verbose is set on command line.
   --   - if a Topic is given, and this topic is enable, previous parameter
   --     are ignored and the Item is printed.
   procedure Put_Line (Item      : String;
                       File      : String           := "";
                       Line      : Ada.Text_IO.Count;
                       Verbosity : Verbosity_Levels := Normal;
                       Topic     : Extended_Topics  := None);
   procedure Put (Item      : String;
                  File      : String           := "";
                  Line      : Ada.Text_IO.Count;
                  Verbosity : Verbosity_Levels := Normal;
                  Topic     : Extended_Topics  := None);
   procedure New_Line (Verbosity : Verbosity_Levels := Normal;
                       Topic     : Extended_Topics  := None);
   procedure Put_Line (Item      : String;
                       Location  : Location_Type := No_Location;
                       Verbosity : Verbosity_Levels := Normal;
                       Topic     : Extended_Topics  := None);
   procedure Put (Item      : String;
                  Location  : Location_Type := No_Location;
                  Verbosity : Verbosity_Levels := Normal;
                  Topic     : Extended_Topics  := None);

   -- --------------------------------------------------------------------------
   procedure Put_Warning   (Msg  : String;
                            File : String := "";
                            Line :  Ada.Text_IO.Count  := 0);
   procedure Put_Error     (Msg  : String;
                            File : String := "";
                            Line :  Ada.Text_IO.Count  := 0);
   procedure Put_Exception (Msg  : String;
                            File : String := "";
                            Line :  Ada.Text_IO.Count  := 0);
   procedure Put_Warning   (Msg      : String;
                            Location : Location_Type);
   procedure Put_Error     (Msg      : String;
                            Location : Location_Type);
   procedure Put_Exception (Msg      : String;
                            Location : Location_Type);

   -- --------------------------------------------------------------------------
   procedure Enable_Tee (File_Name : String;
                         Verbosity : Verbosity_Levels := Normal);
   -- Enable the duplication af all Put/Put_Line/etc. in a file.
   -- The Verbosity may be different for standard output and for the file.
   -- This is a simple way to have (for example) a terse standard output,
   -- and a verbose log file.
   procedure Pause_Tee;
   -- Store the current state and pause Tee.
   procedure Restore_Tee;
   -- Restore the state before last Pause_Tee call.

   -- --------------------------------------------------------------------------
   -- Error_Count and Warning_Count return the number of call to Put_Error
   -- and Put_Warning.
   function Error_Count   return Natural;
   function Warning_Count return Natural;

   -- --------------------------------------------------------------------------
   -- Some_Error return True if some error occurred, or if some Warning
   -- occurred and option to treat warning as error is set.
   function Some_Error return Boolean;
   function No_Error return Boolean is (not Some_Error);

   -- --------------------------------------------------------------------------
   function Image (Time : Ada.Calendar.Time) return String;
   -- To ensure common options to call to Ada.Calendar.Formatting.Image,
   -- that is:
   -- 1. Include_Time_Fraction => True,
   --    to get hundredths of a second printed in the hope that
   --    it will be useful in future version, or on other OSes.
   -- 2. Time_Zone             => Ada.Calendar.Time_Zones.UTC_Time_Offset
   --    to get the same file time tag printed in local time that the user
   --    would see by making a simple ls -l

   -- --------------------------------------------------------------------------
   use Ada.Strings.Unbounded;
   type File_Name is new Unbounded_String;
   function "+" (Name : File_Name) return String is (To_String (Name));
   function "+" (Name : String) return File_Name is
     (File_Name'(To_Unbounded_String (Name)));
   No_Name : constant File_Name := +"";

private
   type Location_Type is record
      File   : File_Name         := No_Name;
      Line   : Ada.Text_IO.Count := 1;
      Column : Ada.Text_IO.Count := 0;
   end record;

   No_Location : constant Location_Type := (No_Name, 1, 0);

end BBT.IO;
