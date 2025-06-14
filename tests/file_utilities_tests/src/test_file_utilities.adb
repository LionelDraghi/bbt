-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with File_Utilities;

with Ada.Command_Line;
with Ada.Text_IO;      -- use Ada.Text_IO;

procedure Test_File_Utilities is

   Failure_Count : Natural   := 0;
   Check_Idx     : Positive  := 1;
   Quiet         : Boolean := False;
   -- Quiet = True output is limited to the summary

   -- --------------------------------------------------------------------------
   procedure Put       (Item : String) is
   begin
      if not Quiet then Ada.Text_IO.Put (Item);
      end if;
   end Put;

   -- --------------------------------------------------------------------------
   procedure Put_Line  (Item : String)  is
   begin
      if not Quiet then Ada.Text_IO.Put_Line (Item);
      end if;
   end Put_Line;

   -- --------------------------------------------------------------------------
   procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1) is
   begin
      if not Quiet then Ada.Text_IO.New_Line (Spacing);
      end if;
   end New_Line;

   -- --------------------------------------------------------------------------
   function On_Windows return Boolean is (File_Utilities.Separator = '\');

   -- --------------------------------------------------------------------------
   procedure Check (Title    : String;
                    From_Dir : String;
                    To_File  : String;
                    Prefix   : String := "";
                    Expected : String)

     is
      Tmp    : constant String := Positive'Image (Check_Idx);
      Idx    : constant String := Tmp (2 .. Tmp'Last);
      use File_Utilities;
      Result : constant String := Short_Path (From_Dir => From_Dir,
                                              To_File  => To_File,
                                              Prefix   => Prefix);

   begin
      Put (Idx & ". " & Title);
      Check_Idx := Check_Idx + 1;

      if Result /= Expected then Quiet := False; end if;

      if Result = Expected then
         Put (" : OK");
      else
         Put (" : NOK ****");
         Failure_Count := Failure_Count + 1;
      end if;
      New_Line;
      Put ("Short_Path (From_Dir => """ & From_Dir & """,");
      New_Line;
      Put ("            To_File  => """ & To_File  & """");
      if Prefix /= "" then
         Put (",");
         New_Line;
         Put ("            Prefix   => """ & Prefix & """");
      end if;
      Put (") = " & Result);
      New_Line;
      if Result /= Expected then
         Put_Line ("Expected " & Expected);
      end if;
      New_Line;
   end Check;

   -- --------------------------------------------------------------------------
   procedure Check (Title               : String;
                    From_Dir            : String;
                    To_File             : String;
                    Prefix              : String := "";
                    Expected_On_Unix    : String;
                    Expected_On_Windows : String) is
      Expected : constant String := (if On_Windows then Expected_On_Windows
                                     else Expected_On_Unix);
   begin
      Check (Title               => Title,
             From_Dir            => From_Dir,
             To_File             => To_File,
             Prefix              => Prefix,
             Expected            => Expected);
   end Check;

begin
   -- --------------------------------------------------------------------------
   if Ada.Command_Line.Argument_Count /= 0 then
      if Ada.Command_Line.Argument_Count = 1 and
        Ada.Command_Line.Argument (1) = "-q"
      then
         Quiet := True;
      else
         Ada.Text_IO.Put_Line
           ("Wrong command line : only one possible option, -q");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;
   end if;

   Put_Line ("# File_Utilities.Short_Path unit tests");
   New_Line;

   -- --------------------------------------------------------------------------
   if On_Windows then
      Check (Title    => "Subdir with default Prefix",
             From_Dir => "c:\home\tests",
             To_File  => "c:\home\tests\mysite\site\d1\idx.txt",
             Expected => "mysite\site\d1\idx.txt");
   else
      Check (Title    => "Subdir with default Prefix",
             From_Dir => "/home/tests",
             To_File  => "/home/tests/mysite/site/d1/idx.txt",
             Expected => "mysite/site/d1/idx.txt");
   end if;

   if On_Windows then
      Check (Title    => "Case sensitivity",
          From_Dir => "c:\Users\Lionel\",
          To_File  => "C:\USERS\Xavier\PrOJ",
          Expected => "..\Xavier\PrOJ");
   else
      Check (Title    => "Case sensitivity",
             From_Dir => "/home/tests",
             To_File  => "/home/Tests/mysite/site/d1/idx.txt",
             Expected => "../Tests/mysite/site/d1/idx.txt");
   end if;

   if On_Windows then
      Check (Title    => "Dir with final /",
            From_Dir => "/home/tests/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Expected => "mysite\site\d1\idx.txt");
   else
      Check (Title    => "Dir with final /",
            From_Dir => "/home/tests/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Expected => "mysite/site/d1/idx.txt");
   end if;

   if On_Windows then
      Check (Title    => "subdir with Prefix",
             From_Dir => "\home\tests",
             To_File  => "\home\tests\mysite\site\d1\idx.txt",
             Prefix   => "." & File_Utilities.Separator,
             Expected => ".\mysite\site\d1\idx.txt");
   else
      Check (Title    => "subdir with Prefix",
             From_Dir => "/home/tests",
             To_File  => "/home/tests/mysite/site/d1/idx.txt",
             Prefix   => "." & File_Utilities.Separator,
             Expected => "./mysite/site/d1/idx.txt");
   end if;

   if On_Windows then
      Check (Title    => "Sibling subdir",
             From_Dir => "\home\tests\12\34",
             To_File  => "\home\tests\mysite\site\d1\idx.txt",
             Expected => "..\..\mysite\site\d1\idx.txt");
   else
      Check (Title               => "Sibling subdir",
             From_Dir            => "/home/tests/12/34",
             To_File             => "/home/tests/mysite/site/d1/idx.txt",
             Expected            => "../../mysite/site/d1/idx.txt");

   end if;

   if On_Windows then
      Check (Title    => "Parent dir",
             From_Dir => "\home\tests\12\34",
             To_File  => "\home\tests\idx.txt",
             Expected => "..\..\idx.txt");
   else
      Check (Title    => "Parent dir",
             From_Dir => "/home/tests/12/34",
             To_File  => "/home/tests/idx.txt",
             Expected => "../../idx.txt");
   end if;

   if On_Windows then
      Check (Title    => "Other Prefix",
             From_Dir => "\home\tests\12\",
             To_File  => "\home\tests\mysite\site\d1\idx.txt",
             Prefix   => "$PWD\",
             Expected => "$PWD\..\mysite\site\d1\idx.txt");
   else
      Check (Title    => "Other Prefix",
             From_Dir => "/home/tests/12/",
             To_File  => "/home/tests/mysite/site/d1/idx.txt",
             Prefix   => "$PWD/",
             Expected => "$PWD/../mysite/site/d1/idx.txt");
   end if;

   if On_Windows then
      Check (Title    => "Root dir",
             From_Dir => "d:",
             To_File  => "d:\home\tests\mysite\site\d1\idx.txt",
             Expected => "\home\tests\mysite\site\d1\idx.txt");
   else
      Check (Title    => "Root dir",
             From_Dir => "/",
             To_File  => "/home/tests/mysite/site/d1/idx.txt",
             Expected => "/home/tests/mysite/site/d1/idx.txt");
   end if;

   Check (Title    => "File is over dir",
          From_Dir => "/home/tests/mysite/site/d1",
          To_File  => "/home/readme.txt",
          Expected_On_Unix => "../../../../readme.txt",
          Expected_On_Windows => "..\..\..\..\readme.txt");

   Check (Title    => "File is over Dir, Dir with final /",
          From_Dir => "/home/tests/mysite/site/d1/",
          To_File  => "/home/readme.txt",
          Expected_On_Unix =>  "../../../../readme.txt",
          Expected_On_Windows => "..\..\..\..\readme.txt");

   Check (Title    => "File is the current dir",
          From_Dir => "/home/tests/",
          To_File  => "/home/tests",
          Expected_On_Unix => "./",
          Expected_On_Windows => ".\");

   Check (Title    => "File is over Dir, Dir and File with final /",
          From_Dir => "/home/tests/",
          To_File  => "/home/tests/",
          Expected_On_Unix => "./",
          Expected_On_Windows => ".\");

   if On_Windows then
      Check (Title    => "No common part",
             From_Dir => "\home\toto\src\tests\",
             To_File  => "\opt\GNAT\2018\lib64\libgcc_s.so",
             Expected => "\opt\GNAT\2018\lib64\libgcc_s.so");
   else
      Check (Title    => "No common part",
             From_Dir => "/home/toto/src/tests/",
             To_File  => "/opt/GNAT/2018/lib64/libgcc_s.so",
             Expected => "/opt/GNAT/2018/lib64/libgcc_s.so");
   end if;

   if On_Windows then
      -- lets run Windows specific tests
      Check (Title    => "Windows Path",
             From_Dir => "c:\Users\Lionel\",
             To_File  => "c:\Users\Xavier\Proj",
             Expected => "..\Xavier\Proj");
      Check (Title    => "Windows Path",
             From_Dir => "x:",
             To_File  => "Xavier\Proj",
             Expected => "Xavier\Proj");
      Check (Title    => "Windows Path, not on the same drive",
             From_Dir => "c:\Users\Lionel\",
             To_File  => "d:\Users\Xavier\Proj",
             Expected => "d:\Users\Xavier\Proj");
      Check (Title    => "Windows Path, case sensitivity",
             From_Dir => "c:\Users\Lionel\",
             To_File  => "C:\USERS\Xavier\PrOJ",
             Expected => "..\Xavier\PrOJ");
      Check (Title    => "UNC Path",
             From_Dir => "\\Volume\Server\Users\Lionel\",
             To_File  => "\\Volume\Server\USERS\Xavier\Proj",
             Expected => "..\Xavier\Proj");
      Check (Title    => "Mixing both separator uses",
             From_Dir => "c:/Users/Lionel/tmp",
             To_File  => "c:\Users\Lionel\Proj\truc",
             Expected => "..\Proj\truc");
   end if;

   -- --------------------------------------------------------------------------
   Quiet := False;
   if Failure_Count /= 0 then
      New_Line;
      Put_Line (Natural'Image (Failure_Count) & " tests fails");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      New_Line;
      Put_Line ("File_Utilities.Short_Path tests OK");
   end if;

end Test_File_Utilities;
