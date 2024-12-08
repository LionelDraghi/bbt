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

   procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1) is
   begin
      if not Quiet then Ada.Text_IO.New_Line (Spacing);
      end if;
   end New_Line;

   -- --------------------------------------------------------------------------
   procedure Check (Title    : String;
                    From_Dir : String;
                    To_File  : String;
                    Prefix   : String := "";
                    -- Result   : String;
                    Expected : String) is
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
   Check (Title    => "Subdir with default Prefix",
          From_Dir => "/home/tests",
          To_File  => "/home/tests/mysite/site/d1/idx.txt",
          Expected => "mysite/site/d1/idx.txt");

   Check (Title    => "Dir with final /",
          From_Dir => "/home/tests/",
          To_File  => "/home/tests/mysite/site/d1/idx.txt",
          Expected => "mysite/site/d1/idx.txt");

   Check (Title    => "subdir with Prefix",
          From_Dir => "/home/tests",
          To_File  => "/home/tests/mysite/site/d1/idx.txt",
          Prefix   => "." & File_Utilities.Separator,
          Expected => "./mysite/site/d1/idx.txt");

   Check (Title    => "Sibling subdir",
          From_Dir => "/home/tests/12/34",
          To_File  => "/home/tests/mysite/site/d1/idx.txt",
          Expected => "../../mysite/site/d1/idx.txt");

   Check (Title    => "Parent dir",
          From_Dir => "/home/tests/12/34",
          To_File  => "/home/tests/idx.txt",
          Expected => "../../idx.txt");

   Check (Title    => "Other Prefix",
          From_Dir => "/home/tests/12/",
          To_File  => "/home/tests/mysite/site/d1/idx.txt",
          Prefix   => "$PWD/",
          Expected => "$PWD/../mysite/site/d1/idx.txt");

   Check (Title    => "Root dir",
          From_Dir => "/",
          To_File  => "/home/tests/mysite/site/d1/idx.txt",
          Expected => "/home/tests/mysite/site/d1/idx.txt");

   Check (Title    => "File is over dir",
          From_Dir => "/home/tests/mysite/site/d1",
          To_File  => "/home/readme.txt",
          Expected => "../../../../readme.txt");

   Check (Title    => "File is over Dir, Dir with final /",
          From_Dir => "/home/tests/mysite/site/d1/",
          To_File  => "/home/readme.txt",
          Expected => "../../../../readme.txt");

   Check (Title    => "File is the current dir",
          From_Dir => "/home/tests/",
          To_File  => "/home/tests",
          Expected => "./");

   Check (Title    => "File is over Dir, Dir and File with final /",
          From_Dir => "/home/tests/",
          To_File  => "/home/tests/",
          Expected => "./");

   Check (Title    => "No common part",
          From_Dir => "/home/toto/src/tests/",
          To_File  => "/opt/GNAT/2018/lib64/libgcc_s.so",
          Expected => "/opt/GNAT/2018/lib64/libgcc_s.so");

   if File_Utilities.Separator = '\' then
      -- lets run Windows specific tests
      Check (Title    => "Windows Path",
             From_Dir => "c:\Users\Lionel\",
             To_File  => "c:\Users\Xavier\Proj",
             Expected => "..\Xavier\Proj");
      Check (Title    => "Windows Path, case sensitivity",
             From_Dir => "c:\Users\Lionel\",
             To_File  => "c:\USERS\Xavier\Proj",
             Expected => "..\Xavier\Proj");
      Check (Title    => "UNC Path",
             From_Dir => "\\Volume\Server\Users\Lionel\",
             To_File  => "\\Volume\Server\USERS\Xavier\Proj",
             Expected => "..\Xavier\Proj");
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
