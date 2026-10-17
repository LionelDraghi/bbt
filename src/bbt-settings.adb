-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;

with GNAT.OS_Lib;

package body BBT.Settings is

   -- --------------------------------------------------------------------------
   -- Most of the variable here are "write once, read more".
   -- To avoid the cost of Unbounded strings manipulation,
   -- they are implemented as access to String, allocated once and
   -- never deallocated.
   Launch_Dir : constant access String :=
     new String'(Ada.Directories.Current_Directory);
   Outfile_Name,
   Exec_Dir_Name,
   Tmp_Dir_Name,
   Badge_Name,
   JUnit_File_Name : access String := null;

   -- --------------------------------------------------------------------------
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
   --  Shells a shell step may run the command string through
   --  (cf. docs/features/A310_Run_Shell.md): the default is cmd.exe on
   --  Windows, whose redirections have the POSIX semantics, and sh
   --  on the other systems
   function Shell_Name (K : Shell_Kind) return String is
     (case K is
         when Sh         => "sh",
         when Bash       => "bash",
         when Dash       => "dash",
         when Zsh        => "zsh",
         when Fish       => "fish",
         when Cmd        => "cmd",
         when Powershell => "powershell",
         when Pwsh       => "pwsh");

   The_Shell : Shell_Kind :=
     (if GNAT.OS_Lib.Directory_Separator = '\' then Cmd else Sh);

   function Shell return Shell_Kind is (The_Shell);

   procedure Set_Shell (Value : String; OK : out Boolean) is
      use Ada.Strings.Fixed, Ada.Strings.Maps.Constants;
      Lower : constant String :=
        Translate (Trim (Value, Ada.Strings.Both), Lower_Case_Map);
   begin
      OK := False;
      for K in Shell_Kind loop
         if Lower = Shell_Name (K) then
            The_Shell := K;
            OK := True;
            exit;
         end if;
      end loop;
   end Set_Shell;

   function On_Windows return Boolean is
     (GNAT.OS_Lib.Directory_Separator = '\');

   function Shell_Exe (K : Shell_Kind) return String is
     (case K is
         when Sh         => "sh",
         when Bash       => "bash",
         when Dash       => "dash",
         when Zsh        => "zsh",
         when Fish       => "fish",
         when Cmd        => (if On_Windows then "cmd.exe" else "cmd"),
         when Powershell => (if On_Windows then "powershell.exe"
                             else "powershell"),
         when Pwsh       => (if On_Windows then "pwsh.exe" else "pwsh"));

   function Shell_Flag (K : Shell_Kind) return String is
   --  The flags taking the command string as a single argument. The
   --  PowerShell shells need two flags, -NoProfile -Command: a user
   --  profile could otherwise change the location or the encoding
   --  defaults at each command run. The flags are appended by
   --  Run_Cmd as one argument each, ada-util joining the arguments
   --  without quoting them
     (case K is
         when Sh | Bash | Dash | Zsh | Fish => "-c",
         when Cmd                           => "/c",
         when Powershell | Pwsh             => "-NoProfile -Command");

   function Shell_Preamble (K : Shell_Kind) return String is
   --  Note: the preamble restores UTF-8 with a BOM only, in Windows
   --  PowerShell 5.1 (`utf8NoBOM` only exists from PowerShell 7)
     (case K is
         when Powershell | Pwsh =>
           "$PSDefaultParameterValues['Out-File:Encoding']='utf8'; ",
         when others => "");

   -- --------------------------------------------------------------------------
   function Launch_Directory return String
   is (Launch_Dir.all);

   -- --------------------------------------------------------------------------
   procedure Set_Exec_Dir (Dir_Name : String) is
   begin
      Exec_Dir_Name := new String'(Dir_Name);
   end Set_Exec_Dir;

   function Exec_Dir return String is
    (if Exec_Dir_Name = null then Ada.Directories.Current_Directory
       else Exec_Dir_Name.all);

   -- --------------------------------------------------------------------------
   procedure Set_JUnit_File (File_Name : String) is
   begin
      JUnit_File_Name := new String'(File_Name);
   end Set_JUnit_File;

   function JUnit_File return String is (JUnit_File_Name.all);

   -- --------------------------------------------------------------------------
   procedure Set_Tmp_Dir (Dir_Name : String) is
   begin
      Tmp_Dir_Name := new String'(Dir_Name);
   end Set_Tmp_Dir;

   function Tmp_Dir return String is
     (if Tmp_Dir_Name = null then Ada.Directories.Current_Directory
      else Tmp_Dir_Name.all);

   -- --------------------------------------------------------------------------
   function Output_File_Dir return String renames Tmp_Dir;

   -- --------------------------------------------------------------------------
   procedure Set_Index_File (File_Name : String) is
   begin
      Outfile_Name := new String'(File_Name);
   end Set_Index_File;

   function Index_File_Name return String is
    (if Outfile_Name = null then "" else Outfile_Name.all);

   function Index_Dir return String is
    (if Outfile_Name = null then Ada.Directories.Current_Directory
       else Ada.Directories.Containing_Directory (Outfile_Name.all));

   -- --------------------------------------------------------------------------
   procedure Set_Badge_File_Name (File_Name : String) is
   begin
      Badge_Name := new String'(File_Name);
   end Set_Badge_File_Name;

   function Badge_File_Name return String is
     (if Badge_Name = null then "" else Badge_Name.all);

   -- --------------------------------------------------------------------------
   procedure Set_Scenario_Timeout (Value : String; OK : out Boolean) is
      Total  : Duration := 0.0;
      Index  : Integer  := Value'First;
      Amount : Natural;
      Digit_Count : Natural;
   begin
      OK := Value /= "";
      while OK and then Index <= Value'Last loop
         Amount := 0;
         Digit_Count := 0;
         while Index <= Value'Last and then Value (Index) in '0' .. '9' loop
            Amount := Amount * 10
                        + (Character'Pos (Value (Index))
                             - Character'Pos ('0'));
            Digit_Count := Digit_Count + 1;
            Index  := Index + 1;
         end loop;
         OK := Digit_Count > 0;
         exit when not OK;
         if Index <= Value'Last then
            case Value (Index) is
               when 's' | 'S' => Total := Total + Duration (Amount);
               when 'm' | 'M' => Total := Total + 60.0 * Duration (Amount);
               when 'h' | 'H' => Total := Total + 3600.0 * Duration (Amount);
               when others    => OK := False;
            end case;
            Index := Index + 1;
         else
            --  no unit: a plain number of seconds
            Total := Total + Duration (Amount);
         end if;
      end loop;
      if OK then
         Scenario_Timeout := Total;
      end if;
   end Set_Scenario_Timeout;

end BBT.Settings;
