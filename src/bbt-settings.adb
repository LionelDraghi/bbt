-- -----------------------------------------------------------------------------
-- bbt, the BlackBox tester (https://github.com/LionelDraghi/bbt)
-- © 2024 Lionel Draghi <lionel.draghi@free.fr>
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------

with Ada.Directories;

package body BBT.Settings is

   -- --------------------------------------------------------------------------
   -- Most of the variable here are "write once, read more".
   -- To avoid the cost of Unbounded strings manipulation,
   -- they are implemented as access to String
   -- Runfl_Name      : access String := null;
   -- Cmd_Line        : Unbounded_String := Null_Unbounded_String;
   WD : constant access String :=
          new String'(Ada.Directories.Current_Directory);
   Outfile_Name      : access String := null;

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

   -- --------------------------------------------------------------------------
   procedure Set_Output_File (File_Name : String) is
   begin
      Outfile_Name := new String'(File_Name);
   end Set_Output_File;

   function Get_Output_File_Name return String  is
     (if Outfile_Name = null then "" else Outfile_Name.all);

end BBT.Settings;
