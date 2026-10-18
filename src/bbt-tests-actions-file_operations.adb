-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO,
     BBT.Settings,
     BBT.Status_Bar,
     Termicap.TTY;

package body BBT.Tests.Actions.File_Operations is

   -- --------------------------------------------------------------------------
   function Confirm (Prompt : String) return Boolean is
      C : Character;
   begin
      if Settings.Yes then return True;
      end if;

      --  Nobody can answer the prompt when the standard input is not
      --  an interactive terminal: on a closed or redirected stdin
      --  (cron, CI), Get_Immediate raises END_ERROR, and on an open
      --  but mute stdin (a nested bbt), it blocks forever. Refuse the
      --  operation, as on a "N" answer.
      if not Termicap.TTY.Is_TTY (Termicap.TTY.Stdin) then
         BBT.IO.Put_Error
           ("cannot prompt for confirmation on a non interactive stdin, "
            & "use --yes to confirm all operations");
         return False;
      end if;

      loop
         Status_Bar.Clear;
         --  Erase the status bar, so that the prompt is not glued
         --  at the end of the bar line
         Ada.Text_IO.Put (Prompt);
         Ada.Text_IO.Put_Line ("   [Y]es/[N]o/[A]ll");
         Ada.Text_IO.Get_Immediate (C);
         case C is
            when 'N' | 'n' => return False;
            when 'Y' | 'y' => return True;
            when 'A' | 'a' =>
               Settings.Yes := True;
               return True;
            when others => null;
         end case;
      end loop;
   end Confirm;

   -- --------------------------------------------------------------------------
   function Confirm_Overwrite (File_Name : String) return Boolean is
   begin
      return Confirm ("Overwrite file " & File_Name & "?");
   end Confirm_Overwrite;

   -- --------------------------------------------------------------------------
   function Exists (Name : String) return Boolean is
   begin
      return Ada.Directories.Exists (Name);
   exception
      when Ada.Directories.Name_Error =>
         BBT.IO.Put_Error ("Illegal file name : """ & Name & """");
         raise;
         -- return False;
   end Exists;

   function Kind (Name : String) return File_Kind
                  renames Ada.Directories.Kind;

   -- --------------------------------------------------------------------------
   procedure Delete_File (Name : String) is
   begin
      if Exists (Name)
        and then Confirm ("Delete file " & Name & "?")
      then
         Ada.Directories.Delete_File (Name);
      end if;
   end Delete_File;

   -- --------------------------------------------------------------------------
   procedure Delete_Tree (Dir_Name : String)  is
   begin
      if Exists (Dir_Name)
        and then Confirm ("Delete tree " & Dir_Name & "?")
      then
         Ada.Directories.Delete_Tree (Dir_Name);
      end if;
   end Delete_Tree;

   -- --------------------------------------------------------------------------
    procedure Close  (File : in out File_Type) renames Ada.Text_IO.Close;

end BBT.Tests.Actions.File_Operations;
