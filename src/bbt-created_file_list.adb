-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO,
     BBT.Settings;

with Ada.Containers.Indefinite_Ordered_Sets,
     Ada.Containers.Indefinite_Vectors,
     Ada.Directories,
     Ada.Text_IO;

with GNAT.OS_Lib;

use BBT.IO,
    Ada,
    Ada.Directories,
    Ada.Text_IO;

package body BBT.Created_File_List  is

   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := IO.Created_Files)
                             renames IO.Put_Line;
   pragma Warnings (Off, Put_Debug_Line);

   package String_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);
   File_Set : String_Sets.Set;

   Snapshot_Set : String_Sets.Set;
   Snapshot_OK  : Boolean := False;
   -- Full names recorded by Take_Snapshot, the reference state of the
   -- exec dir at the start of the current document run

   type Entry_Rec (Name_Len : Natural) is record
      Full_Name : String (1 .. Name_Len);
      Kind      : File_Kind;
   end record;

   package Entry_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Entry_Rec);
   New_Entries : Entry_Vectors.Vector;
   -- Entries of the exec dir that appeared since the snapshot,
   -- collected parent before children, so that a reverse iteration
   -- deletes the children before their directory

   -- --------------------------------------------------------------------------
   type Entry_Process is access procedure (Full_Name : String;
                                            Kind      : File_Kind);

   procedure Walk (Dir : String; Process : Entry_Process) is
      Search  : Search_Type;
      Dir_Ent : Directory_Entry_Type;
   begin
      Start_Search (Search,
                    Directory => Dir,
                    Pattern   => "",
                    Filter    => [Ordinary_File => True,
                                  Directory     => True,
                                  others        => False]);
      --  The filter excludes special files and symbolic links, that
      --  GNAT's Search does not report anyway: a link is never
      --  traversed, so that the snapshot and the removal cannot
      --  escape the exec dir through a link, contrary to Delete_Tree
      --  that follows them.

      while More_Entries (Search) loop
         Get_Next_Entry (Search, Dir_Ent);
         declare
            Name : constant String := Simple_Name (Dir_Ent);
            Full : constant String := Full_Name (Dir_Ent);
         begin
            if Name /= "." and then Name /= ".." then
               if GNAT.OS_Lib.Is_Symbolic_Link (Full) then
                  --  Never recurse through a symbolic link: the
                  --  entries behind it are recorded under their real
                  --  path, and removing through the link would escape
                  --  the exec dir. The link is recorded as a file:
                  --  Delete_File removes the link itself, never the
                  --  target.
                  Process (Full, Ordinary_File);
               else
                  Process (Full, Kind (Dir_Ent));
                  if Kind (Dir_Ent) = Directory then
                     Walk (Full, Process);
                  end if;
               end if;
            end if;
         end;
      end loop;
      End_Search (Search);

   exception
      when others =>
         IO.Put_Warning ("Unable to scan dir " & Dir);
   end Walk;

   -- --------------------------------------------------------------------------
   procedure Remember (Full_Name : String;
                       Kind      : File_Kind) is
      pragma Unreferenced (Kind);
   begin
      Snapshot_Set.Insert (Full_Name);
   end Remember;

   -- --------------------------------------------------------------------------
   procedure Record_If_New (Full_Name : String;
                             Kind      : File_Kind) is
   begin
      if not Snapshot_Set.Contains (Full_Name) then
         New_Entries.Append (Entry_Rec'(Name_Len => Full_Name'Length,
                                        Full_Name => Full_Name,
                                        Kind      => Kind));
      end if;
   end Record_If_New;

   -- --------------------------------------------------------------------------
   procedure Register (File_Name : String) is
      use String_Sets;
   begin
      if File_Set.Find (File_Name) = No_Element then
         File_Set.Insert (File_Name);
      end if;
   end Register;

   -- --------------------------------------------------------------------------
   procedure Add_Path (Path : String) is
   begin
      Put_Debug_Line ("== Add_Path (" & Path & ");");

      if Exists (Path) then
         -- The file exists, let's exit
         Put_Debug_Line ("   Path " & Path & " exists, exiting... ");
      else
         Put_Debug_Line ("   " & Path & " doesnt exists, adding it");
         Register (Path);
         declare
            Parent : constant String :=
                       Directories.Containing_Directory (Path);
         begin
            Add_Path (Parent);
         end;
      end if;
   end Add_Path;

   -- --------------------------------------------------------------------------
   procedure Add (Name : String) is
      -- use String_Sets;
   begin
      --  if File_Set.Contains (Name) then
      --     Put_Warning ("Overwriting " & Name);
      --  end if;
      Put_Debug_Line ("== Add (" & Name & ");");
      Register (Name);
      Add_Path (Directories.Containing_Directory (Name));
   end Add;

   -- --------------------------------------------------------------------------
   procedure Put is
   begin
      for F of File_Set loop
         Text_IO.Put_Line (F);
      end loop;
   end Put;

   -- --------------------------------------------------------------------------
   procedure Delete_All is
   begin
      if Settings.Cleanup then
         Put_Debug_Line ("+++ Delete_All ");

         for F of File_Set loop
            -- We could delete in the opposite order of creation, so that
            -- dir1/f1 is deleted before dir1.
            -- But if bbt did create dir1, then what's in is supposed to be
            -- cleanup also. So there is no "reverse" in the loop, and we
            -- use Delete_Tree and not Delete_Directory.
            begin
               if Exists (F) then
                  if Kind (F) = Ordinary_File then
                     Put_Debug_Line ("Cleanup, deleting file """ & F & """");
                     Delete_File (F);
                  elsif Kind (F) = Directory then
                     Put_Debug_Line
                       ("Cleanup, deleting tree rooted at """ & F & """");
                     Delete_Tree (F);
                  end if;
               end if;
            exception
               when others =>
                  IO.Put_Exception ("Unable to cleanup " & F'Image);
                  raise;
            end;
         end loop;

         File_Set.Clear;
      end if;
   end Delete_All;

   -- --------------------------------------------------------------------------
   procedure Take_Snapshot is
   begin
      Snapshot_Set.Clear;
      New_Entries.Clear;
      Walk (".", Remember'Access);
      Snapshot_OK := True;
      Put_Debug_Line ("== Take_Snapshot, " & Snapshot_Set.Length'Image
                      & " entries recorded in the snapshot");
   end Take_Snapshot;

   -- --------------------------------------------------------------------------
   procedure Delete_New_Entries
     (Confirm : not null access function (Prompt : String) return Boolean)
   is
      use Entry_Vectors;
      procedure Reset_State is
      begin
         Snapshot_Set.Clear;
         Snapshot_OK  := False;
         New_Entries.Clear;
      end Reset_State;

   begin
      if not Snapshot_OK then
         Put_Debug_Line ("Full cleanup: no snapshot, nothing to do");
         return;
      end if;

      New_Entries.Clear;
      Walk (".", Record_If_New'Access);

      if New_Entries.Is_Empty then
         Put_Debug_Line ("Full cleanup: no new entry since the snapshot");
         Reset_State;
         return;
      end if;

      --  The entries were not created by bbt itself: ask before
      --  removing, through the Confirm function provided by the
      --  caller, which may refuse
      if not Confirm
        ("Delete" & Natural (New_Entries.Length)'Image
         & " files and directories created during the run?")
      then
         Reset_State;
         return;
      end if;

      for E of reverse New_Entries loop
         begin
            if E.Kind = Ordinary_File then
               Put_Debug_Line
                 ("Full cleanup, deleting file """ & E.Full_Name & """");
               Delete_File (E.Full_Name);
            else
               Put_Debug_Line
                 ("Full cleanup, deleting dir """ & E.Full_Name & """");
               Delete_Directory (E.Full_Name);
               --  The children were deleted first, in the reverse
               --  iteration: the dir is empty here, and
               --  Delete_Directory never traverses a symbolic link.
            end if;
         exception
            when others =>
               IO.Put_Warning ("Unable to cleanup " & E.Full_Name);
               --  e.g. a file still held open by a running command
               --  on Windows, or a dir still containing a new
               --  symbolic link: do not fail the run on it
         end;
      end loop;

      Reset_State;
   end Delete_New_Entries;

end BBT.Created_File_List;
