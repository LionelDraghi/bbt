-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

--  The witness persistence, in the format the other ToDoMVC
--  implementations use (cf. ../web/README.md, ../ToDoMVC.md): one
--  'U <title>' (undone) or 'D <title>' (done) line per todo, plus a
--  'F <filter>' line. Written on every model change, so that closing
--  the window never loses an acknowledged change; an edition in
--  progress is not persisted.

with TodoMVC_Model;

package TodoMVC_Store is

   State_File_Name : constant String := "todos.state";

   procedure Load (List : in out TodoMVC_Model.Todo_List);
   --  Reads State_File_Name into List; a missing or unreadable file
   --  leaves the fresh app as it is

   procedure Save (List : TodoMVC_Model.Todo_List);
   --  Writes State_File_Name, or removes it when the list is empty:
   --  a fresh app then starts with no list and no footer

end TodoMVC_Store;
