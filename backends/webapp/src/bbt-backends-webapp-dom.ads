-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

--  The page, seen the way the GUI vocabulary addresses it: role and
--  accessible name (G015), state (G015), content and emptiness (G080),
--  window text (G010).
--
--  A browser exposes no accessibility-tree query to a non assistive
--  client: the backend computes the same lookup key on the DOM, with
--  the ARIA mapping the vocabulary is grounded on (explicit role
--  attribute first, native element second, and the accname sources the
--  witness relies on: aria-label, else the element's text).
--
--  Each function returns a JavaScript expression, run through the
--  WebDriver script execution. Roles, names and values are normalized
--  on the Ada side and embedded as JavaScript literals.

package BBT.Backends.Webapp.Dom is

   function Exists_Script (Role : String; Name : String) return String;
   --  A shown element of that role and accessible name is in the page

   function Mark_Script (Role : String; Name : String) return String;
   --  Marks the element matching the lookup, so that the driver can
   --  address it (the only way to reach its default action and its key
   --  handling): "found" or ""

   function Enabled_Script (Role : String; Name : String) return String;
   --  The user-visible enabled state: not disabled, and not
   --  aria-disabled

   function Text_Script (Role : String; Name : String) return String;
   --  The rendered text of the element subtree, "" when absent

   function Child_Count_Script (Role : String; Name : String) return String;
   --  The number of shown children of the element: the "is empty" and
   --  "has n items" checks (G080), where an invisible placeholder does
   --  not count, the way the accessibility tree hides it

   function Value_Script (Role : String; Name : String) return String;
   --  The current value of the named field, "" when it is absent: the
   --  value a commit is about to read

   function Set_Value_Script
     (Role : String; Name : String; Value : String) return String;
   --  The semantic value set of a text field (G035, its "type" step):
   --  the value, an input event, and no validation

   function Window_Text_Script return String;
   --  The rendered text of the page: the window content checks (G010)

   function Ready_Script return String;
   --  The document finished loading

   Element_Marker : constant String := "[data-bbt-target='1']";
   --  The CSS selector of the element marked by Mark_Script

end BBT.Backends.Webapp.Dom;
