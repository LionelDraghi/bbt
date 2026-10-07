-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

--  This package centralizes the Markdown syntax knowledge shared by the
--  writers. It is a first step of the readers and writers organization
--  discussion, cf. docs/dev/design_discussions.md.

package Markdown_Utilities is

   -- --------------------------------------------------------------------------
   Hard_Break : constant String := "  ";
   -- In Markdown, two spaces at the end of a line force a line break,
   -- without starting a new paragraph.

   -- --------------------------------------------------------------------------
   function Web_Path (Path : String) return String;
   -- Returns Path with the Windows separators ('\') translated to '/',
   -- as expected in Markdown links and URLs: a backslash in a Markdown
   -- link destination is an escape character, and thus breaks the link
   -- (`..\..\foo.md` is read `....\foo.md` by any CommonMark parser).
   -- On Unix, Path is returned unchanged.

   -- --------------------------------------------------------------------------
   function Link (Name : String;
                  Path : String) return String;
   -- Returns the Markdown link "[Name](Path)".
   -- Path should be a Web_Path when it contains separators.

   -- --------------------------------------------------------------------------
   function Checkbox (Done : Boolean) return String;
   -- Returns "[X]" if Done, "[ ]" otherwise.

end Markdown_Utilities;
