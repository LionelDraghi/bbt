-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

--  Text normalization for the GUI vocabulary matching: the accessible
--  names, window titles and rendered text of UI toolkits carry
--  spacing and casing that the vocabulary matches flexibly.

package BBT_Backends.Text is

   function Normalized (S : String) return String;
   --  The normalized form of S: leading and trailing blanks removed,
   --  and every blank run (spaces, tabs, line breaks) collapsed to a
   --  single space. Matching compares normalized forms.

   function Contains
     (Content, Expected : String;
      Ignore_Case       : Boolean := False) return Boolean;
   --  True when the normalized Content contains the normalized
   --  Expected; when Merged_Case is True, the comparison is case
   --  insensitive

end BBT_Backends.Text;
