-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

private package BBT.Status_Bar.Platform is

   --  -------------------------------------------------------------------------
   function UTF8_Output return Boolean;
   --  True when the standard output decodes UTF-8:
   --  on Windows, the glyphs and the braille spinner are mojibake unless
   --  the console output code page is 65001, whatever the terminal is;
   --  on Unix, the locale, as detected by Termicap, is authoritative.

end BBT.Status_Bar.Platform;
