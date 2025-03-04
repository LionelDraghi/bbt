-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

package BBT.Writers.Text is

   -- --------------------------------------------------------------------------
   procedure Initialize;

private
   type Text_Writer is new Abstract_Writer with null record;

   -- --------------------------------------------------------------------------
   overriding function File_Extensions
     (Writer : Text_Writer) return String is ("*.txt");

   -- --------------------------------------------------------------------------
   overriding function Default_Extension
     (Writer : Text_Writer) return String is (".txt");

end BBT.Writers.Text;
