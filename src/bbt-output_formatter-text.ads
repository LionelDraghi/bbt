-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

package BBT.Output_Formatter.Text is

   -- --------------------------------------------------------------------------
   procedure Initialize;

private
   type Text_Formatter is new Abstract_Formatter with null record;

   -- --------------------------------------------------------------------------
   overriding function File_Extensions
     (Formatter : Text_Formatter) return String is ("*.txt");

   -- --------------------------------------------------------------------------
   overriding function Default_Extension
     (Formatter : Text_Formatter) return String is (".txt");

end BBT.Output_Formatter.Text;
