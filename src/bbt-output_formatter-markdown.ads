-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

package BBT.Output_Formatter.Markdown is

   -- --------------------------------------------------------------------------
   procedure Initialize;

private
   type Markdown_Formatter is new Abstract_Formatter with null record;

   -- --------------------------------------------------------------------------
   overriding function File_Extensions
     (Formatter : Markdown_Formatter) return String is ("*.md");

   -- --------------------------------------------------------------------------
   overriding function Default_Extension
     (Formatter : Markdown_Formatter) return String is (".md");

end BBT.Output_Formatter.Markdown;
