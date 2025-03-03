-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

private package BBT.Output_Formatter is
-- This package defines common services for all types of output, and
-- the abstract interface that each new formatter (defined in child
-- packages) should implement.

   -- --------------------------------------------------------------------------
   type Format is (MD);
   -- Txt, AsciiDoc, Terminal);
   -- WARNING : There should be a registered Formatter fo each enum, otherwise
   --           an access check exception will be raised at run time.
   --           (cf. the child procedure Initialize)

   -- -------------------------------------------------------------------------
   function File_Extensions (For_Format : Format) return String;
   -- Returns the regexp that will be used to identify sources files
   -- of this format.

   function Default_Extension (For_Format : Format) return String;
   -- Preferred extension when creating a file of this format

   -- asciidoc : *.(adoc|asciidoc)

private
   -- -------------------------------------------------------------------------
   type Abstract_Formatter is abstract tagged limited null record;

   function File_Extensions
     (Formatter : Abstract_Formatter) return String is abstract;
   -- Returns the regexp that will be used to identify sources files
   -- processed by this formatter.
   function Default_Extension
     (Formatter : Abstract_Formatter) return String is abstract;
   -- Preferred extension when creating a file of this format

   -- -------------------------------------------------------------------------
   type Interface_Access is access all Abstract_Formatter'class;
   procedure Register (Formatter   : Interface_Access;
                       For_Format : Format);

end BBT.Output_Formatter;
