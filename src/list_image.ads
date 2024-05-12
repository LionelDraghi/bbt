-- -----------------------------------------------------------------------------
-- Copyright 2018 2024 Lionel Draghi
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-- -----------------------------------------------------------------------------
-- This file is part of the List_Image project
-- available at https://github.com/LionelDraghi/List_Image
-- -----------------------------------------------------------------------------

package List_Image is

   -- --------------------------------------------------------------------------
   --                               Style
   -- --------------------------------------------------------------------------
   -- This signature package defines the style of the String returned by the
   -- Image function.
   --
   -- Prefix, Postfix and Separator parameters are self explaining.
   -- If Prefix = '(', Postfix = ')', and Separator = ',', Image will be of
   -- this kind : (A,B,C,D)
   -- If all parameters are set to "", the image will be : ABCD
   --
   -- Special Prefix and Postfix are possible for null list, and for list with
   -- a single element.
   -- This is useful when you want to want "[A,B,C]" as an image, but you don't
   -- want "[]" when the list is empty.
   --
   -- A useful application of this feature is to have well written comments
   -- regarding singular and plural.
   -- If you want your image to be "A item found" but "A, B, C items found",
   -- just set Postfix to " items found", and Postfix_If_Single to
   -- " item found".
   -- And by the way, if you want the Image to be "No item found" when the
   -- list is empty, Prefix_If_Empty and Postfix_If_Empty are here for you.
   --
   -- Last_Separator allows to have this kind of output :
   -- "A, B, C and D"
   --
   -- Note that Separator may be whatever String. You may want to insert an End
   -- of Line sequence to split the list on several line, the EOL String and
   -- parameters are provided for that purpose.
   -- --------------------------------------------------------------------------

   generic
      Prefix    : String := "";
      Postfix   : String := "";
      Separator : String := "";

      Last_Separator    : String := Separator;
      Prefix_If_Empty   : String := Prefix;
      Postfix_If_Empty  : String := Postfix;
      Prefix_If_Single  : String := Prefix;
      Postfix_If_Single : String := Postfix;

      EOL               : String := "";

   package Image_Style is end Image_Style;

   -- --------------------------------------------------------------------------
   --                     Predefined single line styles
   -- --------------------------------------------------------------------------
   --
   -- Predefined **single line** styles (that are not relying on EOL
   -- definition), and are not platform specific, are proposed here after.
   --
   -- - Default_Style :
   --   > A, B, C
   --
   -- - English_Style :
   --   > A, B and C
   --
   -- - Bracketed_List_Style :
   --   > [A, B, C]
   --
   -- - Markdown_Table_Style :
   --   > | A | B | C |
   --   Note : Markdown don't define tables, but it's a common extension,
   --          like in Github Flavored Markdown for example.
   --
   -- --------------------------------------------------------------------------
   package Default_Style is new Image_Style (Separator => ", ");

   package English_Style is new Image_Style (Separator      => ", ",
                                             Last_Separator => " and ");

   package Bracketed_List_Style is new Image_Style (Prefix    => "[",
                                                    Postfix   => "]",
                                                    Separator => ", ");

   package Markdown_Table_Style is new Image_Style (Prefix           => "|",
                                                    Separator        => "|",
                                                    Postfix          => "|",
                                                    Prefix_If_Empty  => "",
                                                    Postfix_If_Empty => "");

   -- --------------------------------------------------------------------------
   --                     Predefined Multi-Lines styles
   -- --------------------------------------------------------------------------
   --
   -- Predefined (platform dependent) **multi lines** styles
   -- are proposed in the following child packages :
   -- - List_Image.Unix_Predefined_Styles
   -- - List_Image.Windows_Predefined_Styles
   --
   -- Common line terminator definitions are provided here for convenience,
   -- as there is no easy and standard way in Ada to get the right EOL
   -- terminator on the current platform (supposing there is one on the
   -- targeted platform).
   --
   -- --------------------------------------------------------------------------

   -- Predefined EOL are (terminator explicit) :
   LF_EOL      : constant String := [ASCII.LF];
   CRLF_EOL    : constant String := ASCII.CR & ASCII.LF;

   -- or (platform explicit) :
   Unix_EOL    : constant String := LF_EOL;
   Windows_EOL : constant String := CRLF_EOL;

   -- --------------------------------------------------------------------------
   --                             Cursors
   -- --------------------------------------------------------------------------
   generic
      type Container (<>) is limited private;
      type Cursor is private;
      with function First   (Self : Container) return Cursor  is <>;
      with function Next        (Pos : Cursor) return Cursor  is <>;
      with function Has_Element (Pos : Cursor) return Boolean is <>;
   package Cursors_Signature is end Cursors_Signature;

   -- --------------------------------------------------------------------------
   --                       The Image function
   -- --------------------------------------------------------------------------
   generic
      with package Cursors is new Cursors_Signature (<>);
      with function Image (C : Cursors.Cursor) return String is <>;
      with package Style is new Image_Style (<>);
   function Image (Cont : Cursors.Container) return String;

end List_Image;
