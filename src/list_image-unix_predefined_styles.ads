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

package List_Image.Unix_Predefined_Styles is

   EOL : constant String := Unix_EOL;
   -- Note that two identical packages exist, named :
   -- List_Image.[Unix|Windows]_Predefined_Styles
   -- The only difference between those packages is this platform specific
   -- EOL definition.

   -- --------------------------------------------------------------------------
   --                         Predefined multi-line style
   -- --------------------------------------------------------------------------
   --
   -- - Bulleted_List_Style :
   --   > - A
   --   > - B
   --   > - C
   --
   -- - Markdown_Bulleted_List_Style :
   --   Like the bulleted list, but surrounded by
   --   two empty lines (in some Markdown implementation, if the first bullet
   --   is not preceded by an empty line, the list is not recognized)
   --
   -- - HTML_Bulleted_List_Style :
   --   > <ul>
   --   > <li>A</li>
   --   > <li>B</li>
   --   > <li>C</li>
   --   > </ul>
   --   Note : <ul></ul>, an empty list, is recognized by most navigator,
   --          but seems to be illegal html.
   --          No problem here, thanks to _If_Empty parameters nothing will
   --          be generated if the list is empty.
   --
   -- --------------------------------------------------------------------------

   package Bulleted_List_Style is new Image_Style
     (Prefix           => EOL & "- ",
      Separator        => EOL & "- ",
      Postfix          => EOL,
      Prefix_If_Empty  => "",
      Postfix_If_Empty => "");

   package Markdown_Bulleted_List_Style is new Image_Style
     (Prefix           => EOL & EOL & "- ",
      Separator        => EOL & "- ",
      Postfix          => EOL & EOL,
      Prefix_If_Empty  => EOL,
      Postfix_If_Empty => "");

   package HTML_Bulleted_List_Style is new Image_Style
     (Prefix           => "<ul>"  & EOL & "  <li>",
      Separator        => "</li>" & EOL & "  <li>",
      Postfix          => "</li>" & EOL & "</ul>",
      Prefix_If_Empty  => "",
      Postfix_If_Empty => "");

end List_Image.Unix_Predefined_Styles;
