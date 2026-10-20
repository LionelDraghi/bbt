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

with Ada.Strings.Unbounded;

package body List_Image is

   function Image (Cont : in Cursors.Container) return String is
      use Cursors, Ada.Strings.Unbounded;
      C1, C2 : Cursor;
      Tmp    : Unbounded_String;

   begin
      C1 := First (Cont);

      if not Has_Element (C1) then
         -- empty data structure
         return Style.Prefix_If_Empty & Style.Postfix_If_Empty;

      else
         -- before using the first list item, we need to know if there is
         -- another one.
         C2 := Next (C1);

         if not Has_Element (C2) then
            -- single item list
            return Style.Prefix_If_Single & Image (C1)
              & Style.Postfix_If_Single;

         else
            -- at least two item in the list
            Tmp := To_Unbounded_String (Style.Prefix);
            Append (Tmp, Image (C1));
            loop
               C1 := C2;
               C2 := Next (C2);
               if Has_Element (C2) then
                  -- C1 do not yet point the last item
                  Append (Tmp, Style.Separator);
                  Append (Tmp, Image (C1));

               else
                  -- C1 point the last item
                  Append (Tmp, Style.Last_Separator);
                  Append (Tmp, Image (C1));
                  exit;

               end if;

            end loop;

         end if;

      end if;

      Append (Tmp, Style.Postfix);
      return To_String (Tmp);

   end Image;

end List_Image;
