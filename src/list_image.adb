-- -----------------------------------------------------------------------------
-- bbt, the BlackBox tester (https://github.com/LionelDraghi/bbt)
-- © 2024 Lionel Draghi <lionel.draghi@free.fr>
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------

with Ada.Strings.Unbounded;

package body List_Image is

   function Image (Cont : Cursors.Container) return String is
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
