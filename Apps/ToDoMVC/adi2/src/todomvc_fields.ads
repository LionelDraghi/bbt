-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

--  A text input that reports its Return key. Adi2's Text_Input handles
--  no Return at all (no submission signal: cf. its On_Key_Down), so an
--  application that commits a field on Enter — as the vocabulary's
--  'enter' step means — has to derive one. The type stays in the
--  text_input family on purpose: the probe serializes the Ada tag as
--  the widget type, and the adi2 backend recognizes a textbox there.

with Adi.SDL.Events;
with Adi.Widget;
with Adi.Widget.Extension;
with Adi.Widget.Text_Input;

package TodoMVC_Fields is

   type Commit_Callback is access procedure (Text : String);
   --  Called with the field's value when its Return key goes down

   type Text_Input_Commit is
     new Adi.Widget.Text_Input.Text_Input_Widget with record
      On_Commit : Commit_Callback := null;
   end record;

   overriding procedure On_Key_Down
     (W        : in out Text_Input_Commit;
      Scancode : Adi.SDL.Events.SDL_Scancode;
      Key_Mod  : Adi.SDL.Events.SDL_Keymod;
      Repeat   : Boolean);
   --  The ordinary editing keys reach Adi2's own handler; Return calls
   --  On_Commit instead

   package Field_Extension is
     new Adi.Widget.Extension (Text_Input_Commit);

   subtype Field_Handle is Field_Extension.Handle;

   function Create_Handle (Text : String := "") return Field_Handle;
   --  A registered commit field, holding Text, born clickable and
   --  focusable: the extension store declares neither flag, so a field
   --  built through it would neither take a click nor the keyboard

   function As_Text_Input (H : Field_Handle)
                           return Adi.Widget.Text_Input.Text_Input_Handle;
   --  The Adi2 text input handle of the field: Set_Text, Get_Text,
   --  Set_Min_Visible_Chars and the part styles speak that type

   function "+" (H : Field_Handle) return Adi.Widget.Widget_Handle
     renames Field_Extension."+";
   --  The widget handle of the field, for the generic widget operations

   procedure Set_On_Commit (H : Field_Handle; Callback : Commit_Callback);
   --  Wires the field's Return key

end TodoMVC_Fields;
