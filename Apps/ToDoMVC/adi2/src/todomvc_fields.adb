-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

package body TodoMVC_Fields is

   use type Adi.SDL.Events.SDL_Scancode;

   overriding procedure On_Key_Down
     (W        : in out Text_Input_Commit;
      Scancode : Adi.SDL.Events.SDL_Scancode;
      Key_Mod  : Adi.SDL.Events.SDL_Keymod;
      Repeat   : Boolean)
   is
      Parent : Adi.Widget.Text_Input.Text_Input_Widget renames
        Adi.Widget.Text_Input.Text_Input_Widget (W);
   begin
      if Scancode = Adi.SDL.Events.SDL_SCANCODE_RETURN
        and then not Repeat
        and then W.On_Commit /= null
      then
         W.On_Commit (Adi.Widget.Text_Input.Get_Text (Parent));
         return;
      end if;
      --  Every other key keeps Adi2's own editing behaviour: a static
      --  call, the parent body, not a redispatch to this one
      Adi.Widget.Text_Input.On_Key_Down (Parent, Scancode, Key_Mod, Repeat);
   end On_Key_Down;

   function Create_Handle (Text : String := "") return Field_Handle is
      H : constant Field_Handle := Field_Extension.New_Widget;
      W : constant Adi.Widget.Widget_Handle :=
        Field_Extension.To_Widget_Handle (H);
   begin
      --  A widget built through the extension store is born with the
      --  Visible flag only, where Adi2's own Text_Input.Create also
      --  declares Clickable and Focusable. Both are needed for the
      --  widget to behave as a field: the window routes the mouse press
      --  to the Clickable widget under the pointer, which is what runs
      --  the field's own On_Mouse_Down (caret placement), and it routes
      --  the keys to the focused one, which must be a focus candidate.
      Adi.Widget.Set_Flag (W, Adi.Widget.Clickable, True);
      Adi.Widget.Set_Flag (W, Adi.Widget.Focusable, True);

      if Text'Length > 0 then
         Adi.Widget.Text_Input.Set_Text (As_Text_Input (H), Text);
      end if;
      return H;
   end Create_Handle;

   function As_Text_Input (H : Field_Handle)
                           return Adi.Widget.Text_Input.Text_Input_Handle
   is
      W : constant Adi.Widget.Widget_Handle :=
        Field_Extension.To_Widget_Handle (H);
   begin
      return Adi.Widget.Text_Input.Try_As_Text_Input (W);
   end As_Text_Input;

   procedure Set_On_Commit (H : Field_Handle; Callback : Commit_Callback) is
      R : constant Field_Extension.Ref := Field_Extension.Borrow (H);
   begin
      R.Ptr.On_Commit := Callback;
   end Set_On_Commit;

end TodoMVC_Fields;
