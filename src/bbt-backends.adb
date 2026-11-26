-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

--  Body of the GUI backends root: dispatch every GUI operation to the
--  Selected backend access, cf. the spec comment, or return the
--  backend-less value. Selected is set at startup by the backend
--  executable main (cf. docs/dev/design_discussions.md (D16));
--  the default is no backend.

package body BBT.Backends is

   procedure Launch (Binary_Path : String) is
   begin
      if Selected = null then
         raise No_Backend_Available
           with "no GUI backend available: select one with --backend=<kind>";
      else
         Selected.Launch (Binary_Path);
      end if;
   end Launch;

   procedure Shutdown is
   begin
      if Selected /= null then
         Selected.Shutdown;
      end if;
   end Shutdown;

   function Window_Appears (Expected_Title : String := "") return Boolean is
   begin
      if Selected = null then
         return False;
      else
         return Selected.Window_Appears (Expected_Title);
      end if;
   end Window_Appears;

   function Observed_Title return String is
   begin
      if Selected = null then
         return "";
      else
         return Selected.Observed_Title;
      end if;
   end Observed_Title;

   function Window_Contains (Expected : String) return Boolean is
   begin
      if Selected = null then
         return False;
      else
         return Selected.Window_Contains (Expected);
      end if;
   end Window_Contains;

   function Widget_Named (Role : String; Name : String) return Boolean is
   begin
      if Selected = null then
         return False;
      else
         return Selected.Widget_Named (Role, Name);
      end if;
   end Widget_Named;

   function Widget_Enabled
     (Role : String; Name : String) return Boolean
   is
   begin
      if Selected = null then
         return False;
      else
         return Selected.Widget_Enabled (Role, Name);
      end if;
   end Widget_Enabled;

   function Press_Widget (Role : String; Name : String) return Boolean is
   begin
      if Selected = null then
         return False;
      else
         return Selected.Press_Widget (Role, Name);
      end if;
   end Press_Widget;

   function Widget_Contains
     (Role, Name, Expected : String) return Boolean
   is
   begin
      if Selected = null then
         return False;
      else
         return Selected.Widget_Contains (Role, Name, Expected);
      end if;
   end Widget_Contains;

   function Widget_Is_Empty (Role : String; Name : String) return Boolean is
   begin
      if Selected = null then
         return False;
      else
         return Selected.Widget_Is_Empty (Role, Name);
      end if;
   end Widget_Is_Empty;

   function Widget_Item_Count (Role : String; Name : String) return Natural is
   begin
      if Selected = null then
         return 0;
      else
         return Selected.Widget_Item_Count (Role, Name);
      end if;
   end Widget_Item_Count;

   function Type_Into
     (Role : String; Name : String; Text : String) return Boolean
   is
   begin
      if Selected = null then
         return False;
      else
         Selected.Type_Into (Role, Name, Text);
         return True;
      end if;
   end Type_Into;

   function Enter_Into
     (Role : String; Name : String; Text : String) return Boolean
   is
   begin
      if Selected = null then
         return False;
      else
         Selected.Enter_Into (Role, Name, Text);
         return True;
      end if;
   end Enter_Into;

   function Close_Window (Title : String := "") return Boolean is
   begin
      if Selected = null then
         return False;
      else
         return Selected.Close_Window (Title);
      end if;
   end Close_Window;

end BBT.Backends;
