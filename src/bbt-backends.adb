-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

--  Body of the GUI backends root: dispatch every GUI operation to the
--  Selected backend access, cf. the spec comment, or return the
--  backend-less value. Selected is set at startup by the backend
--  executable main (cf. docs/proposed/crate_restructuring.md);
--  the default is no backend.

package body BBT.Backends is

   procedure Launch (Binary_Path : String) is
   begin
      if Selected = null then
         raise No_Backend_Available
           with "no GUI backend available: select one with --gui=<kind>";
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

end BBT.Backends;
