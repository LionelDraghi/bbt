-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- -----------------------------------------------------------------------------

--  No backend: raised by every operation of BBT.Tests.GUI_Backends
--  when the backend is not implemented on the running platform.
with BBT.Tests.GUI_Backends;

package body BBT.Tests.GUI_Backends is

   procedure Launch (Binary_Path : String) is
   begin
      raise No_Backend_Available
        with "no GUI backend available on this platform";
   end Launch;

   procedure Kill_Last is
   begin
      null;
   end Kill_Last;

   function Window_Appears (Expected_Title : String := "") return Boolean is
      pragma Unreferenced (Expected_Title);
   begin
      return False;
   end Window_Appears;

   function Observed_Title return String is
   begin
      return "";
   end Observed_Title;

   function Window_Contains (Expected : String) return Boolean is
      pragma Unreferenced (Expected);
   begin
      return False;
   end Window_Contains;

   function Dialog_Appears (Expected_Name : String := "") return Boolean is
      pragma Unreferenced (Expected_Name);
   begin
      return False;
   end Dialog_Appears;

   function Widget_Named (Role : String; Name : String) return Boolean is
      pragma Unreferenced (Role, Name);
   begin
      return False;
   end Widget_Named;

   function Widget_Enabled
     (Role : String; Name : String) return Boolean is
      pragma Unreferenced (Role, Name);
   begin
      return False;
   end Widget_Enabled;

end BBT.Tests.GUI_Backends;
