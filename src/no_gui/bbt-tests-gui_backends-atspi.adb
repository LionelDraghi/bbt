-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- -----------------------------------------------------------------------------

--  Non Linux platforms: no libatspi to bind. The unit keeps the same
--  name as the Linux implementation (src/atspi), so that the parent
--  body always compiles; every operation answers a no op, and Launch
--  raises the parent's No_Backend_Available.

package body BBT.Tests.GUI_Backends.Atspi is

   Backend_True : constant Boolean := False;
   pragma Unreferenced (Backend_True);

   procedure Launch (Binary_Path : String)
   is
      pragma Unreferenced (Binary_Path);
   begin
      raise No_Backend_Available
        with "no GUI backend available on this platform";
   end Launch;

   procedure Shutdown is
   begin
      null;
   end Shutdown;

   function Window_Appears (Expected_Title : String := "") return Boolean is
      pragma Unreferenced (Expected_Title);
   begin
      return False;
   end Window_Appears;

   function Window_Contains (Expected : String) return Boolean is
      pragma Unreferenced (Expected);
   begin
      return False;
   end Window_Contains;

   function Observed_Title return String is
   begin
      return "";
   end Observed_Title;

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

end BBT.Tests.GUI_Backends.Atspi;
