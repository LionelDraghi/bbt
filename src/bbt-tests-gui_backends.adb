-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- -----------------------------------------------------------------------------

with BBT.Tests.GUI_Backends.Atspi;

package body BBT.Tests.GUI_Backends is

   procedure Launch (Binary_Path : String) is
   begin
      case Selected is
         when GUI_Atspi =>
            Atspi.Launch (Binary_Path);
         when No_Backend =>
            raise No_Backend_Available
              with "no GUI backend available, use --backend";
      end case;
   end Launch;

   procedure Shutdown is
   begin
      case Selected is
         when GUI_Atspi =>
            Atspi.Shutdown;
         when No_Backend =>
            null;
      end case;
   end Shutdown;

   function Window_Appears (Expected_Title : String := "") return Boolean is
   begin
      case Selected is
         when GUI_Atspi =>
            return Atspi.Window_Appears (Expected_Title);
         when No_Backend =>
            return False;
      end case;
   end Window_Appears;

   function Observed_Title return String is
   begin
      case Selected is
         when GUI_Atspi =>
            return Atspi.Observed_Title;
         when No_Backend =>
            return "";
      end case;
   end Observed_Title;

   function Window_Contains (Expected : String) return Boolean is
   begin
      case Selected is
         when GUI_Atspi =>
            return Atspi.Window_Contains (Expected);
         when No_Backend =>
            return False;
      end case;
   end Window_Contains;

   function Widget_Named (Role : String; Name : String) return Boolean is
   begin
      case Selected is
         when GUI_Atspi =>
            return Atspi.Widget_Named (Role, Name);
         when No_Backend =>
            return False;
      end case;
   end Widget_Named;

   function Widget_Enabled
     (Role : String; Name : String) return Boolean is
   begin
      case Selected is
         when GUI_Atspi =>
            return Atspi.Widget_Enabled (Role, Name);
         when No_Backend =>
            return False;
      end case;
   end Widget_Enabled;

end BBT.Tests.GUI_Backends;
