-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

package body BBT.Scenarios is

   -- --------------------------------------------------------------------------
   procedure Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      A      :        Line_Attributes) is
   begin
      Output.Put (Line_Kind'Image (A.Kind) & " | ");
      case A.Kind is
         when Feature_Line    |
              Scenario_Line   |
              Background_Line => Output.Put (A.Name'Image);
         when Text_Line       => Output.Put (A.Line'Image);
         when Step_Line       => Output.Put (A.Step_Ln'Image);
         when Code_Fence      |
              Empty_Line      => null;
      end case;
   end Put_Image;

end BBT.Scenarios;
