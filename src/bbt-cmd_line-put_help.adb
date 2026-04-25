-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Bbt_Config;
with BBT.Scenarios.Steps;

separate (BBT.Cmd_Line)

-- NB: when changing the texts hereafter, don't forget to update
--     the B130_Cmd_Line_Help.md test.

-- -----------------------------------------------------------------------------
procedure Put_Help (Topic : Settings.Help_Topic) is
   Base_Help      : constant String with External_Initialization => "../docs/help/base.txt";
   Filtering_Help : constant String with External_Initialization => "../docs/help/filtering.txt";
   Matching_Help  : constant String with External_Initialization => "../docs/help/matching.txt";
   Other_Help     : constant String with External_Initialization => "../docs/help/other.txt";
   Debug_Help     : constant String with External_Initialization => "../docs/help/debug.txt";
   Tutorial_Help  : constant String with External_Initialization => "../docs/help/tutorial.md";
   Example_Help   : constant String with External_Initialization => "../docs/help/example.md";

begin
   case Topic is
      when Base      => Put_Line ("bbt version " & Bbt_Config.Crate_Version);
                        New_Line;
                        Put_Line (Base_Help);
      when Filtering => Put_Line (Filtering_Help);
      when Matching  => Put_Line (Matching_Help);
      when Other     => Put_Line (Other_Help);
         -- Put_Line ("   -ot  | --output_tag 'tag' : include a specific tag in the results file");
      when Debug     => Put_Line (Debug_Help);
      when Tutorial  => Put_Line (Tutorial_Help);
      when Example   => Put_Line (Example_Help);
      when Grammar   => Scenarios.Steps.Put_Grammar;
      when Keywords  => Scenarios.Steps.Put_Keywords;

      when On_All =>
         -- First the base, then topics
         Put_Help (Base);
         for Topic in Help_Topic when Topic not in On_All | Base | Tutorial | Example loop
            Put_Help (Topic);
         end loop;

   end case;

end Put_Help;
