-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Output_Formatter.Markdown;
     -- BBT.Output_Formatter.Text;

procedure BBT.Output_Formatter.Initialize is
begin
   BBT.Output_Formatter.Markdown.Initialize;
   -- BBT.Output_Formatter.Text.Initialize;
end BBT.Output_Formatter.Initialize;
