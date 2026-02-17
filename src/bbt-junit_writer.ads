-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Model.Documents;

private package BBT.JUnit_Writer is

   -- Generate JUnit XML report from test results
   procedure Generate_Report (File_Name : String;
                              Docs      : BBT.Model.Documents.List);

end BBT.JUnit_Writer;
