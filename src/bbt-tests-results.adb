-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Settings;

with Ada.Strings.Fixed;
with Ada.Text_IO;

package body BBT.Tests.Results is

   -- --------------------------------------------------------------------------
   Results : Test_Results_Count;

   -- --------------------------------------------------------------------------
   function Result (Scenario : Scenarios.Scenario_Type'Class)
                    return Test_Result is
   begin
      if Scenario.Failed_Step_Count > 0 then
         return Failed;
      elsif Scenario.Successful_Step_Count > 0 then
         return Successful;
      elsif not Scenario.Has_Run then
         return Skipped;
      else
         return Empty;
      end if;
   end Result;

   -- --------------------------------------------------------------------------
   procedure Sum_Results
     (Docs : Documents.List)
   is
      procedure Get_Results (S : Scenarios.Scenario_Type'Class) is
      begin
         Results (Result (S)) := @ + 1;
      end Get_Results;
   begin
      for D of Docs loop
         if D.Feature_List.Is_Empty and D.Scenario_List.Is_Empty then
            -- Empty Doc should be reported
            Results (Empty) := @ + 1;
         end if;

         for Scen of D.Scenario_List loop
            Get_Results (Scen);
         end loop;

         for F of D.Feature_List loop
            if F.Scenario_List.Is_Empty then
               -- Empty Feature should be reported
               Results (Empty) := @ + 1;
            end if;

            for Scen of F.Scenario_List loop
               Get_Results (Scen);
            end loop;

         end loop;
      end loop;
   end Sum_Results;

   -- --------------------------------------------------------------------------
   function Success return Boolean is
     (Results (Failed) = 0 and Results (Empty) = 0);

   function No_Fail return Boolean is (Results (Failed) = 0);

   -- --------------------------------------------------------------------------
   function Count (Test : Test_Result) return Natural is (Results (Test));

   -- --------------------------------------------------------------------------
   procedure Generate_Badge is

      function Generate_URL return String is
         use Ada.Strings;
         use Ada.Strings.Fixed;
         Passed_Text : constant String :=
                         Trim (Natural'Image (Results (Successful)),
                               Side => Left);
         Failed_Text : constant String :=
                         Trim (Natural'Image (Results (Failed)),
                               Side => Left);
         Empty_Text  : constant String :=
                         Trim (Natural'Image (Results (Empty)),
                               Side => Left);
         Color       : constant String :=
                         (if Results (Failed) > 0 then "red"
                          else (if Results (Empty) > 0 then "orange"
                            else "blue"));
      begin
         return "https://img.shields.io/badge/bbt-"
           & Passed_Text & "_tests_passed_|_"
           & Failed_Text & "_failed_|_"
           & Empty_Text & "_empty-"
           & Color & ".svg?style=flat-square";
      end Generate_URL;
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create   (File, Name => BBT.Settings.Badge_File_Name);
      Ada.Text_IO.Put_Line (File, Generate_URL);
      Ada.Text_IO.Close    (File);
   end Generate_Badge;

   -- --------------------------------------------------------------------------
   Blank_Image : constant Count_String := [others => ' '];

   -- --------------------------------------------------------------------------
   function Count_String_Image (Test : Test_Result) return Count_String is
      Img : constant String := Results (Test)'Image;
   begin
      return Ada.Strings.Fixed.Overwrite (Source   => Blank_Image,
                                          Position => 1,
                                          New_Item => Img);
   end Count_String_Image;


end BBT.Tests.Results;
