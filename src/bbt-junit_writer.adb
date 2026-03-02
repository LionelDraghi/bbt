-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Directories,
     Ada.Text_IO,
     Ada.Strings.Unbounded,
     Ada.Strings.Fixed;

with BBT.Model.Scenarios,
     BBT.Model,
     Ada.Strings;

package body BBT.JUnit_Writer is

   use Ada.Directories;
   use Ada.Text_IO;
   use Ada.Strings;
   use Ada.Strings.Unbounded;
   use Ada.Strings.Fixed;
   use BBT.Model;

   -- --------------------------------------------------------------------------
   -- Helper functions to calculate test counts using existing model functions
   -- --------------------------------------------------------------------------
   function Total_Test_Count (Results : Test_Results_Count) return Natural is
     (Results (Successful) + Results (Failed) + Results (Not_Run));

   function Total_Failure_Count (Results : Test_Results_Count) return Natural is
     (Results (Failed));

   function Total_Skipped_Count (Results : Test_Results_Count) return Natural is
     (Results (Not_Run));

   -- --------------------------------------------------------------------------
   -- XML escaping function for special characters
   -- --------------------------------------------------------------------------
   function Escape_XML (Text : String) return String is
      Result : Unbounded_String := To_Unbounded_String (Text);
      Position : Natural := 1;
   begin
      -- Process the string character by character
      while Position <= Length (Result) loop
         case Element (Result, Position) is
            when '&' =>
               Replace_Element (Result, Position, '&');
               Insert (Result, Position + 1, "amp;");
               Position := Position + 5;
            when '"' =>
               Replace_Element (Result, Position, '&');
               Insert (Result, Position + 1, "quot;");
               Position := Position + 6;
            when Character'Val (39) =>
               Replace_Element (Result, Position, '&');
               Insert (Result, Position + 1, "apos;");
               Position := Position + 6;
            when '<' =>
               Replace_Element (Result, Position, '&');
               Insert (Result, Position + 1, "lt;");
               Position := Position + 4;
            when '>' =>
               Replace_Element (Result, Position, '&');
               Insert (Result, Position + 1, "gt;");
               Position := Position + 4;
            when others =>
               Position := Position + 1;
         end case;
      end loop;

      return To_String (Result);
   end Escape_XML;

   -- --------------------------------------------------------------------------
   -- Helper function to generate XML attributes in the format name="value"
   -- Automatically trims the value and escapes XML special characters
   -- --------------------------------------------------------------------------
   function Attrib (Name, Value : String) return String is
   begin
      return Name & "=""" & Escape_XML (Trim (Value, Ada.Strings.Both)) & """";
   end Attrib;

   function Time_Attrib (Time : Duration) return String is
   begin
      return "time=""" & Trim (Duration'Image (Time), Ada.Strings.Left) & """";
   end Time_Attrib;

   -- --------------------------------------------------------------------------
   procedure Generate_Report (File_Name : String;
                              Docs      : BBT.Model.Documents.List) is
      XML_File : File_Type;

      -- -----------------------------------------------------------------------
      procedure Write_Test_Case (XML_File : File_Type;
                                 Scen     : BBT.Model.Scenarios.Scenario_Type'Class;
                                 Suite_Name : String) is
      begin
         if Scen.Failed_Step_Count = 0 then
            -- Put opening tag and closing tag on the same line for simple testcases
            Put_Line (XML_File, "    <testcase " &
                      Attrib ("name", To_String (Scen.Name)) & " " &
                      Attrib ("classname", Suite_Name) & " " &
                      Time_Attrib (Elapsed_Time (Scen)) & "/>");
         else
            Put_Line (XML_File, "    <testcase " &
                      Attrib ("name", To_String (Scen.Name)) & " " &
                      Attrib ("classname", Suite_Name) & " " &
                      Time_Attrib (Elapsed_Time (Scen)) & ">");
            Put_Line (XML_File, "      <failure message=""Test failed"">");
            Put (XML_File, "        Failed steps: " & Scen.Failed_Step_Count'Image);
            Put_Line (XML_File, "      </failure>");
            Put_Line (XML_File, "    </testcase>");
         end if;
      end Write_Test_Case;

   begin
      -- Open XML file
      Create (XML_File, Out_File, File_Name);

      -- Write XML header
      Put_Line (XML_File, "<?xml version=""1.0"" encoding=""UTF-8""?>");

      -- Use existing Get_Results function to count tests, failures, errors, and skipped
      declare
         Total_Results : constant Test_Results_Count := BBT.Model.Documents.Get_Results (Docs);
      begin
         Suite_Name : constant String := Base_Name (File_Name);
         -- Use the XML filename (without extension) as the testsuites name
         Put_Line (XML_File, "<testsuites " &
                   Attrib ("name", Suite_Name) & " " &
                   Attrib ("tests", Total_Test_Count (Total_Results)'Image) & " " &
                   Attrib ("failures", Total_Failure_Count (Total_Results)'Image) & " " &
                   Attrib ("errors", "0") & " " &
                   Attrib ("skipped", Total_Skipped_Count (Total_Results)'Image) & " " &
                   Time_Attrib (Elapsed_Time (Docs.First_Element)) & ">"); -- FIXME: total time for all docs not implemented
      end;

      -- Process all documents
      for Doc of Docs loop
         if not Doc.Filtered then
            -- Write testsuite for this document
            -- Determine suite name (empty for documents without features, as per JUnit spec)
            -- and classname (Simple test for consistency with expected output)
            Suite_Name : constant String :=
               (if Doc.Feature_List.Is_Empty and then not Doc.Scenario_List.Is_Empty then
                    ""  -- Empty name for documents without features
                else
                    To_String (Doc.Name));
            Class_Name : constant String :=
               (if Doc.Feature_List.Is_Empty and then not Doc.Scenario_List.Is_Empty then
                    ""  -- Empty classname for documents without features
                else
                    To_String (Doc.Name));

            -- Count tests in this document
               -- Use existing Get_Results function to count tests, failures, and skipped
               declare
                  Doc_Results : constant Test_Results_Count := BBT.Model.Documents.Get_Results (Doc);
               begin
                  Put_Line (XML_File, "  <testsuite " &
                    Attrib ("name", Suite_Name) & " " &
                    Attrib ("tests", Total_Test_Count (Doc_Results)'Image) & " " &
                    Attrib ("failures", Total_Failure_Count (Doc_Results)'Image) & " " &
                    Attrib ("errors", "0") & " " &
                    Attrib ("skipped", Total_Skipped_Count (Doc_Results)'Image) & " " &
                    Time_Attrib (Elapsed_Time (Doc)) & ">");
               end;

               -- Write testcases from features
               for Feat of Doc.Feature_List loop
                  if not Feat.Filtered then
                     for Scen of Feat.Scenario_List loop
                        if not Scen.Filtered then
                           Write_Test_Case (XML_File, Scen, Class_Name);
                        end if;
                     end loop;
                  end if;
               end loop;

               -- Write testcases from scenarios
               for Scen of Doc.Scenario_List loop
                  if not Scen.Filtered then
                     Write_Test_Case (XML_File, Scen, Class_Name);
                  end if;
               end loop;

               Put_Line (XML_File, "  </testsuite>");
         end if;
      end loop;

      -- Close XML
      Put_Line (XML_File, "</testsuites>");
      Close (XML_File);
   end Generate_Report;

end BBT.JUnit_Writer;
