-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

package body BBT.Writers is

   -- --------------------------------------------------------------------------
   Writer_List : array (Format) of Interface_Access;
   Enabled     : array (Format) of Boolean := [others => False];

   use BBT.IO;
   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := Output)
                             renames BBT.IO.Put_Line;
   pragma Warnings (Off, Put_Debug_Line);

   -- -------------------------------------------------------------------------
   function Is_Enabled (F : Format) return Boolean is (Enabled (F));

   function File_Extensions (For_Format : Format) return String is
     (File_Extensions (Writer_List (For_Format).all));
   -- Dispatching call

   function Default_Extension (For_Format : Format) return String is
     (Default_Extension (Writer_List (For_Format).all));
   -- Dispatching call

   -- -------------------------------------------------------------------------
   procedure Enable_Output (For_Format : Format;
                            File_Name  : String := "") is
   begin
      Enabled (For_Format) := True;
      Enable_Output (Writer_List (For_Format).all, File_Name);
      if File_Name /= "" then
         IO.Enable_Tee (File_Name, Verbosity => Verbose);
      end if;
   end Enable_Output;

   procedure Put_Summary is
   begin
      for F in Writer_List'Range when Enabled (F) loop
         Put_Summary (Writer_List (F).all);
      end loop;
   end Put_Summary;

   procedure Put_Step_Result (Step     : BBT.Documents.Step_Type;
                              Success  : Boolean;
                              Fail_Msg : String;
                              Loc      : BBT.IO.Location_Type) is
   begin
      for F in Writer_List'Range when Enabled (F) loop
         Put_Step_Result (Writer_List (F).all,
                          Step,
                          Success,
                          Fail_Msg,
                          Loc);
      end loop;
   end Put_Step_Result;

   procedure Put_Overall_Results (Results : BBT.Results.Test_Results_Count) is
   begin
      for F in Writer_List'Range when Enabled (F) loop
         Put_Overall_Results (Writer_List (F).all, Results);
      end loop;
   end Put_Overall_Results;

   -- --------------------------------------------------------------------------
   procedure Put_Scenario (Writer     : Interface_Access;
                           Scenario   : Scenario_Type) is
   begin
      Put_Scenario_Title (Writer.all,
                          Line (Scenario.Location)'Image & ": Scenario """ &
                          (+Scenario.Name) & """");
      for Step of Scenario.Step_List loop
         Put_Step (Writer.all, Step);
      end loop;
      -- New_Line;
   end Put_Scenario;

   -- --------------------------------------------------------------------------
   procedure Put_Feature (Writer     : Interface_Access;
                          Feature    : Feature_Type) is
   -- Pref : constant String := "## ";
   begin
      -- Current_Indent_Level := 1;
      Put_Feature_Title (Writer.all,
                         Line (Feature.Location)'Image & ": Feature """ &
                         (+Feature.Name) & """");

      if Feature.Background /= null then
         Put_Scenario (Writer, Feature.Background.all);
      end if;

      for Scenario of Feature.Scenario_List loop
         Put_Scenario (Writer, Scenario);
      end loop;
      New_Line;
   end Put_Feature;

   -- --------------------------------------------------------------------------
   procedure Put_Document (Writer     : Interface_Access;
                           Doc        : Document_Type) is
   begin
      Put_Document_Title (Writer.all,
                          "Document Name : " & (+Doc.Name));
      New_Line;
      if Doc.Background /= null then
         Put_Scenario (Writer, Doc.Background.all);
      end if;

      for S of Doc.Scenario_List loop
         Put_Scenario (Writer, S);
      end loop;

      for Feature of Doc.Feature_List loop
         Put_Feature (Writer, Feature);
      end loop;
   end Put_Document;

   -- --------------------------------------------------------------------------
   procedure Put_Document_List (Doc_List : Documents_Lists.Vector) is
   begin
      for F in Writer_List'Range when Enabled (F) loop
         for Doc of Doc_List loop
            Put_Document (Writer_List (F), Doc);
         end loop;
      end loop;
   end Put_Document_List;

   -- --------------------------------------------------------------------------
   procedure Register (Writer     : Interface_Access;
                       For_Format : Format) is
   begin
      Writer_List (For_Format) := Writer;
   end Register;

end BBT.Writers;
