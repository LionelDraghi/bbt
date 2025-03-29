-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

package body BBT.Writers is

   -- -----------------------------------------------------------------------
   use BBT.IO;
   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := IO.Writers)
                             renames BBT.IO.Put_Line;
   pragma Warnings (Off, Put_Debug_Line);

   -- --------------------------------------------------------------------------
   Writer_List : array (Output_Format) of Interface_Access;
   Enabled     : array (Output_Format) of Boolean := [others => False];

   -- -------------------------------------------------------------------------
   function Is_Enabled (F : Output_Format) return Boolean is (Enabled (F));
   function No_Output_Format_Enabled return Boolean is
     (for all F in Enabled'Range => Enabled (F) = False);

   function File_Pattern (For_Format : Output_Format) return String is
     (File_Pattern (Writer_List (For_Format).all));
   -- Dispatching call

   -- -------------------------------------------------------------------------
   function Default_Extension (For_Format : Output_Format) return String is
     (Default_Extension (Writer_List (For_Format).all));
   -- Dispatching call

   -- -------------------------------------------------------------------------
   procedure File_Format (File_Name    :     String;
                          Found        : out Boolean;
                          Found_Format : out Output_Format) is
   begin
      Put_Debug_Line ("Writers.File_Format");
      Found        := False;
      Found_Format := Output_Format'Last;
      for F in Writer_List'Range when Writer_List (F) /= null loop
         if Is_Of_The_Format (Writer_List (F).all,
                              File_Name)
         then
            Found        := True;
            Found_Format := F;
            return;
         end if;
      end loop;
   end File_Format;

   -- -------------------------------------------------------------------------
   procedure Enable_Output (For_Format : Output_Format;
                            File_Name  : String := "") is
   begin
      Put_Debug_Line ("Writers.Enable_Output (For_Format => " &
                        For_Format'Image & ", File_Name => " & File_Name);
      Enabled (For_Format) := True;
      Enable_Output (Writer_List (For_Format).all, File_Name);
      if File_Name /= "" then
         IO.Enable_Tee (File_Name, Verbosity => Verbose);
      end if;
   end Enable_Output;

   -- -------------------------------------------------------------------------
   procedure Put_Summary is
   begin
      Put_Debug_Line ("Writers.Put_Summary");
      for F in Writer_List'Range when Enabled (F) loop
         Put_Summary (Writer_List (F).all);
      end loop;
   end Put_Summary;

   -- -------------------------------------------------------------------------
   procedure Put_Step_Result (Step     : BBT.Documents.Step_Type;
                              Success  : Boolean;
                              Fail_Msg : String;
                              Loc      : BBT.IO.Location_Type) is
   begin
      for F in Writer_List'Range when Enabled (F) loop
         Put_Debug_Line (Inline_Image (Step));
         Put_Step_Result (Writer_List (F).all,
                          Step,
                          Success,
                          Fail_Msg,
                          Loc);
      end loop;
   end Put_Step_Result;

   -- -------------------------------------------------------------------------
   procedure Put_Overall_Results
     (Results : BBT.Tests.Results.Test_Results_Count) is
   begin
      for F in Writer_List'Range when Enabled (F) loop
         Put_Overall_Results (Writer_List (F).all, Results);
      end loop;
   end Put_Overall_Results;

   -- --------------------------------------------------------------------------
   procedure Put_Scenario (Writer     : Interface_Access;
                           Scenario   : Scenario_Type) is
   begin
      if Scenario.Filtered then
         Put_Debug_Line ("Scenario " & (+Scenario.Name) & " is filtered");
      else
         Put_Scenario_Title (Writer.all,
                             Scenario.Location'Image & " Scenario """ &
                             (+Scenario.Name) & """");
         for Step of Scenario.Step_List loop
            if Step.Filtered then
               Put_Debug_Line ("Filtered Step " & (+Step.Step_String));
            else
               Put_Step (Writer.all, Step);
            end if;
         end loop;
         -- New_Line;
      end if;
   end Put_Scenario;

   -- --------------------------------------------------------------------------
   procedure Put_Feature (Writer     : Interface_Access;
                          Feature    : Feature_Type) is
   begin
      if Feature.Filtered then
         Put_Debug_Line ("Feature " & (+Feature.Name) & " is filtered");
      else
         Put_Feature_Title (Writer.all,
                            Feature.Location'Image & " Feature """ &
                            (+Feature.Name) & """");

         if Feature.Background /= null then
            Put_Scenario (Writer, Feature.Background.all);
         end if;

         for Scenario of Feature.Scenario_List loop
            Put_Scenario (Writer, Scenario);
         end loop;
         New_Line;
      end if;
   end Put_Feature;

   -- --------------------------------------------------------------------------
   procedure Put_Document (Writer     : Interface_Access;
                           Doc        : Document_Type) is
   begin
      if Doc.Filtered then
         Put_Debug_Line ("Document " & (+Doc.Name) & " is filtered");
      else
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
      end if;
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
                       For_Format : Output_Format) is
   begin
      Writer_List (For_Format) := Writer;
   end Register;

end BBT.Writers;
