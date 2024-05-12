with BBT.Tests.Builder;

with Ada.Directories; use Ada.Directories;

package body BBT.Documents is

   -- type Indent_Level is range 0 .. 3;
   Prefix : constant Texts.Vector := [1 => "",
                                      2 => "  "];
   Current_Indent_Level : Positive := 1;

   -- --------------------------------------------------------------------------
   function To_Bold (S    : String;
                     Bold : Boolean) return String is
     (if Bold then "**" & S & "**" else S);
   function To_Bold (S : Unbounded_String;
                     Bold : Boolean) return String is
     (To_Bold (To_String (S), Bold));

   -- --------------------------------------------------------------------------
   procedure Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      S      :        Step_Type)
   is
      procedure Put_If_Not_Null (Prefix : String; S : Unbounded_String) is
      begin
         if S /= Null_Unbounded_String then
            Output.Put (Prefix & " " & S'Image);
         end if;
      end Put_If_Not_Null;
   begin
      Output.Put (S.Cat'Image & ", ");
      Output.Put (S.Action'Image);
      Put_If_Not_Null (", Step_String = ", S.Step_String);
      Put_If_Not_Null (", Object_String = ", S.Object_String);
      Put_If_Not_Null (", Subject_String = ", S.Subject_String);
      Output.Put (", File_Type = " & S.File_Type'Image);
      --  if S.File_Type = Directory then
      --     Put_If_Not_Null (", Dir name =", S.File_Name);
      --  else
      --     Put_If_Not_Null (", File name =", S.File_Name);
      --  end if;
      if not S.File_Content.Is_Empty then
         Output.Put (S.File_Content'Image);
      end if;
   end Put_Image;

   -- --------------------------------------------------------------------------
   --  procedure Add_Fail (To : in out Scenario_Type) is
   --  begin
   --     To.Failed_Step_Count := @ + 1;
   --  end Add_Fail;
   --  procedure Add_Success (To : in out Scenario_Type) is
   --  begin
   --     To.Successful_Step_Count := @ + 1;
   --  end Add_Success;
   procedure Add_Result (Success : Boolean; To : in out Scenario_Type) is
   begin
      if Success then
         To.Successful_Step_Count := @ + 1;
      else
         To.Failed_Step_Count := @ + 1;
      end if;
   end Add_Result;

   -- --------------------------------------------------------------------------
   procedure Put_Text (The_Text : Text) is
      Pref : constant String := Prefix (Current_Indent_Level);
   begin
      for L of The_Text loop
         Put_Line (Pref & L);
      end loop;
   end Put_Text;

   -- --------------------------------------------------------------------------
   procedure Put_Step (Step               : Step_Type;
                       With_Comments      : Boolean;
                       With_Bold_Keywords : Boolean) is
      pragma Unreferenced (With_Comments, With_Bold_Keywords);
      Pref : constant String := Prefix (1);
   begin
      Put_Line (Pref & Step'Image);
   end Put_Step;

   -- --------------------------------------------------------------------------
   procedure Put_Scenario (Scenario           : Scenario_Type;
                           With_Comments      : Boolean;
                           With_Bold_Keywords : Boolean) is
   begin
      Current_Indent_Level := 1;
      declare
         Pref : constant String := Prefix (Current_Indent_Level) & "### ";
      begin
         New_Line;
         if With_Bold_Keywords then
            Put_Line (Pref & To_Bold ("Scenario", With_Bold_Keywords)
                      & ": " &  To_String ((Scenario.Name)));
         else
            Put_Line (Pref & "Scenario " & To_String ((Scenario.Name)));
         end if;
         New_Line;
         if With_Comments then
            Put_Text ((Scenario.Comment));
            New_Line;
         end if;
         for Step of Scenario.Step_List loop
            Put_Step (Step, With_Comments, With_Bold_Keywords);
         end loop;

      end;
   end Put_Scenario;

   -- --------------------------------------------------------------------------
   procedure Put_Feature (Feature            : Feature_Type;
                          With_Comments      : Boolean;
                          With_Bold_Keywords : Boolean) is
      Pref : constant String := "## ";
   begin
      Current_Indent_Level := 1;
      if With_Bold_Keywords then
         Put_Line (Pref & To_Bold ("Feature",
                   With_Bold_Keywords) & ": " & To_String (Feature.Name));
      else
         Put_Line (Pref & "Feature" & ": " & To_String (Feature.Name));
      end if;
      if With_Comments then
         New_Line;
         Put_Text (Feature.Comment);
      end if;
      for Scenario of Feature.Scenario_List loop
         Put_Scenario (Scenario, With_Comments, With_Bold_Keywords);
      end loop;
   end Put_Feature;

   -- --------------------------------------------------------------------------
   procedure Put_Document (Doc                : Document_Type;
                           With_Comments      : Boolean;
                           With_Bold_Keywords : Boolean) is
   begin
      Current_Indent_Level := 1;
      Put_Line ("# " & To_Bold (Doc.Name, With_Bold_Keywords));
      New_Line;
      if With_Comments then
         Put_Text (Doc.Comment);
         New_Line;
      end if;
      for Feature of Doc.Feature_List loop
         Put_Feature (Feature, With_Comments, With_Bold_Keywords);
      end loop;
   end Put_Document;

   -- --------------------------------------------------------------------------
   procedure Put_Document_List (Doc_List           : Documents_Lists.Vector;
                                With_Comments      : Boolean;
                                With_Bold_Keywords : Boolean) is
   begin
      Put_Line ("**Document list:**");
      New_Line;
      Put_Line ("[[TOC]]");
      New_Line;
      for Doc of Doc_List loop
         Put_Document (Doc,
                       With_Comments      => With_Comments,
                       With_Bold_Keywords => With_Bold_Keywords);
      end loop;
   end Put_Document_List;

   -- --------------------------------------------------------------------------
   function Result (Scenario : Scenario_Type) return Test_Result is
   begin
      if Scenario.Failed_Step_Count > 0 then
         return Failed;
      elsif Scenario.Successful_Step_Count > 0 then
         return Successful;
      else
         return Empty;
      end if;
   end Result;

   -- --------------------------------------------------------------------------
   procedure Put_Run_Summary is
      Failed_Step_Count     : Natural := 0;
      Successful_Step_Count : Natural := 0;
      Test_Result_Counts    : array (Test_Result) of Natural := [others => 0];
   begin
      for D of BBT.Tests.Builder.The_Tests_List.all loop
         for F of D.Feature_List loop
            for Scen of F.Scenario_List loop
               Successful_Step_Count := @ + Scen.Successful_Step_Count;
               Failed_Step_Count     := @ + Scen.Failed_Step_Count;
               Test_Result_Counts (Result (Scen)) := @ + 1;
            end loop;
         end loop;
      end loop;
      New_Line;
      Put_Line ("------------------------------------------------");
      Put_Line ("- Failed     tests = " & Test_Result_Counts (Failed)'Image);
      Put_Line ("- Successful tests = " & Test_Result_Counts (Successful)'Image);
      Put_Line ("- Empty      tests = " & Test_Result_Counts (Empty)'Image);
      if IO.Is_Authorized (Verbose) then
         New_Line;
         Put_Line ("- Failed     steps = " & Failed_Step_Count'Image);
         Put_Line ("- Successful steps = " & Successful_Step_Count'Image);
      end if;
   end Put_Run_Summary;

end BBT.Documents;
