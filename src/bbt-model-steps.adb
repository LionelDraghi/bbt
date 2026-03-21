-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Model.Scenarios,
     BBT.Tests.Filter_List,
     Ada.Directories;

use BBT.Model.Scenarios,
    BBT.Tests.Filter_List,
    Ada.Directories;

package body BBT.Model.Steps is

   -- --------------------------------------------------------------------------
   procedure Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      S      :        Step_Type) is
   begin
      Output.New_Line;
      Put_Image (Output, S.Data);
   end Put_Image;

   -- --------------------------------------------------------------------------
   procedure Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      S      :        Step_Data) is
   begin
      Output.Put ("   Step type        = " & S.Cat'Image & ", ");
      Output.New_Line;
      Output.Put ("   Action           = " & S.Action'Image);
      Output.New_Line;
      Output.Put ("   Src_Code         = " & S.Src_Code'Image);
      Output.New_Line;
      Output.Put ("   Subject string   = " & S.Subject_String'Image);
      Output.New_Line;
      Output.Put ("   Object_String    = " & S.Object_String'Image);
      Output.New_Line;
      Output.Put ("   Object_File_Name = " & S.Object_File_Name'Image);
      Output.New_Line;
      Output.Put ("   File_Type        = " & S.File_Type'Image);
      Output.New_Line;
      Output.Put ("   Ignore order     = " & S.Ignore_Order'Image);
      Output.New_Line;
      Output.Put ("   File_Content     = " & Code_Fenced_Image (S.File_Content));
      Output.New_Line;
   end Put_Image;

   -- --------------------------------------------------------------------------
   function Explain (Step : Step_Type) return String is
      Prefix_Start : constant String := Line (Step.Location)
        & (if Step.Filtered then " [filtered] " else "") & ": - ";
      Prefix_End : constant String :=
        (case Step.Data.Cat is
         when Unknown    => "Unknown step type ??? ",
         when Given_Step => "Ensure ",
         when When_Step  => "Run ",
         when Then_Step  => "Check ");
      Prefix : constant String := Prefix_Start & Prefix_End;
      function File_Kind return String is
         (case Step.Data.File_Type is when Directory     => "directory",
                                      when Ordinary_File => "file",
                                      when Special_File  => "special file  ");
      function Executable return String is
         (if Step.Data.Executable_File then " (executable)" else " ");

      Obj_String_Available   : constant Boolean := Step.Data.Object_String /= "";
      File_Name_Available    : constant Boolean := Step.Data.Object_File_Name /= "";
      File_Content_Available : constant Boolean := not Is_Empty (Step.Data.File_Content);

      function Optional_Content (Prefix_If_Found : String;
                                 If_Not_Found    : String := "") return String is
         (if    Obj_String_Available   then Prefix_If_Found & "`" & (+Step.Data.Object_String) & "`"
          elsif File_Name_Available    then Prefix_If_Found & "`" & (+Step.Data.Object_File_Name) & "` file content"
          elsif File_Content_Available then Prefix_If_Found & Code_Fenced_Image (Step.Data.File_Content)
          else If_Not_Found);

      function Output_Is return String is
         (if    Obj_String_Available   then "that output is `" & (+Step.Data.Object_String) & "`"
          elsif File_Content_Available then "that output is " & Code_Fenced_Image (Step.Data.File_Content)
          else " that output is as in file `" & (+Step.Data.Object_File_Name) & "`");

      function Output_Contains return String is
         (if    Obj_String_Available   then "that output contains `" & (+Step.Data.Object_String) & "`"
          elsif File_Content_Available then "that output contains " & Code_Fenced_Image (Step.Data.File_Content)
          else " that output contains as in file `" & (+Step.Data.Object_File_Name) & "`");

      function Output_Does_Not_Contain return String is
         (if    Obj_String_Available   then "that output does not contain `" & (+Step.Data.Object_String) & "`"
          elsif File_Content_Available then "that output does not contain " & Code_Fenced_Image (Step.Data.File_Content)
          else " that output does not contain as in file `" & (+Step.Data.Object_File_Name) & "`");

   begin
      -- Put_Line ("File_Content_Available = " & File_Content_Available'Image & " Optional_Containing " & Code_Fenced_Image (Step.Data.File_Content));

      case Step.Data.Action is
      when None => return Prefix & "Uninitialized Step ???";

      -- Setup actions
      when Setup_No_File =>
         return Prefix & "that there is no file `"
                & (+Step.Data.Object_File_Name) & "` (erase it if needed)";

      when Setup_No_Dir =>
        return Prefix & "that there is no directory `"
               & (+Step.Data.Object_File_Name)  & "` (erase it if needed)";

      when Check_File_Existence =>
         return Prefix & "that file `" & (+Step.Data.Object_File_Name)
                & "` exists";

      when Check_Dir_Existence  =>
        return Prefix & "that directory `" & (+Step.Data.Object_File_Name)
               & "` exists";

      when Erase_And_Create     =>
        return Prefix & "that there is a new " & File_Kind
               & " named `" & (+Step.Data.Subject_String) & "`"
               & Optional_Content (Prefix_If_Found => " containing ")
               & "(create if none, overwrite otherwise)";

      when Create_If_None       =>
        return Prefix & "that there is a " & File_Kind
               & " named `" & (+Step.Data.Subject_String) & "`"
               & Optional_Content (Prefix_If_Found => " containing ")
               & "(create if none, fail otherwise)";

      -- Run actions
      when Run_Cmd              =>
         return Prefix_Start & "Run command `" & (+Step.Data.Object_String) & "`"
                & Executable;

      when Run_Without_Error =>
        return Prefix_Start & "Run command `" & (+Step.Data.Object_String) & "`"
               & Executable
               & "and check that it does not return an error";

      -- Check actions
      when Check_No_File =>
        return Prefix & "that file `" & (+Step.Data.Object_File_Name)
               & "` does not exist";

      when Check_No_Dir =>
        return Prefix & "that directory `" & (+Step.Data.Object_File_Name)
               & "` does not exist";

      when Error_Return_Code =>
        return Prefix & "that command returns an error";

      when No_Error_Return_Code =>
        return Prefix & "that command does not return an error";

      when Output_Is =>
         return Prefix & Output_Is;

      when Output_Contains =>
         return Prefix & Output_Contains;

      when Output_Does_Not_Contain =>
         return Prefix & Output_Does_Not_Contain;

      when Output_Matches =>
         return Prefix & "that command output matches regex `"
                & (+Step.Data.Object_String) & "`";

      when Output_Does_Not_Match =>
         return Prefix & "that command output does not match regex `"
                & (+Step.Data.Object_String) & "`";

      when File_Matches =>
         return Prefix & "that file `" & (+Step.Data.Subject_String)
                & "` content matches regex `" & (+Step.Data.Object_String) & "`";

      when File_Does_Not_Match =>
         return Prefix & "that file `" & (+Step.Data.Subject_String)
                & "` content does not match regex `"
                & (+Step.Data.Object_String) & "`";

      when File_Is =>
         return Prefix & "that file `" & (+Step.Data.Subject_String) & "`"
                & Optional_Content (Prefix_If_Found => " content is exactly ",
                                    If_Not_Found    => " ?????????? exists");

      when File_Is_Not =>
         return Prefix & "that file `" & (+Step.Data.Subject_String)
                & Optional_Content (Prefix_If_Found => "` content is not exactly `",
                                    If_Not_Found    => " ??????????? exists");

      when File_Contains =>
         return Prefix & "that file `" & (+Step.Data.Subject_String)
                & (if Step.Data.Ignore_Order then " (ignoring lines order)"
                   else "")
                & Optional_Content (Prefix_If_Found => " contains ");

      when File_Does_Not_Contain =>
         return Prefix & "that file `" & (+Step.Data.Subject_String)
                & (if Step.Data.Ignore_Order then " (ignoring lines order)"
                   else "")
                & Optional_Content (Prefix_If_Found => " does not contain ");

      when No_Output =>
         return Prefix & "that last command produces no output";

      end case;

   end Explain;

   -- --------------------------------------------------------------------------
   function Create_Step (Info            : Step_Data;
                         Loc             : Location_Type;
                         Parent_Scenario : Scenarios.Scenario_Access)
                         return Step_Type
   is (Filtered    => <>,
       Location    => Loc,
       Comment     => <>,
       Name        => To_Unbounded_String ("step"),
       Start_Time  => <>,
       End_Time    => <>,
       Data        => Info,
       Parent      => Node_Access (Parent_Scenario));

   -- --------------------------------------------------------------------------
   procedure Set_Filter (S        : in out Step_Type'Class;
                         Filtered :        Boolean) is
   begin
      Put_Debug_Line ("Set_Filter (Step => " & (+S.Data.Src_Code)
                      & ", To => " & Filtered'Image);
      S.Filtered := Filtered;
   end Set_Filter;

   -- --------------------------------------------------------------------------
   procedure Set_Has_Syntax_Error (S         : in out Step_Type'Class;
                                   Has_Error :        Boolean) is
   begin
      Put_Debug_Line ("Set_Has_Syntax_Error (Step => " & (+S.Data.Src_Code)
                      & ", To => " & Has_Error'Image);
      S.Data.Syntax_Error := Has_Error;
   end Set_Has_Syntax_Error;

   -- --------------------------------------------------------------------------
   function Has_Syntax_Error (S : in out Step_Type'Class) return Boolean is
      (S.Data.Syntax_Error);

   -- --------------------------------------------------------------------------
   overriding procedure Apply_Filters_To (S : in out Step_Type) is
      Result : constant Filter_Result := Is_Filtered (+S.Data.Src_Code, Step);
   begin
      Put_Debug_Line ("Apply_Filters_To step '" & (+S.Data.Src_Code) & "'");
      case Result is
         when Selected =>
            Put_Debug_Line ("Step selected : '" & (+S.Data.Src_Code) & "'");
            Unfilter_Parents (S);
            Unfilter (S);
            -- if One step is selected, we must mark the parent scenario
            -- as selected, and possibly Background, etc.

         when  Filtered =>
            Put_Debug_Line ("Step filtered : '" & (+S.Data.Src_Code) & "'");
            Filter (S);

         when No_Match => null;
            Put_Debug_Line ("Step ignored : '" & (+S.Data.Src_Code) & "'");

      end case;
   end Apply_Filters_To;

   -- -------------------------------------------------------------------------
   function Parent (S : Step_Type) return Scenarios.Scenario_Access is
     (Scenario_Access (S.Parent));

   -- --------------------------------------------------------------------------
   function Last_Step
     (S : in out Step_Lists.Vector) return Step_Maybe
   is
     (Step_Maybe (S.Reference (S.Last).Element));

end BBT.Model.Steps;
