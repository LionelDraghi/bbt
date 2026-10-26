-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
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
      Output.Put ("   Negated          = " & S.Negated'Image);
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
      Output.Put ("   Syntax_Error     = " & S.Syntax_Error'Image);
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

      function Object_File_Name return String is
         (Wrap_In_Backticks (+Step.Data.Object_File_Name));
      function Object_String return String is
         (Wrap_In_Backticks (+Step.Data.Object_String));
      function Subject_String return String is
         (Wrap_In_Backticks (+Step.Data.Subject_String));
      function Code_Fenced_File_Content return String is
         (Code_Fenced_Image (Step.Data.File_Content));

      function Optional_Content (Prefix_If_Found : String;
                                 If_Not_Found    : String := "") return String is
         (if    Obj_String_Available   then Prefix_If_Found & Object_String
          elsif File_Name_Available    then Prefix_If_Found & Object_File_Name
          elsif File_Content_Available then Prefix_If_Found & Code_Fenced_File_Content
          else If_Not_Found);

      --  The comparison step explanations, parameterized by the checked
      --  flow: "output" for the standard output, "error output" for the
      --  standard error
      function Output_Is (Subject : String := "output") return String is
         (if    Obj_String_Available   then
            "that the " & Subject & " is"
            & (if Step.Data.Negated then " not " else " ") & Object_String
          elsif File_Content_Available then
            "that the " & Subject & " is"
            & (if Step.Data.Negated then " not " else " ")
            & Code_Fenced_File_Content
          else
            "that the " & Subject & " is"
            & (if Step.Data.Negated then " different from the content of file "
               else " equal to the content of file ")
            & Object_File_Name);

      function Output_Contains (Subject : String := "output") return String is
         (if    Obj_String_Available   then
            "that the " & Subject
            & (if Step.Data.Negated then " does not contain " else " contains ")
            & Object_String
          elsif File_Content_Available then
            "that the " & Subject
            & (if Step.Data.Negated then " does not contain " else " contains ")
            & Code_Fenced_File_Content
          else
            "that the " & Subject
            & (if Step.Data.Negated then " does not contain " else " contains ")
            & "the content of file " & Object_File_Name);

      --  The (ignoring ...) clause is part of the comparison semantics:
      --  it shows in the explanation, with the same syntax as in the
      --  step line
      function Ignoring_Clause return String is
         Patterns : constant String :=
                      Text_Utilities.Image (Step.Data.Ignore_Patterns);
      begin
         if Patterns = "" and then not Step.Data.Ignore_Order
            and then Step.Data.Ignore_Regexps.Is_Empty
         then
            return "";
         end if;
         declare
            R     : Unbounded_String := To_Unbounded_String (" (ignoring ");
            First : Boolean          := True;

            procedure Add (Item : String) is
            begin
               if not First then
                  Append (R, ", ");
               end if;
               Append (R, Item);
               First := False;
            end Add;

         begin
            if Patterns /= "" then
               Add (Patterns);
            end if;
            if Step.Data.Ignore_Order then
               Add ("order");
            end if;
            for Regexp of Step.Data.Ignore_Regexps loop
               Add ("/" & Regexp & "/");
            end loop;
            Append (R, ")");
            return To_String (R);
         end;
      end Ignoring_Clause;

   begin
      case Step.Data.Action is
      when None =>
         return Prefix_Start & "Unrecognized step";

      -- Setup actions
      when Check_File_Existence =>
         return Prefix
           & (if Step.Data.Negated
              then (if Step.Data.Cat = Given_Step
                    then "that there is no file " & Object_File_Name
                         & " (erase it if needed)"
                    else "that the file " & Object_File_Name
                         & " does not exist")
              else "that the file " & Object_File_Name & " exists");

      when Check_Dir_Existence  =>
        return Prefix
          & (if Step.Data.Negated
             then (if Step.Data.Cat = Given_Step
                   then "that there is no directory " & Object_File_Name
                        & " (erase it if needed)"
                   else "that the directory " & Object_File_Name
                        & " does not exist")
             else "that the directory " & Object_File_Name & " exists");

      when Erase_And_Create     =>
        return Prefix_Start & "Create a new " & File_Kind
               & " (overwrite if existing) named " & Subject_String
               & Optional_Content (Prefix_If_Found => " containing ");

      when Create_If_None       =>
        return Prefix_Start & "Create a " & File_Kind
               & (if Step.Data.File_Type = Directory
                  then " (if not already existing) named "
                  else " (overwritten if content differs) named ")
               & Subject_String
               & Optional_Content (Prefix_If_Found => " containing ");

      -- Run actions
      when Run_Cmd              =>
         return Prefix_Start & "Run command " & Object_String & Executable;

      when Run_Without_Error =>
        return Prefix_Start & "Run command " & Object_String & Executable
               & " and check that it does not return an error";

      when Run_With_Error =>
        return Prefix_Start & "Run command " & Object_String & Executable
               & " and check that it fails (returns an error)";

      when Run_Shell_Cmd =>
        return Prefix_Start & "Run command " & Object_String
               & " in a shell";

      when Run_Shell_Without_Error =>
        return Prefix_Start & "Run command " & Object_String
               & " in a shell and check that it does not return an error";

      -- Interactive input actions
      when Type_Text =>
        return Prefix_Start & "Type " & Object_String
               & " to the running command, without trailing newline";

      when Enter_Text =>
        return Prefix_Start & "Enter " & Object_String
               & " followed by a newline to the running command";

      -- Check actions
      when Error_Return_Code =>
        return Prefix & (if Step.Data.Negated
                         then "that the command does not return an error"
                         else "that the command returns an error");

      when Output_Is =>
         return Prefix & Output_Is & Ignoring_Clause;

      when Output_Contains =>
         return Prefix & Output_Contains & Ignoring_Clause;

      when Output_Matches =>
         return Prefix & (if Step.Data.Negated
                          then "that the command output does not match regex "
                          else "that the command output matches regex ")
                & Object_String & Ignoring_Clause;

      when File_Matches =>
         return Prefix & "that the content of file " & Subject_String
                & (if Step.Data.Negated
                   then " does not match regex "
                   else " matches regex ")
                & Object_String & Ignoring_Clause;

      when File_Is =>
         return Prefix & "that the content of file " & Subject_String
                & Optional_Content
                  (Prefix_If_Found => (if Step.Data.Negated
                                       then " is different from "
                                       else " is exactly "),
                   If_Not_Found    => " is missing"
                                      & " (no expected content provided)")
                & Ignoring_Clause;

      when File_Contains =>
         return Prefix & "that the file " & Subject_String
                & Optional_Content
                  (Prefix_If_Found => (if Step.Data.Negated
                                       then " does not contain "
                                       else " contains "))
                & Ignoring_Clause;

      when Output_Not_Empty =>
         return Prefix & (if Step.Data.Negated
                          then "that the last command produces no output"
                          else "that the last command produces output");

      when Set_Env_Var =>
         return Prefix_Start
           & (if Step.Data.Negated then "Unset" else "Set")
           & " environment variable " & Subject_String
           & (if Step.Data.Negated then "" else " to " & Object_String)
           & " for the scenario";

      when Stderr_Is =>
         return Prefix & Output_Is ("error output") & Ignoring_Clause;

      when Stderr_Contains =>
         return Prefix & Output_Contains ("error output") & Ignoring_Clause;

      when Stderr_Not_Empty =>
         return Prefix & (if Step.Data.Negated
                          then "that the last command produces no error output"
                          else "that the last command produces error output");

      when Exit_Code_Is =>
         return Prefix & "that the exit code of the last command is"
                & (if Step.Data.Negated then " not" else "")
                & " " & Object_String;

      end case;

   end Explain;

   -- --------------------------------------------------------------------------
   procedure Set_Parent  (Step            : in out Step_Type;
                          Parent_Scenario :        Scenarios.Scenario_Access) is
   begin
      Step.Parent := Node_Access (Parent_Scenario);
   end Set_Parent;

   -- --------------------------------------------------------------------------
   function Get_Expected (Step : Step_Type'Class) return Text is
      use type Text;
      function File_Exists (Name : String) return Boolean is
        (Name /= ""
         and then Exists (Name)
         and then Kind (Name) = Ordinary_File);
   begin
      if Step.Data.File_Content /= Empty_Text then
         -- File content provided in code fenced lines
         Put_Debug_Line ("======= Get_Expected returning Text" & Step.Data.File_Content'Image);
         return Step.Data.File_Content;

      elsif Step.Data.Object_File_Name /= Null_Unbounded_String
        and then File_Exists (+Step.Data.Object_File_Name)
      then
         -- The string denotes a file
         declare
            T : constant Text := Get_Text (+Step.Data.Object_File_Name);
         begin
            Put_Debug_Line ("======= Get_Expected returning content of file " & Step.Data.Object_File_Name'Image);
            return T;
         end;

      elsif Step.Data.Object_String /= Null_Unbounded_String then
         -- The string is the content
         Put_Debug_Line ("======= Get_Expected returning string content" & Step.Data.Object_String'Image);
         return [1 => +Step.Data.Object_String];

      else
         -- Either the provided file content was null (two consecutive code
         -- fence marks), or there is an error somewhere in the scenario.
         -- But scenario errors are supposed to be caught during scenario
         -- analysis, and the run stopped before reaching this point,
         -- unless run with "--keep_going".
         -- In both cases, returning an Empty_Text seems to be the right
         -- things to do.
         Put_Debug_Line ("======= Get_Expected returning empty Text");
         return Empty_Text;
      end if;
   end Get_Expected;

   -- --------------------------------------------------------------------------
   function Create_Step (Info            : Step_Data;
                         Loc             : Location_Type;
                         Parent_Scenario : access Scenarios.Scenario_Type)
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
            -- If One step is selected, we must mark the parent scenario
            -- as selected, and possibly Background, etc.

         when Filtered =>
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
