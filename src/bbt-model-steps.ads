-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Containers.Indefinite_Vectors,
     Ada.Directories,
     Ada.Strings.Text_Buffers;

limited with BBT.Model.Scenarios;

package BBT.Model.Steps is

-- --------------------------------------------------------------------------
   type Extended_Step_Categories is
     (Unknown,
      Given_Step, -- Precond
      When_Step,  -- Action
      Then_Step); -- Checks
   subtype Step_Categories is Extended_Step_Categories range
     Extended_Step_Categories'Succ (Unknown) ..
     Extended_Step_Categories'Last;

   --  Every check action is declared in its positive form; the negated
   --  forms ("no", "not", "does not"...) are not part of the grammar:
   --  they set the step Negated flag below, and the check procedures
   --  invert their result accordingly (cf. Negatable in
   --  BBT.Scenarios.Steps for the list of negatable actions).
   type Actions is (None,
                    -- Setup actions
                    Check_File_Existence,
                    -- negated: Given, erase the file if needed;
                    --          Then, check that the file doesn't exist
                    Check_Dir_Existence,
                    -- negated: same, for directories
                    Erase_And_Create,
                    Create_If_None,
                    Set_Env_Var,
                    -- negated: unset the environment variable
                    -- Run actions
                    Run_Cmd,
                    Run_Without_Error,
                    Run_With_Error,
                    Run_Shell_Cmd,
                    Run_Shell_Without_Error,
                    -- Interactive input actions
                    Type_Text,
                    Enter_Text,
                    -- Check actions
                    Error_Return_Code,
                    -- negated: no error return code
                    Output_Is,
                    -- negated: output is different from expected
                    Output_Not_Empty,
                    -- negated: empty output
                    Output_Contains,
                    -- negated: output does not contain expected
                    Output_Matches,
                    -- negated: output does not match expected
                    File_Is,
                    -- negated: file content is different from expected
                    File_Contains,
                    -- negated: file does not contain expected
                    File_Matches,
                    -- negated: file does not match expected
                    Stderr_Is,
                    -- negated: error output is different from expected
                    Stderr_Contains,
                    -- negated: error output does not contain expected
                    Stderr_Not_Empty,
                    -- negated: empty error output
                    Exit_Code_Is);
                    -- negated: exit code different from expected

   type Step_Type;
   type Step_Maybe is access all Step_Type;

   --  -------------------------------------------------------------------------
   --  A step may include multiple commands with the syntax
   --  "-When I run "cmd1" or "cmd2" or "cmd3" ..."
   package Cmd_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, String);
   subtype Cmd_List is Cmd_Lists.Vector;
   Empty_Cmd_List : Cmd_Lists.Vector := Cmd_Lists.Empty_Vector;

   function Filtered_By_Default return Boolean is (Settings.Selection_Mode);
   -- If in Selection mode, then items are filtered by default, and selected
   -- items should be provided with --select.
   -- If in normal mode, nothing is filtered by default, and filtered items
   -- should be provided with --exclude.

   -- --------------------------------------------------------------------------
   type Step_Data is record
      Cat              : Extended_Step_Categories  := Unknown;
      Action           : Actions                   := None;
      Negated          : Boolean                   := False;
      -- Set when the step wording carries a negation ("no", "not",
      -- "does not"...). Only meaningful for negatable actions (cf.
      -- BBT.Scenarios.Steps); the check procedures invert their
      -- comparison result when it is set.
      Src_Code         : Unbounded_String          := Null_Unbounded_String;
      Subject_String   : Unbounded_String          := Null_Unbounded_String;
      Object_String    : Unbounded_String          := Null_Unbounded_String;
      Object_File_Name : Unbounded_String          := Null_Unbounded_String;
      File_Type        : Ada.Directories.File_Kind := Ada.Directories.Ordinary_File;

      Commands         : Cmd_List                  := Empty_Cmd_List;
      Code_Span_First  : Natural                   := 0;
      Code_Span_Last   : Natural                   := 0;
      -- Code_Span_First and Code_Span_Last point in Src_Code to the "or"
      -- part that should be replaced when duplicating code.
      -- "- When I run `cmd1` or `cmd2` or `cmd3` whatever comment"
      --                ^ Code_Span_First      ^ Code_Span_Last
      -- That will ultimately be displayed as
      -- "- When I run `cmd1`  whatever comment"
      -- "- When I run `cmd2`  whatever comment"
      -- "- When I run `cmd3`  whatever comment"

      Executable_File  : Boolean                   := False;
      Ignore_Order     : Boolean                   := True;
      Ignore_Patterns  : Ignore_Pattern_Set        := No_Ignore_Pattern;
      Ignore_Regexps   : Ignore_Regexp_List        := No_Ignore_Regexp;
      -- Patterns of the (ignoring ...) clause, masking the volatile
      -- parts on both sides before the comparison: the named patterns
      -- in Ignore_Patterns, the custom regexps in Ignore_Regexps
      -- (cf. docs/features/A300_Ignoring.md)
      File_Content     : Text                      := Empty_Text;
      Syntax_Error     : Boolean                   := False;
   end record with Put_Image => Put_Image;

   -- --------------------------------------------------------------------------
   type Step_Type is new Non_Root_Node with record
      Data : Step_Data;
   end record with Put_Image => Put_Image;

   -- --------------------------------------------------------------------------
   procedure Set_Parent  (Step            : in out Step_Type;
                          Parent_Scenario :        Scenarios.Scenario_Access);

   function Create_Step (Info            : Step_Data;
                         Loc             : Location_Type;
                         Parent_Scenario : access Scenarios.Scenario_Type)
                         return Step_Type;

   procedure Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      S      :        Step_Type);
   procedure Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      S      :        Step_Data);
   -- Put_Image returns the full image

   function Explain
     (Step : Step_Type) return String;
   -- returns an explanation of the step in natural language

   function Get_Expected (Step : Step_Type'Class) return Text;
   -- returns the expected content of the step: the content provided in
   -- code fenced lines, or the content of the file named by the object,
   -- or the object string itself, or an empty text

   overriding procedure Apply_Filters_To
     (S : in out Step_Type);

  function Parent
    (S : Step_Type) return Scenarios.Scenario_Access
    with Pre => S.Parent /= null;
  -- A Step cannot be created outside of an existing scenario,
  -- this access should never be null.

   -- --------------------------------------------------------------------------
   procedure Set_Filter (S        : in out Step_Type'Class;
                         Filtered :        Boolean);

   -- --------------------------------------------------------------------------
   procedure Set_Has_Syntax_Error (S         : in out Step_Type'Class;
                                   Has_Error :        Boolean);
   function Has_Syntax_Error (S : in out Step_Type'Class) return Boolean;

   -- --------------------------------------------------------------------------
   package Step_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Step_Type'Class);
   subtype List is Step_Lists.Vector;
   Empty_Step_List : List renames Step_Lists.Empty_Vector;

   -- --------------------------------------------------------------------------
   function Last_Step
     (S : in out Step_Lists.Vector) return Step_Maybe;

end BBT.Model.Steps;
