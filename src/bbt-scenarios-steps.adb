-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Model.Steps,
     BBT.Scenarios.Steps.Lexer,
     BBT.Settings;

with Text_Utilities;

with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Strings.Maps;

with GNAT.Regpat;

use BBT.Scenarios.Steps.Lexer,
    BBT.Model.Steps,
    Text_Utilities,
    Ada.Directories,
    Ada.Strings.Fixed,
    Ada.Strings.Unbounded;

package body BBT.Scenarios.Steps is

   -- Token types for step parsing
   type Tokens is ( -- Prepositions -----------------------------------------
                    Given,
                    When_P,
                    Then_P,
                    -- Subjects Attribute -----------------------------------
                    No_SA, -- = no Subject Attribute
                    Executable,
                    New_SA,
                    -- Subjects ---------------------------------------------
                    No_Subject,
                    Dir_Subj,  -- dir name
                    File_Subj, -- file name
                    Text_Subj, -- content of code span or following
                    --               code fenced lines, before verb
                    Output_Subj,
                    Stderr_Subj, -- "the error output"
                    Exit_Subj,    -- "the exit code"
                    Env_Var_Subj,      -- "the environment variable `NAME`"
                    Window_Subj,       -- "the window"
                    -- Verbs ------------------------------------------------
                    --  Negated forms ("is no", "get no", "does not
                    --  contain"...) are not distinct verbs: the negation
                    --  words set State.Negated, and the resulting polarity
                    --  is carried by Step_Data.Negated.
                    --  The same for the run verb qualifiers: "shell" and
                    --  "successfully" set State.Shell_Met and
                    --  State.Successfully_Met, and the run action is
                    --  derived from them when the step is resolved.
                    No_Verb,
                    Run,
                    Get,
                    Contains,
                    Matches,
                    Containing,
                    Is_V,
                    Fail,
                    Type_Text,
                    Enter_Text,
                    Appears,
                    Press,
                    -- Objects ----------------------------------------------
                    No_Object,
                    Obj_Output,
                    Obj_Stderr, -- "there is no error output"
                    Obj_File_Name,
                    Obj_Dir_Name, -- file or dir name
                    Obj_Text, -- content of code span on the same line or
                    --           code fenced block expected on following lines,
                    --           after the verb.
                    Obj_Command_List,
                    Obj_Error,
                    Obj_Window_Name,
                    -- a window name, as in "a `title` window appears"
                    Obj_Widget_Name,
                    -- a widget accessible name, as in
                    -- "there is a button named `Continue`"
                    Obj_Widget_State,
                    -- the "enabled/disabled" of a widget state step
                    -- Adjectives
                    Unordered);

   -- Token subtypes
   subtype Adjectives     is Tokens range Unordered    .. Tokens'Last;
   subtype Objects        is Tokens range No_Object    .. Tokens'Pred (Adjectives'First);
   subtype Verbs          is Tokens range No_Verb      .. Tokens'Pred (Objects'First);
   subtype Subjects       is Tokens range No_Subject   .. Tokens'Pred (Verbs'First);
   subtype Subject_Attrib is Tokens range No_SA        .. Tokens'Pred (Subjects'First);
   subtype Prepositions   is Tokens range Tokens'First .. Tokens'Pred (Subject_Attrib'First);

   -- Convert token to string representation
   function Image (T : Tokens) return String is separate;

   type Example_String is access String;

   -- Grammar item definition
   type Grammar_Items is record
      Action              : Actions := None;
      Code_Block_Expected : Boolean := False;
      -- set when a code fenced block is expected after the step line
      Example             : Example_String := new String'("");
   end record;

   --  Rule sets: CLI and GUI rules share the same registry and the
   --  same parse engine, but are stated in separate initializers, so
   --  that a vocabulary extension never perturbs CLI parsing.
   type Rule_Sets is (CLI_Set, GUI_Set);

   --  Grammar rule key: the step signature
   type Grammar_Key is record
      Prep : Prepositions;
      SA   : Subject_Attrib;
      Sub  : Subjects;
      Verb : Verbs;
      Obj  : Objects;
   end record;

   --  One registered grammar rule: a key mapped to the item it resolves to
   type Grammar_Rule is record
      Set  : Rule_Sets;
      Key  : Grammar_Key;
      Item : Grammar_Items;
   end record;

   package Rule_Registries is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Grammar_Rule);

   --  Check and setup actions that support a negated form: the negation
   --  words ("no", "not", "does not"...) don't select a distinct verb or
   --  action, they set the step polarity (Step_Data.Negated), and the
   --  action procedure inverts its comparison result. Any other action
   --  triggers a warning when the step line contains a negation word.
   function Negatable (A : Actions) return Boolean is
     (case A is
      when Check_File_Existence | Check_Dir_Existence | Set_Env_Var |
           Error_Return_Code     | Exit_Code_Is |
           Output_Is             | Output_Not_Empty |
           Output_Contains       | Output_Matches |
           File_Is               | File_Contains | File_Matches |
           Stderr_Is             | Stderr_Contains | Stderr_Not_Empty |
           GUI_Window_Appears    | GUI_Window_Contains |
           GUI_Widget_Exists     | GUI_Widget_State
        => True,
      when others => False);

   --  The sparse grammar registry
   The_Rules : Rule_Registries.Vector;

   --  Return the grammar item registered for the given step signature,
   --  or a default "no action" item when no rule matches.
   function Lookup (Prep : Prepositions;
                    SA   : Subject_Attrib;
                    Sub  : Subjects;
                    Verb : Verbs;
                    Obj  : Objects)
                    return Grammar_Items is
   begin
      for R of The_Rules loop
         if    R.Key.Prep = Prep
           and then R.Key.SA   = SA
           and then R.Key.Sub  = Sub
           and then R.Key.Verb = Verb
           and then R.Key.Obj  = Obj
         then
            return R.Item;
         end if;
      end loop;

      return (Action => None,
              Code_Block_Expected => False,
              Example => new String'(""));
   end Lookup;

   type Preposition_Lists is array (Positive range <>) of Prepositions;

   procedure Set_One (Prep       : Prepositions;
                      Sub_Attrib : Subject_Attrib;
                      Sub        : Subjects;
                      Verb       : Verbs;
                      Object     : Objects;
                      Action     : Actions;
                      Code_Block : Boolean;
                      Example    : String) is
      --  State one grammar rule. The example string is stored prefixed
      --  with the preposition, so that a rule shared by several
      --  prepositions is stated only once.
      Grammar_Error : exception;
   begin
      for R of The_Rules loop
         if    R.Key.Prep = Prep
           and then R.Key.SA   = Sub_Attrib
           and then R.Key.Sub  = Sub
           and then R.Key.Verb = Verb
           and then R.Key.Obj  = Object
         then
            raise Grammar_Error with "Grammar already initialized for " &
              Prep'Image & ", " & Sub_Attrib'Image & ", " & Sub'Image & ", " &
              Verb'Image & ", " & Object'Image;
         end if;
      end loop;
      The_Rules.Append
        (Grammar_Rule'(Set  => CLI_Set,
                       Key  => Grammar_Key'(Prep, Sub_Attrib, Sub, Verb,
                                            Object),
                       Item => (Action,
                                Code_Block,
                                new String'("- " & Image (Prep) & " "
                                            & Example))));
   end Set_One;

   procedure Set (Prep       : Prepositions;
                  Sub_Attrib : Subject_Attrib;
                  Sub        : Subjects;
                  Verb       : Verbs;
                  Object     : Objects;
                  Action     : Actions;
                  Example    : String;
                  Code_Block : Boolean := False) is
   begin
      Set_One (Prep, Sub_Attrib, Sub, Verb, Object, Action, Code_Block, Example);
   end Set;

   procedure Set (Preps      : Preposition_Lists;
                  Sub_Attrib : Subject_Attrib;
                  Sub        : Subjects;
                  Verb       : Verbs;
                  Object     : Objects;
                  Action     : Actions;
                  Example    : String;
                  Code_Block : Boolean := False) is
   begin
      for Prep of Preps loop
         Set_One (Prep, Sub_Attrib, Sub, Verb, Object, Action, Code_Block, Example);
      end loop;
   end Set;


   -- Create the complete BBT grammar
   procedure Initialize_Grammar is separate;

   --  Register the GUI rule set (rules and keywords), after the CLI
   --  ones. Empty as long as the GUI vocabulary is not implemented,
   --  cf. [gui_vocabulary](docs/proposed/gui_vocabulary.md).
   procedure Initialize_GUI_Grammar is separate;

   -- Type to hold parsing state for separate procedures
   type Parse_State is record
      Successfully_Met : Boolean                  := False;
      -- Set by the "successfully" keyword, whatever its position in
      -- the step line: it is a run verb qualifier, not a distinct
      -- verb (cf. Shell_Met below and the negation words).

      Shell_Met        : Boolean                  := False;
      -- Set by the "shell" keyword: a run verb qualifier, combined
      -- with Successfully_Met to derive the run action when the
      -- step is resolved (cf. Parse)

      Or_Met           : Natural                  := 0;
      Cmd_List         : Model.Steps.Cmd_List     := Model.Steps.Empty_Cmd_List;
      Cmd_Expected     : Boolean                  := False;
      Code_Span_First  : Natural                  := 0; -- = 0 until first Code_Span is meet
      Code_Span_Last   : Natural                  := 0; -- = 0 until first Code_Span is meet

      Negated          : Boolean                  := False;
      -- Set by any negation word ("no", "not", "does not"...) met in the
      -- step line, whatever its position in the sentence
      Prep             : Prepositions             := Prepositions'First;
      Subject_Attr     : Subject_Attrib           := No_SA;
      Subject          : Subjects                 := No_Subject;
      Object           : Objects                  := No_Object;
      File_Type        : File_Kind                := Ordinary_File;
      Cat              : Extended_Step_Categories := Unknown;
      Action           : Actions                  := None;
      Subject_String   : Unbounded_String         := Null_Unbounded_String;
      Object_String    : Unbounded_String         := Null_Unbounded_String;
      Object_File_Name : Unbounded_String         := Null_Unbounded_String;
      Ignore_Order     : Boolean                  := False;
      Executable       : Boolean                  := False;
      Ignore_Patterns  : Ignore_Pattern_Set       := No_Ignore_Pattern;
      Ignore_Regexps   : Ignore_Regexp_List       := No_Ignore_Regexp;
      Order_Clause     : Boolean                  := False;
      -- Set when the (ignoring order) pattern is in the clause, as
      -- opposed to the deprecated unordered keyword, that also sets
      -- Ignore_Order during the lexing
      Ignoring_Clause  : Boolean                  := False;
      -- Set when the step line ends with an (ignoring ...) clause,
      -- whatever its content: the clause is only legal on comparison
      -- steps

      GUI_Role         : Unbounded_String         := Null_Unbounded_String;
      GUI_Access_Name  : Unbounded_String         := Null_Unbounded_String;
      GUI_Widget_State : Model.Steps.GUI_Widget_States := Model.Steps.GUI_Any;
      -- The GUI payload, only meaningful on the GUI actions (cf.
      -- docs/proposed/gui_vocabulary.md)
   end record;

   -- --------------------------------------------------------------------------
   -- Chunk package for parsing state management
   package Chunk is
      procedure Initialize;
      procedure Set_Verb (V : Verbs; Loc : Location_Type := No_Location);
      function Verb return Verbs;
      function In_Subject_Part return Boolean;
      function In_Object_Part  return Boolean;
   end Chunk;
   package body Chunk is separate;

   -- -----------------------------------------------------------------------
   procedure Put_Rule (P           : Prepositions;
                       SA          : Subject_Attrib;
                       S           : Subjects;
                       V           : Verbs;
                       O           : Objects;
                       A           : Grammar_Items;
                       Verbosity   : IO.Verbosity_Levels := IO.Normal)
   is
      use Ada.Text_IO;
      C : Positive_Count := Col;
   begin
      if Is_Authorized (Verbosity) then
         Ada.Text_IO.Put  ("| " & Image (P));
         C := @ + Prepositions'Width + 1; Set_Col (C);

         Ada.Text_IO.Put (" | " & Image (SA));
         C := @ + Subject_Attrib'Width; Set_Col (C);

         Ada.Text_IO.Put (" | " & Image (S));
         C := @ + Subjects'Width - 3; Set_Col (C);

         Ada.Text_IO.Put (" | " & Image (V));
         C := @ + Verbs'Width + 3; Set_Col (C);

         Ada.Text_IO.Put (" | " & Image (O));
         C := @ + 20; Set_Col (C);

         Ada.Text_IO.Put (" | " & A.Action'Image);
         C := @ + Actions'Width + 3; Set_Col (C);

         Ada.Text_IO.Put (" | " &
                          (if A.Code_Block_Expected then "    X" else ""));
         C := @ + 13; Set_Col (C);

         Ada.Text_IO.Put_Line (" | " & A.Example.all & " |  ");
      end if;
   end Put_Rule;

   Previous_Step_Kind : Extended_Step_Categories := Unknown;
   -- Step starting with "And" or "But" have the kind of the last "Given" /
   -- "When" or "then" encountered, this variable keep this memory
   -- between calls to Parse.

   -- --------------------------------------------------------------------------
   procedure Put_Grammar is
   begin
      Ada.Text_IO.Put_Line ("| Prep  |         |Subject |       Verb       | Object |         Action          | Code block expected | Use Example |  ");
      Ada.Text_IO.Put_Line ("|-------|---------|--------|------------------|--------|-------------------------|---------------------|-------------|  ");
      for R of The_Rules loop
         if R.Item.Action /= None then
            Put_Rule (R.Key.Prep, R.Key.SA, R.Key.Sub, R.Key.Verb, R.Key.Obj,
                      R.Item);
         end if;
      end loop;
   end Put_Grammar;

   -- --------------------------------------------------------------------------
   procedure Put_Keywords is
   begin
      Lexer.Put_Keywords;
   end Put_Keywords;

   -- --------------------------------------------------------------------------
   -- Process a keyword token during parsing
   procedure Process_Keyword (Tok      : String;
                              Loc      : Location_Type;
                              State    : in out Parse_State) is separate;

   -- --------------------------------------------------------------------------
   -- Process a code span token during parsing
   procedure Process_Code_Span (Tok             : String;
                                Loc             : Location_Type;
                                In_Subject_Part : Boolean;
                                In_Object_Part  : Boolean;
                                Current_Verb    : Verbs;
                                State           : in out Parse_State) is separate;

   -- --------------------------------------------------------------------------
   -- Extract the (ignoring ...) clause at the end of the step line, if
   -- any, set the corresponding Patterns, and return the line truncated
   -- before the clause, so that the clause is not seen by the lexer
   function Extract_Ignoring_Clause (Line     : String;
                                     Loc      : Location_Type;
                                     Patterns : out Ignore_Pattern_Set;
                                     Regexps  : out Ignore_Regexp_List;
                                     Order    : out Boolean)
                                     return String is separate;

   -- --------------------------------------------------------------------------
   -- Validate the semantic consistency of the parsed step state
   procedure Validate_Step_State (State               : Parse_State;
                                  Loc                 : Location_Type;
                                  Verb                : Verbs;
                                  Code_Block_Expected : out Boolean) is separate;

   -- --------------------------------------------------------------------------
   function Parse (Line                :        Unbounded_String;
                   Loc                 : in out IO.Location_Type;
                   Code_Block_Expected :    out Boolean)
                   return Model.Steps.Step_Data
   is
      State    : Parse_State;
      Src_Code : Unbounded_String := Null_Unbounded_String;

      Tmp : aliased String :=
        Extract_Ignoring_Clause (To_String (Line), Loc,
                                  State.Ignore_Patterns, State.Ignore_Regexps,
                                  State.Order_Clause);
      TT  : Token_Type;

   begin
      Code_Block_Expected := False;
      Src_Code := Line;

      -- A clause was found when the step line was truncated before it
      if State.Order_Clause then
         State.Ignore_Order := True;
      end if;
      State.Ignoring_Clause := Tmp'Length < Length (Line);

      Initialize_Lexer;

      -- First token processing
      declare
         Tok           : constant String := Next_Token (Tmp'Access, TT);
         Lower_Keyword : constant String := Translate
           (Source  => Tok,
            Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);

      begin
         if    Lower_Keyword = "given" then
            State.Cat  := Given_Step;
            State.Prep := Given;

         elsif Lower_Keyword = "when" then
            State.Cat  := When_Step;
            State.Prep := When_P;

         elsif Lower_Keyword = "then" then
            State.Cat  := Then_Step;
            State.Prep := Then_P;

         elsif Lower_Keyword = "and" or else Lower_Keyword = "but" then
            State.Cat := Previous_Step_Kind;
            case Previous_Step_Kind is
               when Given_Step => State.Prep := Given;
               when When_Step  => State.Prep := When_P;
               when Then_Step  => State.Prep := Then_P;
               when Unknown    =>
                  IO.Put_Error ("No previous step to determine step kind of "
                                & Line'Image, Loc);
                  -- A scenario cannot start with an "and" or "but" step.
            end case;

         else
            IO.Put_Error ("line starting with '- " & Tok
                          & "' is not a step, ignored", Loc);
         end if;
      end;

      if No_Error then

         Chunk.Initialize;

         Line_Processing : while More_Token loop

            declare
               First : Natural;
               Last  : Natural;
               Tok : constant String := Next_Token (Tmp'Access, First, Last, TT);

            begin
               Put_Debug_Line ("   In Parse, " & TT'Image & " " & Tok, Loc);
               case TT is
               when Keyword =>
                  Process_Keyword (Tok, Loc, State);

               when Identifier =>
                  null;

               when Code_Span =>
                  if State.Code_Span_First = 0 then
                     State.Code_Span_First := First;
                  end if;
                  State.Code_Span_Last := Last;
                  -- Put_Debug_Line ("   In Parse, Code Span from " & State.Code_Span_First'Image & " to" & Last'Image, Loc);
                  -- Put_Debug_Line ("   In Parse, Line    = >" & Tmp & "<", Loc);
                  -- Put_Debug_Line ("   In Parse, Or zone = >" & Tmp (State.Code_Span_First .. Last) & "<", Loc);

                  Process_Code_Span (Tok,
                                     Loc,
                                     Chunk.In_Subject_Part,
                                     Chunk.In_Object_Part,
                                     Chunk.Verb,
                                     State);

               when Empty =>
                  null;

               end case;
            end;

         end loop Line_Processing;

         State.Action := Lookup (State.Prep,
                                 State.Subject_Attr,
                                 State.Subject,
                                 Chunk.Verb,
                                 State.Object).Action;

         --  The run verb qualifiers ("shell", "successfully") are not
         --  grammar entries: like the negation words, they set flags,
         --  and the resulting action is derived here (cf. Step_Data.Negated).
         if State.Action = Run_Cmd then
            if State.Shell_Met then
               State.Action := (if State.Successfully_Met
                                then Run_Shell_Without_Error
                                else Run_Shell_Cmd);
            elsif State.Successfully_Met then
               State.Action := Run_Without_Error;
            end if;

         else
            if State.Successfully_Met then
               IO.Put_Warning ("""successfully"" ignored: this step form"
                               & " has no ""successfully"" variant", Loc);
            end if;
            if State.Shell_Met then
               IO.Put_Warning ("""shell"" ignored: this step form"
                               & " has no ""shell"" variant", Loc);
            end if;
         end if;

         Put_Debug_Line ("---- Prep = " & State.Prep'Image
                         & ", Subject = " & State.Subject'Image
                         & ", Attr = " & State.Subject_Attr'Image
                         & ", Verb = " & Chunk.Verb'Image
                         & ", Object = " & State.Object'Image, Loc);
         Put_Debug_Line ("---- Action set to " & State.Action'Image, Loc);

         if State.Action = None then
            IO.Put_Error ("Unrecognized step """ & To_String (Line) & """", Loc);

         else
            if State.Negated then
               if Negatable (State.Action) then
                  --  The negation is carried to the check procedure as
                  --  the step polarity
                  null;
               else
                  --  The negation word may be part of the free, non
                  --  interpreted, part of the step line: warn, don't
                  --  reject, and parse the step as positive.
                  IO.Put_Warning
                    ("""no""/""not"" ignored: this step form has no"
                     & " negative variant", Loc);
                  State.Negated := False;
               end if;
            end if;

            Validate_Step_State (State,
                                 Loc,
                                 Chunk.Verb,
                                 Code_Block_Expected);
         end if;
         Previous_Step_Kind := State.Cat;

      end if;

      if State.Action in GUI_Window_Appears | GUI_Window_Contains |
                          GUI_Widget_Exists | GUI_Widget_State | GUI_Press_Widget
      then
         --  The widget or window name: the object's code span, or, for
         --  the forms where it shows up in the subject phrase ("the
         --  `name` `role` is enabled", "a `title` window appears"),
         --  the subject's
         if State.Object_String /= Null_Unbounded_String then
            State.GUI_Access_Name := State.Object_String;
         else
            State.GUI_Access_Name := State.Subject_String;
         end if;
         State.Object_String   := Null_Unbounded_String;
         State.Subject_String  := Null_Unbounded_String;
      end if;

      return (Cat              => State.Cat,
              Action           => State.Action,
              Negated          => State.Negated,
              Src_Code         => Src_Code,
              Subject_String   => State.Subject_String,
              Object_String    => State.Object_String,
              Object_File_Name => State.Object_File_Name,
              File_Type        => State.File_Type,
              Commands         => State.Cmd_List,
              Code_Span_First  => State.Code_Span_First,
              Code_Span_Last   => State.Code_Span_Last,
              Executable_File  => State.Executable,
              Ignore_Order     => State.Ignore_Order,
              Ignore_Patterns  => State.Ignore_Patterns,
              Ignore_Regexps   => State.Ignore_Regexps,
              File_Content     => Empty_Text,
              Syntax_Error     => IO.Some_Error,
              GUI_Role         => State.GUI_Role,
              GUI_Access_Name  => State.GUI_Access_Name,
              GUI_Widget_State => State.GUI_Widget_State);
   end Parse;

begin
   Initialize_Grammar;
   Initialize_GUI_Grammar;

end BBT.Scenarios.Steps;
