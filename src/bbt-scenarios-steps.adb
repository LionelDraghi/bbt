-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Model.Steps,
     BBT.Scenarios.Steps.Lexer;

with Text_Utilities;

-- with Ada.Containers;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Strings.Maps;


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
                    Dir_Subject,  -- dir name
                    Subject_File, -- file name
                    Subject_Text, -- content of code span or following
                    --               code fenced lines, before verb
                    Output_Subj,
                    -- Verbs ------------------------------------------------
                    No_Verb,
                    Run,
                    Successful_Run,
                    Get,
                    Get_No,
                    Does_Not_Contain,
                    Contains,
                    Matches,
                    Does_Not_Match,
                    Containing,
                    Is_V,
                    Is_No,
                    -- Objects ----------------------------------------------
                    No_Object,
                    Output_Obj,
                    Obj_File_Name,
                    Obj_Dir_Name, -- file or dir name
                    Obj_Text, -- content of code span on the same line or
                    --           code fenced block expected on following lines,
                    --           after the verb.
                    Command_List,
                    Error,
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

   -- Grammar item definition
   type Grammar_Items is record
      Action              : Actions := None;
      Code_Block_Expected : Boolean := False;
      -- set when a code fenced block is expected after the step line
      Example             : access String;
   end record;

   -- Full grammar definition, with default "no action" values
   type Grammar is array (Prepositions, Subject_Attrib,
                          Subjects, Verbs, Objects) of Grammar_Items;

   -- Create the complete BBT grammar
   procedure Initialize_Grammar (G : in out Grammar) is separate;

   The_Grammar : Grammar;

   -- Type to hold parsing state for separate procedures
   type Parse_State is record
      Successfully_Met : Boolean        := False;
      Or_Met           : Natural        := 0;
      Not_Met          : Boolean        := False;
      Prep             : Prepositions   := Prepositions'First;
      Subject_Attr     : Subject_Attrib := No_SA;
      Subject          : Subjects       := No_Subject;
      Object           : Objects        := No_Object;
      File_Type        : File_Kind      := Ordinary_File;
      Cat              : Extended_Step_Categories := Unknown;
      Action           : Actions                  := None;
      Subject_String   : Unbounded_String         := Null_Unbounded_String;
      Object_String    : Unbounded_String         := Null_Unbounded_String;
      Object_File_Name : Unbounded_String         := Null_Unbounded_String;
      Ignore_Order     : Boolean                  := False;
      Executable       : Boolean                  := False;
   end record;

   -- --------------------------------------------------------------------------
   -- Chunk package for parsing state management
   package Chunk is
      procedure Initialize;
      procedure Set_Verb (V : Verbs; Loc : Location_Type := No_Location);
      procedure Set_Loc (L : Location_Type);
      function Verb return Verbs;
      function In_Subject_Part return Boolean;
      function In_Object_Part  return Boolean;
   end Chunk;
   package body Chunk is separate;

   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := IO.Steps)
                             renames BBT.IO.Put_Line;
   pragma Warnings (Off, Put_Debug_Line);

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
         C := @ + 9; Set_Col (C);

         Ada.Text_IO.Put (" | " & A.Action'Image);
         C := @ + Actions'Width + 3; Set_Col (C);

         Ada.Text_IO.Put (" | " &
                          (if A.Code_Block_Expected then "    X" else ""));
         C := @ + 13; Set_Col (C);

         Ada.Text_IO.Put_Line (" | " & A.Example);

         Ada.Text_IO.Put_Line (" |  ");
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
      for P in The_Grammar'Range (1) loop -- A of G when A /= None loop
         for SA in The_Grammar'Range (2) loop -- A of G when A /= None loop
            for S in The_Grammar'Range (3) loop -- A of G when A /= None loop
               for V in The_Grammar'Range (4) loop -- A of G when A /= None loop
                  for O in The_Grammar'Range (5) loop -- A of G when A /= None loop
                     if The_Grammar (P, SA, S, V, O).Action /= None then
                        Put_Rule (P, SA, S, V, O,
                                  The_Grammar (P, SA, S, V, O));
                     end if;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
      -- Ada.Text_IO.Put_Line ("|-------|-----|--------|------------------|-------------------|----------------------|");
   end Put_Grammar;

   -- --------------------------------------------------------------------------
   procedure Put_Keywords is
   begin
      Lexer.Put_Keywords;
   end Put_Keywords;

   -- --------------------------------------------------------------------------
   -- Process a keyword token during parsing
   procedure Process_Keyword (Tok      : String;
                              State    : in out Parse_State;
                              Loc      : Location_Type;
                              Cmd_List : in out Model.Steps.Cmd_List) is separate;

   -- --------------------------------------------------------------------------
   -- Process a code span token during parsing
   procedure Process_Code_Span (Tok            : String;
                               State           : in out Parse_State;
                               Loc             : Location_Type;
                               Cmd_List        : in out Model.Steps.Cmd_List;
                               In_Subject_Part : Boolean;
                               In_Object_Part  : Boolean;
                               Current_Verb    : Verbs) is separate;

   -- --------------------------------------------------------------------------
   -- Validate the semantic consistency of the parsed step state
   procedure Validate_Step_State (State               : Parse_State;
                                  Loc                 : Location_Type;
                                  Cmd_List            : Model.Steps.Cmd_List;
                                  Verb                : Verbs;
                                  No_Subject_String   : Boolean;
                                  No_Object_File_Name : Boolean;
                                  Code_Block_Expected : out Boolean) is separate;

   -- --------------------------------------------------------------------------
   function Parse (Line                :        Unbounded_String;
                   Loc                 : in out Location_Type;
                   Code_Block_Expected :    out Boolean;
                   Cmd_List            :    out Model.Steps.Cmd_List)
                   return Model.Steps.Step_Data
   is
      State : Parse_State;
      Src_Code         : Unbounded_String         := Null_Unbounded_String;

      function Is_Null (S : in Unbounded_String) return Boolean is
        (S = Null_Unbounded_String);
      function No_Subject_String return Boolean is
        (Is_Null (State.Subject_String));
      function No_Object_File_Name return Boolean is
        (Is_Null (State.Object_File_Name));

      Tmp : aliased constant String := To_String (Line);
      TT  : Token_Type;

   begin
      Src_Code := Line;
      Cmd_List := Cmd_Lists.Empty_Vector;

      Put_Debug_Line (To_String (Line), Loc);

      Initialize_Lexer;
      -- Chunk.Initialize will be added later

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

         elsif Lower_Keyword = "and" or else
           Lower_Keyword = "but"
         then
            State.Cat := Previous_Step_Kind;
            -- Inherited from the context
            case Previous_Step_Kind is
               when Unknown    => null;
               when Given_Step => State.Prep := Given;
               when When_Step  => State.Prep := When_P;
               when Then_Step  => State.Prep := Then_P;
            end case;

         else
            IO.Put_Error ("line starting with '- " & Tok
                          & "' is not a step, ignored", Loc);
         end if;
      end;

      if No_Error then

         Chunk.Initialize;
         Chunk.Set_Loc (Loc);

         Line_Processing : while More_Token loop

            declare
               Tok : constant String := Next_Token (Tmp'Access, TT);

            begin
               case TT is
               when Keyword =>
                  Process_Keyword (Tok, State, Loc, Cmd_List);

               when Identifier =>
                  null;

               when Code_Span =>
                  Process_Code_Span (Tok, State, Loc, Cmd_List,
                                   Chunk.In_Subject_Part,
                                   Chunk.In_Object_Part,
                                   Chunk.Verb);

               when Empty =>
                  null;

               end case;
            end;

         end loop Line_Processing;

         -- State.Action and Code_Block_Expected will be set later when Verb is available
         State.Action := The_Grammar
           (State.Prep, State.Subject_Attr, State.Subject, Chunk.Verb, State.Object).Action;

         declare
            Code_Block_Expected_Local : Boolean;
         begin
            Validate_Step_State (State, Loc, Cmd_List, Chunk.Verb,
                               No_Subject_String, No_Object_File_Name,
                               Code_Block_Expected_Local);
            Code_Block_Expected := Code_Block_Expected_Local;
         end;

         Previous_Step_Kind := State.Cat;

      end if;

      return (Cat              => State.Cat,
              Action           => State.Action,
              Src_Code         => Src_Code,
              Subject_String   => State.Subject_String,
              Object_String    => State.Object_String,
              Object_File_Name => State.Object_File_Name,
              File_Type        => State.File_Type,
              Executable_File  => State.Executable,
              Ignore_Order     => State.Ignore_Order,
              File_Content     => Empty_Text,
              Syntax_Error     => IO.Some_Error);
   end Parse;

begin
   Initialize_Grammar (The_Grammar);

end BBT.Scenarios.Steps;
