-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

separate (BBT.Scenarios.Steps)


procedure Process_Keyword (Tok      : String;
                           Loc      : Location_Type;
                           State    : in out Parse_State) is
   use Ada.Strings.Maps.Constants;
   use Chunk;

   Lower_Keyword : constant String := Translate (Source  => Tok,
                                                 Mapping => Lower_Case_Map);

begin
   Put_Debug_Line ("  In Process_Keyword, Keyword = " & Lower_Keyword, Loc);
   if Lower_Keyword = "executable" then
      State.Executable := True;

   elsif Lower_Keyword = "run" or Lower_Keyword = "running" then
      Set_Verb (Run, Loc);

   elsif Lower_Keyword = "shell" then
      -- When I run shell `cmd1 | cmd2`
      -- "shell" is a run verb qualifier, not a distinct verb: like the
      -- negation words, it sets a flag, and the run action is derived
      -- from the qualifiers when the step is resolved (cf. Parse).
      -- The preposition that could precede "shell" is decoration:
      -- the keywords are "run" and "shell"
      State.Shell_Met := True;

   elsif Lower_Keyword = "type" then
      -- When I type `Y`
      Set_Verb (Type_Text, Loc);

   elsif Lower_Keyword = "enter" then
      -- When I enter `Y`
      Set_Verb (Enter_Text, Loc);

   elsif Lower_Keyword = "or" then
      if State.Cmd_Expected then
      -- If there is two consecutive, let's warn the user.
         Put_Warning ("Command missing between 'or' ?", Loc);

      elsif State.Object_String = Null_Unbounded_String then
         null;
      -- If there is no Code Span when the first or is met, let's consider
      -- it's not an error, but just or that is used in a comment.

      else
         State.Or_Met := @ + 1;
         State.Cmd_Expected := True;
         if State.Object /= Obj_Command_List then
            State.Object := Obj_Command_List;
            -- There should already be a code span, let's move it in the list
            State.Cmd_List.Append (To_String (State.Object_String));
            State.Object_String := Null_Unbounded_String;
         end if;

      end if;

   elsif Lower_Keyword = "get" then
      Set_Verb (Get, Loc);

   elsif Lower_Keyword = "is" then
      Set_Verb (Is_V, Loc);

   elsif Lower_Keyword = "no"
     or Lower_Keyword = "not"
     or Lower_Keyword = "dont"
     or Lower_Keyword = "doesnt"
     or Lower_Keyword = "doesn't"
   then
      --  The negation is not attached to a specific verb: it sets the
      --  step polarity, checked once the action is resolved (cf. Parse)
      State.Negated := True;

   elsif Lower_Keyword = "fail"
     or Lower_Keyword = "fails"
   then
      Set_Verb (Fail, Loc);
      State.Subject := Text_Subj;

   elsif Lower_Keyword = "successfully" then
      State.Successfully_Met := True;

   elsif Lower_Keyword = "error" then
      State.Object := Obj_Error;

   elsif Lower_Keyword = "output" then
      if State.Object = Obj_Error then
         -- "error output": the standard error of the last command
         --    Then the error output contains `unknown option`
         --    Then there is no error output
         if In_Subject_Part then
            State.Subject := Stderr_Subj;
            State.Object  := No_Object;
         else
            State.Object := Obj_Stderr;
         end if;
      elsif In_Subject_Part then
         State.Subject := Output_Subj;
      else
         State.Object := Obj_Output;
      end if;

   elsif Lower_Keyword = "exit" then
      -- Then the exit code is `2`
      if In_Subject_Part then
         State.Subject := Exit_Subj;
      end if;

   elsif Lower_Keyword = "environment" then
      -- Given the environment variable `NAME` is `value`
      if In_Subject_Part then
         State.Subject := Env_Var_Subj;
      end if;

   elsif Lower_Keyword = "contains" or
     Lower_Keyword = "contain"
   then
      Set_Verb (Contains, Loc);

   elsif Lower_Keyword = "match" or
     Lower_Keyword = "matches"
   then
      Set_Verb (Matches, Loc);

   elsif Lower_Keyword = "containing" then
      Set_Verb (Containing, Loc);

   elsif Lower_Keyword = "new" then
      State.Subject_Attr := New_SA;

   elsif Lower_Keyword = "directory" or Lower_Keyword = "dir"
   then
      State.File_Type := Directory;

      if In_Subject_Part then
         State.Subject := Dir_Subj;

      elsif In_Object_Part then
         State.Object := Obj_Dir_Name;

         State.Object_File_Name := State.Object_String;
         State.Object_String    := Null_Unbounded_String;
      end if;

   elsif Lower_Keyword = "file" then
      State.File_Type := Ordinary_File;

      if In_Subject_Part then
         State.Subject := File_Subj;

      elsif In_Object_Part then
         State.Object := Obj_File_Name;
         -- Then it was considered an Object_String, let's update that:
         State.Object_File_Name := State.Object_String;
         State.Object_String    := Null_Unbounded_String;
      end if;

   elsif Lower_Keyword = "unordered" then
      State.Ignore_Order := True;

   elsif Lower_Keyword = "window" then
      -- "Then a window appears", "a `title` window appears",
      -- "Then the window contains `text`"
      if In_Subject_Part then
         State.Subject := Window_Subj;
      else
         State.Object  := Obj_Window_Name;
      end if;

   elsif Lower_Keyword = "button"
     or Lower_Keyword = "textbox"
     or Lower_Keyword = "dialog"
     or Lower_Keyword = "list"
     or Lower_Keyword = "listitem"
   then
      --  An MVP role word: the widget role goes to the GUI payload;
      --  in the subject part it also resets a wrongly categorized
      --  subject (a code span met before the role word is a widget
      --  name, not a file name)
      State.GUI_Role := To_Unbounded_String (Lower_Keyword);
      if In_Object_Part and then State.Object = No_Object then
         State.Object := Obj_Widget_Name;
      elsif In_Subject_Part then
         State.Subject := No_Subject;
      end if;

   elsif Lower_Keyword = "has" then
      --  The GUI item count step verb (cf. Initialize_GUI_Grammar):
      --  "the `To do` list has `3` items". Like "is", the rest of the
      --  step line is the object part.
      Set_Verb (Has_V, Loc);

   elsif Lower_Keyword = "items"
     or else Lower_Keyword = "item"
   then
      --  The GUI item count step object: the code span met since the
      --  "has" verb is the expected child count (cf. G080).
      if In_Object_Part
        and then Chunk.Verb = Has_V
        and then State.Object = Obj_Text
      then
         State.Object          := Obj_Widget_Count;
         State.GUI_Widget_Text := State.Object_String;
         State.Object_String   := Null_Unbounded_String;
      end if;

   elsif Lower_Keyword = "into" then
      --  The GUI type step marker (cf. Initialize_GUI_Grammar): the
      --  code span met before "into" is the entry text, the following
      --  one is the target field name. Without it, "I type `X`" stays
      --  the CLI interactive input step (a code span to the running
      --  command standard input, cf. A290).
      if In_Object_Part
        and then Chunk.Verb in Type_Text | Enter_Text
        and then State.Object = Obj_Text
      then
         State.GUI_Entry_Text := State.Object_String;
         State.Object_String  := Null_Unbounded_String;
         State.Object         := Obj_Widget_Name;
      end if;

   elsif Lower_Keyword = "appears" then
      Set_Verb (Appears, Loc);

   elsif Lower_Keyword = "press"
     or Lower_Keyword = "select"
   then
      --  "select" is the synonym of "press" for selectable items, like
      --  a listitem: "I select the `Buy milk` listitem" is the natural
      --  wording of the item activation (G020), the same action.
      Set_Verb (Press, Loc);

   elsif Lower_Keyword = "close" then
      --  The window close verb (cf. Initialize_GUI_Grammar):
      --  "I close the window" / "I close the `title` window"
      Set_Verb (Close, Loc);

   elsif Lower_Keyword = "enabled"
     or Lower_Keyword = "disabled"
   then
      --  "Then the `name` `role` is enabled / disabled"
      State.GUI_Widget_State :=
        (if Lower_Keyword = "enabled"
         then Model.Steps.GUI_Enabled
         else Model.Steps.GUI_Disabled);
      if In_Object_Part then
         State.Object := Obj_Widget_State;
      end if;

   elsif Lower_Keyword = "empty" then
      --  "Then the `name` `role` is empty": the emptiness of a
      --  container widget subtree (G030). The CLI steps never use a
      --  bare "empty" keyword (output emptiness has its own step
      --  forms), so this keyword is unambiguous.
      if In_Object_Part then
         State.Object := Obj_Widget_Empty;
      end if;

   end if;
   -- Put_Debug_Line ("  after Process_Keyword, state = " & State'Image, Loc);

end Process_Keyword;
