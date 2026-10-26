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
   -- Put_Debug_Line ("  In Process_Keyword, Keyword = " & Lower_Keyword, Loc);
   if Lower_Keyword = "executable" then
      State.Executable := True;

   elsif Lower_Keyword = "run" or Lower_Keyword = "running" then
      if State.Successfully_Met then
         Set_Verb (Successful_Run, Loc);
      else
         Set_Verb (Run, Loc);
      end if;

   elsif Lower_Keyword = "shell" then
      -- When I run shell `cmd1 | cmd2`
      -- The preposition that could precede "shell" is decoration:
      -- the keywords are "run" and "shell"
      if Verb = Run then
         Set_Verb (Run_Shell, Loc);

      elsif Verb = Successful_Run then
         Set_Verb (Successful_Run_Shell, Loc);
      end if;

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
         if State.Object /= Command_List then
            State.Object := Command_List;
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
      State.Subject := Subject_Text;

   elsif Lower_Keyword = "successfully" then
      State.Successfully_Met := True;

   elsif Lower_Keyword = "error" then
      State.Object := Error;

   elsif Lower_Keyword = "output" then
      if State.Object = Error then
         -- "error output": the standard error of the last command
         --    Then the error output contains `unknown option`
         --    Then there is no error output
         if In_Subject_Part then
            State.Subject := Stderr_Subj;
            State.Object  := No_Object;
         else
            State.Object := Stderr_Obj;
         end if;
      elsif In_Subject_Part then
         State.Subject := Output_Subj;
      else
         State.Object := Output_Obj;
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
         State.Subject := Dir_Subject;

      elsif In_Object_Part then
         State.Object := Obj_Dir_Name;

         State.Object_File_Name := State.Object_String;
         State.Object_String    := Null_Unbounded_String;
      end if;

   elsif Lower_Keyword = "file" then
      State.File_Type := Ordinary_File;

      if In_Subject_Part then
         State.Subject := Subject_File;

      elsif In_Object_Part then
         State.Object := Obj_File_Name;
         -- Then it was considered an Object_String, let's update that:
         State.Object_File_Name := State.Object_String;
         State.Object_String    := Null_Unbounded_String;
      end if;

   elsif Lower_Keyword = "unordered" then
      State.Ignore_Order := True;

   end if;
   -- Put_Debug_Line ("  after Process_Keyword, state = " & State'Image, Loc);

end Process_Keyword;
