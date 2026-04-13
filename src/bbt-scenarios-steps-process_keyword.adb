-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
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
      State.Not_Met := True;
      if Verb = Is_V then
         Set_Verb (Is_No, Loc);
      elsif Verb = Get then
         Set_Verb (Get_No, Loc);
      end if;

   elsif Lower_Keyword = "fail"
     or Lower_Keyword = "fails"
   then
      Set_Verb (Fail, Loc);

   elsif Lower_Keyword = "successfully" then
      State.Successfully_Met := True;

   elsif Lower_Keyword = "error" then
      State.Object := Error;

   elsif Lower_Keyword = "output" then
      if In_Subject_Part then
         State.Subject := Output_Subj;
      else
         State.Object := Output_Obj;
      end if;

   elsif Lower_Keyword = "contains" or
     Lower_Keyword = "contain"
   then
      if State.Not_Met then
         Set_Verb (Does_Not_Contain, Loc);
      else
         Set_Verb (Contains, Loc);
      end if;

   elsif Lower_Keyword = "match" or
     Lower_Keyword = "matches"
   then
      if State.Not_Met then
         Set_Verb (Does_Not_Match, Loc);
      else
         Set_Verb (Matches, Loc);
      end if;

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

end Process_Keyword;
