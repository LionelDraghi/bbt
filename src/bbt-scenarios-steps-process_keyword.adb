-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

separate (BBT.Scenarios.Steps)

procedure Process_Keyword (Tok      : String;
                           State    : in out Parse_State;
                           Loc      : Location_Type) is
                           -- Cmd_List : in out Model.Steps.Cmd_List) is

   use Ada.Strings.Maps.Constants;
   use Chunk;

   Lower_Keyword : constant String := Translate
     (Source  => Tok,
      Mapping => Lower_Case_Map);

begin
   if Lower_Keyword = "executable" then
      State.Executable := True;
      Put_Debug_Line ("  Executable = True", Loc);

   elsif Lower_Keyword = "run" or Lower_Keyword = "running" then
      if State.Successfully_Met then
         Set_Verb (Successful_Run, Loc);
         Put_Debug_Line ("  Verb = Successful_Run", Loc);
      else
         Set_Verb (Run, Loc);
         Put_Debug_Line ("  Verb = Run", Loc);
      end if;

   elsif Lower_Keyword = "or" then
      State.Or_Met := @ + 1;
      if State.Object /= Command_List then
         State.Object := Command_List;
         -- There should already be a code span, let's
         -- move it in the list
         State.Cmd_List.Append (To_String (State.Object_String));
         State.Object_String := Null_Unbounded_String;
      end if;

   elsif Lower_Keyword = "get" then
      Set_Verb (Get, Loc);
      Put_Debug_Line ("  Verb = Get", Loc);

   elsif Lower_Keyword = "is" then
      Set_Verb (Is_V, Loc);
      Put_Debug_Line ("  Verb = Is", Loc);

   elsif Lower_Keyword = "no"
     or Lower_Keyword = "not"
     or Lower_Keyword = "dont"
     or Lower_Keyword = "doesnt"
     or Lower_Keyword = "doesn't"
   then
      State.Not_Met := True;
      Put_Debug_Line ("  Not_Met = True", Loc);
      if Verb = Is_V then
         Set_Verb (Is_No, Loc);
         Put_Debug_Line ("  Verb = Is_No", Loc);
      elsif Verb = Get then
         Set_Verb (Get_No, Loc);
         Put_Debug_Line ("  Verb = Get_No", Loc);
      end if;

   elsif Lower_Keyword = "successfully" then
      State.Successfully_Met := True;
      Put_Debug_Line ("  Successfully_Met = True", Loc);

   elsif Lower_Keyword = "error" then
      State.Object := Error;
      Put_Debug_Line ("  Object = Error", Loc);

   elsif Lower_Keyword = "output" then
      if In_Subject_Part then
         State.Subject := Output_Subj;
         Put_Debug_Line ("  Subject = Output_Subj", Loc);
      else
         State.Object := Output_Obj;
         Put_Debug_Line ("  Object = Output_Obj", Loc);
      end if;

   elsif Lower_Keyword = "contains" or
     Lower_Keyword = "contain"
   then
      Put_Debug_Line ("  Processing 'contains', Not_Met = " & State.Not_Met'Image, Loc);
      if State.Not_Met then
         Set_Verb (Does_Not_Contain, Loc);
         Put_Debug_Line ("  Verb = Does_Not_Contain", Loc);
      else
         Set_Verb (Contains, Loc);
         Put_Debug_Line ("  Verb = Contains", Loc);
      end if;

   elsif Lower_Keyword = "match" or
     Lower_Keyword = "matches"
   then
      if State.Not_Met then
         Set_Verb (Does_Not_Match, Loc);
         Put_Debug_Line ("  Verb = Does_Not_Match", Loc);
      else
         Set_Verb (Matches, Loc);
         Put_Debug_Line ("  Verb = Matches", Loc);
      end if;

   elsif Lower_Keyword = "containing" then
      Set_Verb (Containing, Loc);
      Put_Debug_Line ("  Verb = Containing", Loc);

   elsif Lower_Keyword = "new" then
      State.Subject_Attr := New_SA;
      Put_Debug_Line ("  Subject_Attr = New_SA", Loc);

   elsif Lower_Keyword = "directory" or Lower_Keyword = "dir"
   then
      State.File_Type := Directory;

      if In_Subject_Part then
         State.Subject := Dir_Subject;
         Put_Debug_Line ("  Subject = Dir_Subject", Loc);

      elsif In_Object_Part then
         State.Object := Obj_Dir_Name;
         Put_Debug_Line ("  Object = Obj_Dir_Name", Loc);

         State.Object_File_Name := State.Object_String;
         State.Object_String    := Null_Unbounded_String;
      end if;

   elsif Lower_Keyword = "file" then
      State.File_Type := Ordinary_File;
      Put_Debug_Line ("  File_Type = Ordinary_File", Loc);

      if In_Subject_Part then
         State.Subject := Subject_File;
         Put_Debug_Line ("  Subject = Subject_File", Loc);

      elsif In_Object_Part then
         State.Object := Obj_File_Name;
         Put_Debug_Line ("  Object = Obj_File_Name", Loc);
         -- then it was considered an Object_String,
         -- let's update that:
         State.Object_File_Name := State.Object_String;
         State.Object_String    := Null_Unbounded_String;
      end if;

   elsif Lower_Keyword = "unordered" then
      State.Ignore_Order := True;
      Put_Debug_Line ("  Subject = Subject_File", Loc);

   end if;

end Process_Keyword;
