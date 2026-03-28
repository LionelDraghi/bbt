-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

separate (BBT.Scenarios.Steps)

procedure Process_Code_Span (Tok             : String;
                             Loc             : Location_Type;
                             In_Subject_Part : Boolean;
                             In_Object_Part  : Boolean;
                             Current_Verb    : Verbs;
                             State           : in out Parse_State) is
begin
   --  Put_Debug_Line ("  In Process_Code_Span, State.Cmd_Expected = " &
   --                  State.Cmd_Expected'Image
   --                  & ", Tok = " & Tok, Loc);

   if In_Subject_Part then
      State.Subject_String := To_Unbounded_String (Tok);
      --  Put_Debug_Line ("  Subject_String = " & Tok, Loc);

   else
      if State.Object = Obj_File_Name or State.Object = Obj_Dir_Name then
         -- "file" or "dir" keyword already meet
         State.Object_File_Name := To_Unbounded_String (Tok);
         --  Put_Debug_Line ("  Object_File_Name = " & Tok, Loc);
      else
         -- Otherwise, we don't know yet if the Code_Span is
         -- a simple string or a file name.
         State.Object_String := To_Unbounded_String (Tok);
         --  Put_Debug_Line ("  Object_String = " & Tok, Loc);
      end if;
   end if;

   if In_Subject_Part and then State.Subject = No_Subject then
      if State.File_Type = Directory then
         State.Subject := Dir_Subject;
         --  Put_Debug_Line ("  Subject = Dir_Subject", Loc);
      else
         State.Subject := Subject_File;
         --  Put_Debug_Line ("  Subject = Subject_File", Loc);
      end if;

   elsif In_Object_Part and State.Cmd_Expected then
      State.Cmd_List.Append (Tok);
      State.Cmd_Expected := False;
      --  Put_Debug_Line ("  Cmd_List.Append : " & Tok, Loc);

   elsif In_Object_Part and then State.Object = No_Object then
      case Current_Verb is
         when No_Verb |
              Is_No   =>
            -- Those verbs are always followed by a file/dir name
            if State.File_Type = Directory then
               State.Object := Obj_Dir_Name;
               --  Put_Debug_Line ("  Object = Obj_Dir_Name", Loc);
            else
               State.Object := Obj_File_Name;
               --  Put_Debug_Line ("  Object = Obj_File_Name", Loc);
            end if;

         when Run              |
              Successful_Run   |
              Get              |
              Get_No           |
              Contains         |
              Matches          |
              Does_Not_Contain |
              Does_Not_Match   |
              Containing       =>
            -- Those verbs are always followed by a text
            State.Object := Obj_Text;
            --  Put_Debug_Line ("  Object = Obj_Text", Loc);

         when Is_V =>
            -- Complex case where it depends not only on the
            -- verb...
            if State.Subject = No_Subject then
               -- Example : Given there is a `config.ini` file
               if State.File_Type = Directory then
                  State.Object := Obj_Dir_Name;
                  --  Put_Debug_Line ("  Object = Obj_Dir_Name", Loc);
               else
                  State.Object := Obj_File_Name;
                  --  Put_Debug_Line ("  Object = Obj_File_Name", Loc);
               end if;

            else
               -- Then output is xxxx
               -- or
               -- Then `file` is xxxx
               State.Object := Obj_Text;
               Put_Debug_Line ("  Object = Obj_Text", Loc);

            end if;
      end case;
   end if;
end Process_Code_Span;
