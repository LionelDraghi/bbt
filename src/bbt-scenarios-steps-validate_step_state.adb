-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

separate (BBT.Scenarios.Steps)

procedure Validate_Step_State (State               : Parse_State;
                               Loc                 : Location_Type;
                               Verb                : Verbs;
                               No_Subject_String   : Boolean;
                               No_Object_File_Name : Boolean;
                               Code_Block_Expected : out Boolean) is

begin
   Code_Block_Expected := The_Grammar
     (State.Prep, State.Subject_Attr, State.Subject, Verb, State.Object).Code_Block_Expected;

   case State.Subject is
      when No_Subject | Output_Subj | Subject_Text =>
         null;

      when Subject_File =>
         if No_Subject_String then
            IO.Put_Error ("File name expected in subject phrase", Loc);
            Code_Block_Expected := False;
            -- No sense to have a random error after a syntax error
         end if;

      when Dir_Subject =>
         if No_Subject_String then
            IO.Put_Error ("Dir name expected in subject phrase", Loc);
            --  Code_Block_Expected := False;
            --  -- No sense to have a random error after a syntax error
         end if;

   end case;

   case State.Object is
      when No_Object | Output_Obj | Obj_Text | Command_List | Error =>
         null;

      when Obj_File_Name =>
         if No_Object_File_Name then
            IO.Put_Error ("File name expected in object phrase", Loc);
            Code_Block_Expected := False;
            -- No sense to have a random error after a syntax error
         end if;

      when Obj_Dir_Name =>
         if No_Object_File_Name then
            IO.Put_Error ("Dir name expected in object phrase", Loc);
            --  Code_Block_Expected := False;
            --  -- No sense to have a random error after a syntax error
         end if;

   end case;

   if not State.Cmd_List.Is_Empty
      -- This is a "cmd1 or cmd2 or cmd3" Step
      and Natural (State.Cmd_List.Length) /= (State.Or_Met + 1)
      -- But the number of commands does not match the number of "or"
   then
      IO.Put_Error ("Missing command in 'run cmd1 or cmd2'", Loc);
      Put_Debug_Line ("  0r_Met = " & State.Or_Met'Image &
                      ", Cmd_List_Length = " & State.Cmd_List.Length'Image &
                      ", Cmd_List = " & State.Cmd_List'Image,  Loc);
   end if;

   Put_Debug_Line ("  0r_Met = " & State.Or_Met'Image &
                   ", Cmd_Expected = " & State.Cmd_Expected'Image &
                   ", Cmd_List = " & State.Cmd_List'Image,  Loc);

end Validate_Step_State;