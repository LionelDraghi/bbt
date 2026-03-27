-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

separate (BBT.Scenarios.Steps)

procedure Validate_Step_State (State               : Parse_State;
                               Loc                 : Location_Type;
                               -- Cmd_List            : Model.Steps.Cmd_List;
                               Verb                : Verbs;
                               No_Subject_String   : Boolean;
                               No_Object_File_Name : Boolean;
                               Code_Block_Expected : out Boolean) is
   -- pragma Unreferenced (Cmd_List);

   -- Check for multiple verbs in the step
   --  Verb_Count : Natural := 0;

begin
   --  -- Count all possible verbs in the grammar for this step
   --  for V in Verbs'Range loop
   --     if The_Grammar (State.Prep, State.Subject_Attr, State.Subject, V, State.Object).Action /= None then
   --        Verb_Count := Verb_Count + 1;
   --     end if;
   --  end loop;
   -- Warn if multiple verbs are found (excluding the actual verb used)
   -- Note: This warning is now handled by the Set_Verb procedure,
   -- so we don't need to generate it here to avoid duplicate warnings.

   Code_Block_Expected := The_Grammar
     (State.Prep, State.Subject_Attr, State.Subject, Verb, State.Object).Code_Block_Expected;

   --  Ada.Text_IO.Put_Line ("Prep         = " & Prep'Image);
   --  Ada.Text_IO.Put_Line ("Subject_Attr = " & Subject_Attr'Image);
   --  Ada.Text_IO.New_Line;
   --  Ada.Text_IO.Put_Line (Subject'Image & " " & Subject_String'Image
   --                        & " / " & Verb'Image & " / S ="
   --                        & Object'Image & " " & Object_String'Image
   --                        & " File Name =" & Object_File_Name'Image);
   --  Ada.Text_IO.Put_Line ("Action       = " & Action'Image);
   --  Ada.Text_IO.Put_Line ("Code_Block_Expected = " & Code_Block_Expected'Image);

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

   if not Is_Empty (State.Cmd_List.Length)
      -- This is a " cmd1 or cmd2" step
      and Natural (State.Cmd_List.Length) /= (State.Or_Met + 1))
      -- But the number of commands does not match )
   then
      IO.Put_Error ("Missing command in 'run cmd1 or cmd2'", Loc);
      IO.Put_Error ("  0r_Met = " & State.Or_Met'Image &
                    ", Cmd_List_Length = " & State.Cmd_List.Length'Image &
                    ", Cmd_List = " & State.Cmd_List'Image,  Loc);
   end if;

   Put_Debug_Line ("  0r_Met = " & State.Or_Met'Image &
                   ", Cmd_List = " & State.Cmd_List'Image,  Loc);

end Validate_Step_State;