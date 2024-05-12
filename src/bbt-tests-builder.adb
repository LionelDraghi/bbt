with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with BBT.IO;

with Text_Utilities; use Text_Utilities;

package body BBT.Tests.Builder is

   The_Doc_List : aliased Documents_Lists.Vector;

   type States is (In_Document,
                   In_Feature,
                   In_Scenario,
                   In_Background,
                   In_Step,
                   In_File_Content);

   type Step_States is (In_Given_Step,
                        In_When_Step,
                        In_Then_Step);

   type Backgrounds is (None, Doc, Feature);

   -- --------------------------------------------------------------------------
   package FSM is

      function Current_State return States;
      procedure Set_State (To_State : States);
      procedure Restore_Previous_State;

      function Current_Step_State return Step_States;
      procedure Set_Step_State (To_State : Step_States);

      function Current_Background return Backgrounds;
      procedure Set_Background (The_Background : Backgrounds);

   end FSM;

   package body FSM is separate;

   use FSM;

   -- --------------------------------------------------------------------------
   -- function Last_Doc return Documents_Lists.Cursor is (The_Doc_List.Last);
   function Last_Doc_Ref return Documents_Lists.Reference_Type is
     (The_Doc_List.Reference (The_Doc_List.Last));

   function Last_Feature_Ref return Feature_Lists.Reference_Type is
     (Last_Doc_Ref.Feature_List.Reference
        (Last_Doc_Ref.Element.Feature_List.Last));

   function Last_Scenario_Ref return Scenario_Lists.Reference_Type is
     (Last_Feature_Ref.Element.Scenario_List.Reference
        (Last_Feature_Ref.Scenario_List.Last));

   function Last_Step_Ref return Step_Lists.Reference_Type is
     (Last_Scenario_Ref.Element.Step_List.Reference
        (Last_Scenario_Ref.Step_List.Last));

   -- --------------------------------------------------------------------------
   procedure Add_Document (Name : String) is
   begin
      --  Put_Line ("Add_Document " & Name'Image, Verbosity => IO.Debug);
      The_Doc_List.Append ((Empty_Document with delta
                             Name => To_Unbounded_String (Name)));
      Set_State (In_Document);
      Set_Background (Doc);
   end Add_Document;

   -- --------------------------------------------------------------------------
   procedure Add_Feature (Name : String;    Loc : Location_Type) is
   begin
      --  Put_Line ("Add_Feature " & Name'Image, Verbosity => IO.Debug);
      Last_Doc_Ref.Feature_List.Append
        ((Empty_Feature with delta
           Name => To_Unbounded_String (Name), Location => Loc));
      Set_State (In_Feature);
      Set_Background (Feature);
   end Add_Feature;

   -- --------------------------------------------------------------------------
   procedure Add_Scenario (Name : String;    Loc : Location_Type) is
   begin
      --  Put_Line ("Add_Scenario " & Name'Image, Verbosity => IO.Debug);

      -- We accept scenario without Feature
      if Last_Doc_Ref.Feature_List.Is_Empty then
         Add_Feature ("", Loc); -- no name feature
      end if;

      Last_Feature_Ref.Scenario_List.Append
        ((Empty_Scenario with delta
             Name => To_Unbounded_String (Name), Location => Loc));
      Set_State (In_Scenario);
      Set_Background (None);

   end Add_Scenario;

   -- --------------------------------------------------------------------------
   procedure Add_Background (Name : String;    Loc : Location_Type) is
   begin
      --  Put_Line ("Add_Background " & Name'Image, Verbosity => IO.Debug);

      case Current_State is
         when In_Document =>
            Last_Doc_Ref.Background.Name := To_Unbounded_String (Name);

         when In_Feature =>
            Last_Feature_Ref.Background.Name := To_Unbounded_String (Name);

         when others =>
            IO.Put_Error ("Background should be declared at document"
                          & "or at Feature level, not " & Current_State'Image,
                          Loc);
      end case;

      Set_State (In_Background);
   end Add_Background;

   -- --------------------------------------------------------------------------
   procedure Add_Step (Step : Step_Type) is
      -- Post : constant String := Postfix (Step.Location);
   begin
      Put_Line ("Add_Step " & Step'Image,
                Step.Location,
                Verbosity => IO.Debug);
      if Current_State = In_Document or Current_State = In_Feature then
         raise Missing_Scenario with "Prefix & Premature Step """ &
           To_String (Step.Step_String)
           & """ declaration, should be in Scenarios or Background";
      end if;

      case Step.Cat is
         when Unknown =>
            if Step.Action = None then
               IO.Put_Error ("No context to determine step kind of "
                             & To_String (Step.Step_String),
                             Step.Location);
            end if;

         when Given_Step =>
            if Current_Step_State = In_When_Step then
               IO.Put_Warning ("Given step """ & To_String (Step.Step_String) &
                                 """ appears to late, after a ""When""",
                               Step.Location);
            elsif Current_Step_State = In_Then_Step then
               IO.Put_Warning ("Given step """ & To_String (Step.Step_String) &
                                 """ appears to late, after a ""Then""",
                               Step.Location);
            end if;
            Set_Step_State (In_Given_Step);

         when When_Step  =>
            if Current_Step_State = In_Then_Step then
               IO.Put_Warning ("When step """ & To_String (Step.Step_String) &
                                 """ appears to late, after a ""Then""",
                               Step.Location);
            end if;
            Set_Step_State (In_When_Step);

         when Then_Step  =>
            Set_Step_State (In_Then_Step);

      end case;

      case Current_Background is
         when Doc     => Last_Doc_Ref.    Background.Step_List.Append (Step);
         when Feature => Last_Feature_Ref.Background.Step_List.Append (Step);
         when None    => Last_Scenario_Ref.Step_List.Append           (Step);
      end case;

   end Add_Step;

   -- --------------------------------------------------------------------------
   procedure Add_Code_Block (Loc : Location_Type) is
      -- Code block outside of Steps definition is considered as a comment.
      -- There is no state change, the code block mark is just recorded in
      -- comments at the right level.
      -- At the end of the code block section, the previous Step state
      -- is restored.
      -- Post : constant String := Postfix (Loc);
   begin
      Put_Line ("Add_Code_Block, Current_State = ", Loc, Verbosity => IO.Debug);
      case Current_State is
         when In_Step | In_Scenario | In_Background =>
            -- Entering code block
            -- note that In_Scenario is included, so that if a comment is
            -- inserted between the step and the code fenced file content,
            -- it will work.
            Set_State (In_File_Content);

         when In_File_Content =>
            -- Exiting code block
            Restore_Previous_State;

         when In_Document =>
            Last_Doc_Ref.Comment.Append ("```");

         when In_Feature =>
            Last_Feature_Ref.Comment.Append ("```");

      end case;
   end Add_Code_Block;

   -- --------------------------------------------------------------------------
   procedure Add_Line (Line : String;    Loc : Location_Type) is
      -- Post : constant String := Postfix (Loc);
   begin
      Put_Line ("Add_Line " & Line'Image, Loc, Verbosity => IO.Debug);
      case Current_State is
         when In_Document =>
            Last_Doc_Ref.Comment.Append (Line);

         when In_Feature =>
            Last_Feature_Ref.Comment.Append (Line);

         when In_Scenario | In_Step | In_Background =>
            Last_Scenario_Ref.Comment.Append (Line);
            -- There is no comment attached to Step

         when In_File_Content =>
            -- this is not a comment, we are in a file content description
            Put_Line ("File content = """ & Line & """", Loc,
                      Verbosity => IO.Debug);
            Last_Step_Ref.File_Content.Append (Line);

      end case;
   end Add_Line;

   -- --------------------------------------------------------------------------
   function The_Tests_List return access Documents_Lists.Vector is
     (The_Doc_List'Access);

end BBT.Tests.Builder;
