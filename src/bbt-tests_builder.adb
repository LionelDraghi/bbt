with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with BBT.IO;
with BBT.Settings; -- use BBT.IO;

with Text_Utilities; use Text_Utilities;


package body BBT.Tests_Builder is

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
   -- IO renamed with local Topic
   procedure Put_Line
     (Item  : String;
      File  : String  := "";
      Line  : Integer := 0;
      Level : BBT.Settings.Print_Out_Level := BBT.IO.Normal;
      Topic : Settings.Extended_Topics := Settings.Builder) renames IO.Put_Line;

   -- --------------------------------------------------------------------------
   -- function Last_Doc      return Documents_Lists.Cursor is (The_Doc_List.Last);
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
      --  Put_Line ("Add_Document " & Name'Image, Level => IO.Debug);
      The_Doc_List.Append ((Empty_Document with delta
                             Name => To_Unbounded_String (Name)));
      Set_State (In_Document);
      Set_Background (Doc);
   end Add_Document;

   -- --------------------------------------------------------------------------
   procedure Add_Feature (Name : String) is
   begin
      --  Put_Line ("Add_Feature " & Name'Image, Level => IO.Debug);
      Last_Doc_Ref.Feature_List.Append
        ((Empty_Feature with delta Name => To_Unbounded_String (Name)));
      Set_State (In_Feature);
      Set_Background (Feature);
   end Add_Feature;

   -- --------------------------------------------------------------------------
   procedure Add_Scenario (Name : String) is
   begin
      --  Put_Line ("Add_Scenario " & Name'Image, Level => IO.Debug);

      -- We accept scenario without Feature
      if Last_Doc_Ref.Feature_List.Is_Empty then
         Add_Feature (""); -- no name feature
      end if;

      Last_Feature_Ref.Scenario_List.Append
        ((Empty_Scenario with delta Name => To_Unbounded_String (Name)));

      Set_State (In_Scenario);
      Set_Background (None);

   end Add_Scenario;

   -- --------------------------------------------------------------------------
   procedure Add_Background (Name : String) is
   begin
      --  Put_Line ("Add_Background " & Name'Image, Level => IO.Debug);

      case Current_State is
         when In_Document =>
            Last_Doc_Ref.Background.Name := To_Unbounded_String (Name);

         when In_Feature =>
            Last_Feature_Ref.Background.Name := To_Unbounded_String (Name);

         when others =>
            IO.Put_Error ("Background should be declared at document"
                          & "or at Feature level, not " & Current_State'Image);
      end case;

      Set_State (In_Background);
   end Add_Background;

   -- --------------------------------------------------------------------------
   procedure Add_Step (Step : Step_Details) is
      Cat : Extended_Step_Categories := Unknown;
   begin
      Put_Line ("Add_Step " & Step'Image,
                -- & " Current_Background = " & Current_Background'Image
                -- & "========================================",
                Level => IO.Debug);
      if Current_State = In_Document or Current_State = In_Feature then
         raise Missing_Scenario with "Premature Step """ &
           To_String (Step.Text) & """ declaration, should be in Scenario or Background";
         -- Fixme : ajouter file location
      end if;
      --  Put_Line ("Add_Step : " & Step'Image,
      --            Level => IO.Debug);
      if Step.Cat = Unknown then
         -- category is not defined by the line (for example a line starting
         -- with "and"), and so the category will defined by the history
         -- (if the previous line was "given" then the folowwine "and" line will
         -- be in the same category).
         if Current_Step_State = In_Given_Step then
            Cat := Given_Step;
         elsif Current_Step_State = In_When_Step then
            Cat := When_Step;
         elsif Current_Step_State = In_Then_Step then
            Cat := Then_Step;
         else
            IO.Put_Line
              ("Add_Step : unknown category, but not already in a step???",
               Level => IO.Quiet);
         end if;

      else
         Cat := Step.Cat;

      end if;

      case Step.Cat is
         when Unknown =>
            null; --  impossible, but if ever, there is an error
            --  message here above

         when Given_Step =>
            if Current_Step_State = In_When_Step then
               IO.Put_Warning ("Given step """ & To_String (Step.Text) &
                                 """ appears to late, after a ""When""");
            elsif Current_Step_State = In_Then_Step then
               IO.Put_Warning ("Given step """ & To_String (Step.Text) &
                                 """ appears to late, after a ""Then""");
            end if;
            Set_Step_State (In_Given_Step);

         when When_Step  =>
            if Current_Step_State = In_Then_Step then
               IO.Put_Warning ("When step """ & To_String (Step.Text) &
                                 """ appears to late, after a ""Then""");
            end if;
            Set_Step_State (In_When_Step);

         when Then_Step  =>
            Set_Step_State (In_Then_Step);

      end case;

      case Current_Background is
         when Doc =>
            Last_Doc_Ref.Background.Step_List.Append
              (Step_Type'(Step_String  => Step.Text,
                          File_Content => Empty_Text,
                          Details      => Step,
                          Category     => Cat));
            --  Put_Line ("******* to Doc",
            --            Level => IO.Debug);
         when Feature =>
            Last_Feature_Ref.Background.Step_List.Append
              (Step_Type'(Step_String  => Step.Text,
                          File_Content => Empty_Text,
                          Details      => Step,
                          Category     => Cat));
            --  Put_Line ("******* to Feature",
            --            Level => IO.Debug);
         when None =>
            Last_Scenario_Ref.Step_List.Append
              (Step_Type'(Step_String  => Step.Text,
                          File_Content => Empty_Text,
                          Details      => Step,
                          Category     => Cat));
            --  Put_Line ("******* to Last_Scenario.Step_list",
            --            Level => IO.Debug);
      end case;

   end Add_Step;

   -- --------------------------------------------------------------------------
   procedure Add_Code_Block is
      -- Code block outside of Steps definition is considered as a comment.
      -- There is no state change, the code block mark is just recorded in
      -- comments at the right level.
      -- At the end of the code block section, the previous Step state
      -- is restored.
   begin
      Put_Line ("Add_Code_Block, Current_State = "
                & Current_State'Image, Level => IO.Debug);
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
   procedure Add_Line (Line : String) is
   begin
      -- Put_Line ("Add_Line " & Line'Image, Level => IO.Debug);
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
            Put_Line ("File content = """ & Line & """",
                      Level => IO.Debug);
            Last_Step_Ref.File_Content.Append (Line);

      end case;
   end Add_Line;

   -- --------------------------------------------------------------------------
   function The_Document_List return access Documents_Lists.Vector is
     (The_Doc_List'Access);

end BBT.Tests_Builder;
