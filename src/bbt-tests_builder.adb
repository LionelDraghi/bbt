with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with BBT.IO;
with BBT.Settings; -- use BBT.IO;

package body BBT.Tests_Builder is

   The_Doc_List : aliased Documents_Lists.Vector;

   type States is (In_Document,
                   In_Feature,
                   In_Scenario,
                   In_Given_Step,
                   In_When_Step,
                   In_Then_Step,
                   In_File_Content);
   subtype Step_States is States range In_Given_Step .. In_Then_Step;
   State : States := In_Document;
   Previous_State : States;

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
      Put_Line ("Add_Document """ & Name & """", Level => IO.Debug);
      The_Doc_List.Append (Document_Type'
                             (Name         => To_Unbounded_String (Name),
                              Feature_List => Feature_Lists.Empty,
                              Comment      => Empty_Text));
      State := In_Document;
   end Add_Document;

   -- --------------------------------------------------------------------------
   procedure Add_Feature (Name : String) is
   begin
      Put_Line ("Add_Feature = """ & Name & """", Level => IO.Debug);
      Last_Doc_Ref.Feature_List.Append
        (Feature_Type'(Name          => To_Unbounded_String (Name),
                       Scenario_List => Scenario_Lists.Empty,
                       Comment       => Empty_Text));
      State := In_Feature;
   end Add_Feature;

   -- --------------------------------------------------------------------------
   procedure Add_Scenario (Name : String) is
   begin
      Put_Line ("Add_Scenario """ & Name & """", Level => IO.Debug);
      Last_Feature_Ref.Scenario_List.Append
        (Scenario_Type'(Name       => To_Unbounded_String (Name),
                        Step_List => Step_Lists.Empty,
                        --  Given_List => Given_Lists.Empty,
                        --  When_List  => When_Lists.Empty,
                        --  Then_List  => Then_Lists.Empty,
                        Comment    => Empty_Text,
                        Failed_Step_Count | Successful_Step_Count => 0));
      State := In_Scenario;
   end Add_Scenario;

   -- --------------------------------------------------------------------------
   procedure Add_Step (Step : Step_Details) is
      Cat : Extended_Step_Categories := Unknown;
   begin
      if State = In_Document or State = In_Feature then
         raise Missing_Scenario with "Premature Step """ &
           To_String (Step.Text) & """ declaration, missing Scenario";
         -- Fixme : ajouter file location
      end if;
      --  Put_Line ("Add_Step : " & Step'Image,
      --            Level => IO.Debug);
      if Step.Cat = Unknown then
         -- category is not defined by the line (for example a line starting
         -- with "and"), and so the category will defined by the history
         -- (if the previous line was "given" then the folowwine "and" line will
         -- be in the same category).
         if State = In_Given_Step then
            Cat := Given_Step;
         elsif State = In_When_Step then
            Cat := When_Step;
         elsif State = In_Then_Step then
            Cat := Then_Step;
         else
            IO.Put_Error
              ("Add_Step : unknown category, but not already in a step???");
         end if;

      else
         Cat := Step.Cat;

      end if;

      case Step.Cat is
         when Unknown =>
            null; --  impossible, but if ever, there is an error
                  --  message here above

         when Given_Step =>
            if State = In_When_Step then
               IO.Put_Warning ("Given step """ & To_String (Step.Text) &
                                 """ appears to late, after a ""When""");
            elsif State = In_Then_Step then
               IO.Put_Warning ("Given step """ & To_String (Step.Text) &
                                 """ appears to late, after a ""Then""");
            end if;
            State := In_Given_Step;

         when When_Step  =>
            if State = In_Then_Step then
               IO.Put_Warning ("When step """ & To_String (Step.Text) &
                                 """ appears to late, after a ""Then""");
            end if;
            State := In_When_Step;

         when Then_Step  =>
            State := In_Then_Step;

      end case;

      Last_Scenario_Ref.Step_List.Append
        (Step_Type'(Text         => Step.Text,
                    File_Content => Empty_Text,
                    Details      => Step,
                    Category     => Cat));


   end Add_Step;

   -- --------------------------------------------------------------------------
   procedure Add_Code_Block is
      -- Code block outside of Steps definition is considered as a comment.
      -- There is no state change, the code block mark is just recorded in
      -- comments at the right level.
      -- At the end of the code block section, the previous Step state
      -- is restored.
   begin
      case State is
         when Step_States =>
            -- entering code block
            Previous_State := State;
            State := In_File_Content;

         when In_File_Content =>
            -- exiting code block
            State := Previous_State;

         when In_Document =>
            Last_Doc_Ref.Comment.Append ("```");

         when In_Feature =>
            Last_Feature_Ref.Comment.Append ("```");

         when In_Scenario =>
            Last_Scenario_Ref.Comment.Append ("```");

      end case;
   end Add_Code_Block;

   -- --------------------------------------------------------------------------
   procedure Add_Line (Line : String) is
   begin
      case State is
         when In_Document =>
            Last_Doc_Ref.Comment.Append (Line);

         when In_Feature =>
            Last_Feature_Ref.Comment.Append (Line);

         when In_Scenario | Step_States =>
            Last_Scenario_Ref.Comment.Append (Line);
            -- There is no comment attached to Step

         when In_File_Content =>
            -- this is not a comment, we are in a file content description
            Put_Line ("File content = """ & Line & """",
                      Level => IO.Debug);

            case Previous_State is
               when In_Given_Step | In_When_Step | In_Then_Step =>
                  Last_Step_Ref.File_Content.Append (Line);

               when others =>
                  IO.Put_Error ("Recording file content """ &
                               Line & """ but Previous state is not a Step");

            end case;
      end case;
   end Add_Line;

   -- --------------------------------------------------------------------------
   function The_Document_List return access Documents_Lists.Vector is
     (The_Doc_List'Access);

end BBT.Tests_Builder;
