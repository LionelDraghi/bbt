
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

separate (BBT.Main)

procedure Create_Template is
   Template  : File_Type;
   File_Name : String renames Settings.Template_Name;

begin
   if Ada.Directories.Exists (File_Name) then
      IO.Put_Line ("File " & File_Name & " already exists",
                   Level => IO.Quiet);

   else
      Create (Template, Name => Settings.Template_Name);
      Set_Output (Template);

      Put_Line ("## Feature : *as in* test");
      New_Line;

      New_Line;
      Put_Line ("Testing that a message is put on standard output");
      Put_Line ("and that the command returns no error");
      New_Line;
      Put_Line ("### Scenario : Short option form");
      Put_Line ("  - When I run 'uut -v'");
      Put_Line ("  - Then I get no error");
      Put_Line ("  - And I get 'uut v0.1.0'");
      New_Line;
      Put_Line ("### Scenario : Long form");
      New_Line;
      Put_Line ("Checking the postconditions of another test when running something else");
      Put_Line ("(here, an equivalent option on command line");
      New_Line;

      Put_Line ("  - Then I get just as in `Short option form` scenario");
      New_Line;
      Put_Line ("Testing an expected multiline output");
      New_Line;
      Put_Line ("### Scenario : asking for uut help");
      New_Line;

      Put_Line ("- Then I get ");
      New_Line;
      Put_Line ("```");
      Put_Line ("uut usage :");
      Put_Line ("uut file_name create|read|append [-with text]");
      Put_Line ("```");
      New_Line;
      Put_Line ("-- More extensive explanations : http://lionel.draghi.free.fr/Archicheck/rules/");
      Put_Line ("-- ");
      Put_Line ("-- File generated with BBT " & Settings.BBT_Version);

      Close (Template);
      Set_Output (Standard_Output);

      Put_Line ("Template file " & File_Name & " created.");

   end if;

end Create_Template;
