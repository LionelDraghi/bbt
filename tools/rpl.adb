with Ada.Command_Line;
with Ada.Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

procedure Rpl is
   use Ada.Command_Line;
   use Ada.Strings.Unbounded;
   use type Ada.Containers.Count_Type;

   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Unbounded_String);

   Quiet       : Boolean := False;
   Verbose     : Boolean := False;
   Dry_Run     : Boolean := False;
   Ignore_Case : Boolean := False;
   Whole_Words : Boolean := False;
   Recursive   : Boolean := False;

   Items : String_Vectors.Vector;

   procedure Usage is
   begin
      Ada.Text_IO.Put_Line ("usage: rpl [options] OLD_TEXT NEW_TEXT FILE [FILE ...]");
      Ada.Text_IO.Put_Line ("Search and replace OLD_TEXT by NEW_TEXT in files.");
      Ada.Text_IO.Put_Line ("");
      Ada.Text_IO.Put_Line ("Options:");
      Ada.Text_IO.Put_Line ("  -h, --help       Show this help message and exit");
      Ada.Text_IO.Put_Line ("  --version        Show version and exit");
      Ada.Text_IO.Put_Line ("  -i              Ignore case when searching");
      Ada.Text_IO.Put_Line ("  -q              Quiet mode (no output)");
      Ada.Text_IO.Put_Line ("  -w              Match whole words only");
      Ada.Text_IO.Put_Line ("  -v              Verbose mode (show detailed output)");
      Ada.Text_IO.Put_Line ("  -s              Dry run (show changes without modifying files)");
      Ada.Text_IO.Put_Line ("  -R              Process directories recursively");
   end Usage;

   function Lower (S : String) return String is
      R : String := S;
   begin
      for C of R loop
         C := Ada.Characters.Handling.To_Lower (C);
      end loop;
      return R;
   end Lower;

   function Is_Word_Char (C : Character) return Boolean is
   begin
      return C in 'A' .. 'Z' or else C in 'a' .. 'z' or else C in '0' .. '9' or else C = '_';
   end Is_Word_Char;

   function Boundary_OK (Source : String; Pos : Positive; Len : Natural) return Boolean is
      Before_OK : constant Boolean := Pos = Source'First or else not Is_Word_Char (Source (Pos - 1));
      After_Pos : constant Natural := Pos + Len;
      After_OK  : constant Boolean := After_Pos > Source'Last or else not Is_Word_Char (Source (After_Pos));
   begin
      return Before_OK and then After_OK;
   end Boundary_OK;

   function Starts_With_At (Source, Pattern : String; Pos : Positive) return Boolean is
      Last : constant Natural := Pos + Pattern'Length - 1;
   begin
      if Pattern = "" or else Last > Source'Last then
         return False;
      end if;

      if Ignore_Case then
         return Lower (Source (Pos .. Last)) = Lower (Pattern);
      else
         return Source (Pos .. Last) = Pattern;
      end if;
   end Starts_With_At;

   function Replace_All
     (Source      : String;
      Old_Text    : String;
      New_Text    : String;
      Changed     : out Boolean;
      Occurrences : out Natural) return String
   is
      R : Unbounded_String;
      I : Natural := Source'First;
   begin
      Changed := False;
      Occurrences := 0;

      if Old_Text = "" then
         return Source;
      end if;

      while I <= Source'Last loop
         if Starts_With_At (Source, Old_Text, I)
           and then (not Whole_Words or else Boundary_OK (Source, I, Old_Text'Length))
         then
            Append (R, New_Text);
            I := I + Old_Text'Length;
            Changed := True;
            Occurrences := Occurrences + 1;
         else
            Append (R, Source (I));
            I := I + 1;
         end if;
      end loop;

      return To_String (R);
   end Replace_All;

   procedure Process_File
     (Path     : String;
      Old_Text : String;
      New_Text : String)
   is
      Input       : Ada.Text_IO.File_Type;
      Output      : Ada.Text_IO.File_Type;
      Content     : Unbounded_String;
      New_Content : Unbounded_String;
      Changed     : Boolean;
      Occurrences : Natural;
      Temp_Path : constant String := Path & ".tmp";
   begin

      Ada.Text_IO.Open (Input, Ada.Text_IO.In_File, Path);
      while not Ada.Text_IO.End_Of_File (Input) loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line (Input);
         begin
            Append (Content, Line);
            if not Ada.Text_IO.End_Of_File (Input) then
               Append (Content, Character'Val (10));
            end if;
         end;
      end loop;
      Ada.Text_IO.Close (Input);

      New_Content := To_Unbounded_String
        (Replace_All (To_String (Content), Old_Text, New_Text, Changed, Occurrences));

      if Changed and then not Dry_Run then
         Ada.Text_IO.Create (Output, Ada.Text_IO.Out_File, Temp_Path);
         Ada.Text_IO.Put (Output, To_String (New_Content));
         Ada.Text_IO.Close (Output);

         Ada.Directories.Delete_File (Path);
         Ada.Directories.Rename (Temp_Path, Path);
      end if;

      if Verbose or else (not Quiet and then Changed) then
         Ada.Text_IO.Put_Line (Path & ":" & Natural'Image (Occurrences) & " replacement(s)");
      end if;
   exception
      when others =>
         if Ada.Text_IO.Is_Open (Input) then Ada.Text_IO.Close (Input); end if;
         if Ada.Text_IO.Is_Open (Output) then Ada.Text_IO.Close (Output); end if;
         if Ada.Directories.Exists (Temp_Path) then Ada.Directories.Delete_File (Temp_Path); end if;
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "rpl: cannot process " & Path);
         Set_Exit_Status (Failure);
   end Process_File;

   procedure Process_Target
     (Path     : String;
      Old_Text : String;
      New_Text : String)
   is
      use Ada.Directories;
      use type Ada.Directories.File_Kind;
      Search    : Search_Type;
      Dir_Entry : Directory_Entry_Type;
      Pattern   : constant String := "*";
   begin
      if Kind (Path) = Directory then
         if not Recursive then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "rpl: " & Path & " is a directory; use -R");
            Set_Exit_Status (Failure);
            return;
         end if;

         Start_Search
           (Search    => Search,
            Directory => Path,
            Pattern   => Pattern,
            Filter    => (Ordinary_File => True, Directory => True, others => False));

         while More_Entries (Search) loop
            Get_Next_Entry (Search, Dir_Entry);
            declare
               Full : constant String := Full_Name (Dir_Entry);
               Base : constant String := Simple_Name (Dir_Entry);
            begin
               if Base /= "." and then Base /= ".." then
                  if Kind (Dir_Entry) = Directory then
                     Process_Target (Full, Old_Text, New_Text);
                  else
                     Process_File (Full, Old_Text, New_Text);
                  end if;
               end if;
            end;
         end loop;
         End_Search (Search);
      else
         Process_File (Path, Old_Text, New_Text);
      end if;
   end Process_Target;

   I : Positive := 1;
begin
   if Argument_Count = 0 then
      Usage;
      Set_Exit_Status (Failure);
      return;
   end if;

   while I <= Argument_Count loop
      declare
         A : constant String := Argument (I);
      begin
         if A = "--" then
            I := I + 1;
            exit;
         elsif A = "-h" or else A = "--help" then
            Usage;
            return;
         elsif A = "--version" then
            Ada.Text_IO.Put_Line ("rpl for bbt 0.1");
            return;

         elsif A'Length > 0 and then A (A'First) = '-' then
            for J in A'First + 1 .. A'Last loop
               case A (J) is
                  when 'i' => Ignore_Case := True;
                  when 'w' => Whole_Words := True;
                  when 'q' => Quiet := True;
                  when 'v' => Verbose := True;
                  when 's' => Dry_Run := True;
                  when 'R' => Recursive := True;
                  when others =>
                     Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "rpl: unsupported option -" & A (J));
                     Set_Exit_Status (Failure);
                     return;
               end case;
            end loop;
            I := I + 1;
         else
            exit;
         end if;
      end;
   end loop;

   for J in I .. Argument_Count loop
      Items.Append (To_Unbounded_String (Argument (J)));
   end loop;

   if Items.Length < 3 then
      Usage;
      Set_Exit_Status (Failure);
      return;
   end if;

   declare
      Old_Text : constant String := To_String (Items (1));
      New_Text : constant String := To_String (Items (2));
   begin
      for N in 3 .. Positive (Items.Length) loop
         Process_Target (To_String (Items (N)), Old_Text, New_Text);
      end loop;
   end;
end Rpl;