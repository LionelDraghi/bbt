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

   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Unbounded_String);

   Quiet       : Boolean := False;
   Verbose     : Boolean := False;
   Dry_Run     : Boolean := False;
   Backup      : Boolean := False;
   Ignore_Case : Boolean := False;
   Whole_Words : Boolean := False;
   Escape      : Boolean := False;
   Recursive   : Boolean := False;

   Suffixes : String_Vectors.Vector;
   Items    : String_Vectors.Vector;

   procedure Usage is
   begin
      Ada.Text_IO.Put_Line ("usage: rpl [options] OLD_TEXT NEW_TEXT FILE [FILE ...]");
      Ada.Text_IO.Put_Line ("Search and replace OLD_TEXT by NEW_TEXT in files.");
      Ada.Text_IO.Put_Line ("Implemented options: -h --help --version --encoding ENC");
      Ada.Text_IO.Put_Line ("                     -i -w -b -q -v -s -e -R -xSUFFIX -x SUFFIX");
      Ada.Text_IO.Put_Line ("Accepted no-op options for compatibility: -f -d -a -p -L");
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

   function Expand_Escapes (S : String) return String is
      R : Unbounded_String;
      I : Natural := S'First;

      function Hex_Value (C : Character) return Natural is
      begin
         if C in '0' .. '9' then
            return Character'Pos (C) - Character'Pos ('0');
         elsif C in 'a' .. 'f' then
            return 10 + Character'Pos (C) - Character'Pos ('a');
         elsif C in 'A' .. 'F' then
            return 10 + Character'Pos (C) - Character'Pos ('A');
         else
            return 0;
         end if;
      end Hex_Value;
   begin
      while I <= S'Last loop
         if S (I) = '\\' and then I < S'Last then
            I := I + 1;
            case S (I) is
               when 'n' => Append (R, Character'Val (10));
               when 'r' => Append (R, Character'Val (13));
               when 't' => Append (R, Character'Val (9));
               when '\\' => Append (R, '\\');
               when 'x' =>
                  if I + 2 <= S'Last then
                     Append (R, Character'Val (Hex_Value (S (I + 1)) * 16 + Hex_Value (S (I + 2))));
                     I := I + 2;
                  else
                     Append (R, 'x');
                  end if;
               when others => Append (R, S (I));
            end case;
         else
            Append (R, S (I));
         end if;
         I := I + 1;
      end loop;
      return To_String (R);
   end Expand_Escapes;

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

   function Suffix_Matches (Path : String) return Boolean is
   begin
      if Suffixes.Is_Empty then
         return True;
      end if;

      for S of Suffixes loop
         declare
            X : constant String := To_String (S);
         begin
            if Path'Length >= X'Length and then Path (Path'Last - X'Length + 1 .. Path'Last) = X then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Suffix_Matches;

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
      Temp_Path    : constant String := Path & ".tmp";
      Backup_Path  : constant String := Path & "~";
   begin
      if not Suffix_Matches (Path) then
         return;
      end if;

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
         if Backup then
            if Ada.Directories.Exists (Backup_Path) then
               Ada.Directories.Delete_File (Backup_Path);
            end if;
            Ada.Directories.Copy_File (Path, Backup_Path);
         end if;

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
      Search  : Ada.Directories.Search_Type;
      Entry   : Ada.Directories.Directory_Entry_Type;
      Pattern : constant String := "*";
   begin
      if Ada.Directories.Kind (Path) = Ada.Directories.Directory then
         if not Recursive then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "rpl: " & Path & " is a directory; use -R");
            Set_Exit_Status (Failure);
            return;
         end if;

         Ada.Directories.Start_Search
           (Search    => Search,
            Directory => Path,
            Pattern   => Pattern,
            Filter    => (Ada.Directories.Ordinary_File => True, Ada.Directories.Directory => True, others => False));

         while Ada.Directories.More_Entries (Search) loop
            Ada.Directories.Get_Next_Entry (Search, Entry);
            declare
               Full : constant String := Ada.Directories.Full_Name (Entry);
               Base : constant String := Ada.Directories.Simple_Name (Entry);
            begin
               if Base /= "." and then Base /= ".." then
                  if Ada.Directories.Kind (Entry) = Ada.Directories.Directory then
                     Process_Target (Full, Old_Text, New_Text);
                  else
                     Process_File (Full, Old_Text, New_Text);
                  end if;
               end if;
            end;
         end loop;
         Ada.Directories.End_Search (Search);
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
         elsif A = "--encoding" then
            I := I + 2;
         elsif A = "-x" then
            if I = Argument_Count then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "rpl: -x requires a suffix");
               Set_Exit_Status (Failure);
               return;
            end if;
            Suffixes.Append (To_Unbounded_String (Argument (I + 1)));
            I := I + 2;
         elsif A'Length > 2 and then A (A'First .. A'First + 1) = "-x" then
            Suffixes.Append (To_Unbounded_String (A (A'First + 2 .. A'Last)));
            I := I + 1;
         elsif A'Length > 0 and then A (A'First) = '-' then
            for J in A'First + 1 .. A'Last loop
               case A (J) is
                  when 'i' => Ignore_Case := True;
                  when 'w' => Whole_Words := True;
                  when 'b' => Backup := True;
                  when 'q' => Quiet := True;
                  when 'v' => Verbose := True;
                  when 's' => Dry_Run := True;
                  when 'e' => Escape := True;
                  when 'R' => Recursive := True;
                  when 'f' | 'd' | 'a' | 'p' | 'L' => null;
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
      Old_Text : constant String := (if Escape then Expand_Escapes (To_String (Items (1))) else To_String (Items (1)));
      New_Text : constant String := (if Escape then Expand_Escapes (To_String (Items (2))) else To_String (Items (2)));
   begin
      for N in 3 .. Positive (Items.Length) loop
         Process_Target (To_String (Items (N)), Old_Text, New_Text);
      end loop;
   end;
end Rpl;