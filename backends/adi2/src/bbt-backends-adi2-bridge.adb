-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.OS_Lib;

package body BBT.Backends.Adi2.Bridge is

   use Ada.Strings.Fixed;

   --  The bridge answers commands inside the application's frame loop,
   --  so an alive application answers in tens of milliseconds; these
   --  are ceilings that still fail fast for the failures they name.
   Launch_Deadline : constant Duration := 15.0;
   Command_Deadline : constant Duration := 10.0;

   Spawned_Id : GNAT.OS_Lib.Process_Id;
   Spawned    : Boolean := False;
   Session    : Unbounded_String := Null_Unbounded_String;
   Requests   : Natural := 0;

   use type Ada.Calendar.Time;

   function Session_Base_Dir return String is
      use Ada.Environment_Variables;
   begin
      if Exists ("TMPDIR") then
         return Value ("TMPDIR");
      elsif Exists ("TEMP") then
         return Value ("TEMP");
      elsif Exists ("TMP") then
         return Value ("TMP");
      else
         return "/tmp";
      end if;
   end Session_Base_Dir;

   function Session_Open return Boolean is
     (Session /= Null_Unbounded_String);

   procedure Launch (Binary_Path : String) is
      Base : constant String := Session_Base_Dir & "/adi_mcp";
   begin
      if Spawned then
         Shutdown;
      end if;

      if not Ada.Directories.Exists (Binary_Path) then
         raise Launch_Failed
           with "application binary not found: " & Binary_Path;
      end if;

      Spawned_Id :=
        GNAT.OS_Lib.Non_Blocking_Spawn (Binary_Path, [1 .. 0 => null]);
      Spawned := True;

      declare
         --  The session is named by the spawned process id: the runner
         --  spawns the application binary directly, so that id IS the
         --  application's, and the bridge namespaces its session by it.
         --  No discovery, no guessing.
         Pid : constant String :=
           Trim (GNAT.OS_Lib.Pid_To_Integer (Spawned_Id)'Image,
                 Ada.Strings.Both);
         Dir : constant String := Base & "/" & Pid;
         Deadline : constant Ada.Calendar.Time :=
           Ada.Calendar.Clock + Launch_Deadline;
      begin
         loop
            if Ada.Directories.Exists (Dir & "/ready") then
               Session := To_Unbounded_String (Dir);
               return;
            end if;
            exit when Ada.Calendar.Clock > Deadline;
            delay 0.05;
         end loop;
      end;

      Shutdown;
      raise Launch_Failed
        with "no Adi.MCP session appeared under " & Base & "/<pid> within"
             & Integer'Image (Integer (Launch_Deadline)) & " s: is "
             & Binary_Path
             & " a development-profile build (Adi2's bridge is compiled"
             & " in that profile only)?";
   end Launch;

   procedure Reap is
      --  A killed child stays a zombie until it is waited for: the
      --  same reap the other backends do after their Kill
      Status  : GNAT.OS_Lib.Process_Id;
      Success : Boolean;
   begin
      if Spawned then
         --  The any-child form, as the other backends reap: one
         --  launched application at a time in this harness
         GNAT.OS_Lib.Wait_Process (Status, Success);
      end if;
   end Reap;

   procedure Shutdown is
   begin
      if Spawned then
         GNAT.OS_Lib.Kill (Spawned_Id);
      end if;
      Reap;
      Spawned := False;
      Session := Null_Unbounded_String;
   end Shutdown;

   function Alive return Boolean is
     (Spawned
      and then Session /= Null_Unbounded_String
      and then Ada.Directories.Exists (To_String (Session)));
   --  The bridge removes its session directory when the application
   --  finalizes (Adi.MCP.Finalize, on the ordinary shutdown path): a
   --  gone directory is an exited application, as the bridge's own
   --  documentation describes. No process probing, so no platform
   --  specific process API here.

   procedure Send (Command, Parameters : String) is
      Answer : constant String := Run_Command (Command, Parameters);
      pragma Unreferenced (Answer);
   begin
      null;
   end Send;

   function Quit (Deadline : Duration) return Boolean is
      --  The pinned bridge commit implements "quit"; its own
      --  documentation advertises "quit_app". Ask for both, then wait:
      --  an application whose close handler refuses stays alive, and
      --  the caller's process-level close takes over.
      Deadline_At : constant Ada.Calendar.Time :=
        Ada.Calendar.Clock + Deadline;
   begin
      if not Alive then
         Session := Null_Unbounded_String;
         return True;
      end if;
      Send ("quit_app", "");
      Send ("quit", "");
      loop
         if not Alive then
            Session := Null_Unbounded_String;
            Reap;
            Spawned := False;
            return True;
         end if;
         exit when Ada.Calendar.Clock > Deadline_At;
         delay 0.05;
      end loop;
      return False;
   end Quit;

   function Run_Command (Command, Parameters : String) return String
   is
      use Ada.Text_IO;
      Id : constant String := Trim (Requests'Image, Ada.Strings.Both);
      Cmd_File : constant String :=
        To_String (Session) & "/cmd_" & Id & ".json";
      Resp_File : constant String :=
        To_String (Session) & "/resp_" & Id & ".json";
      File : File_Type;
      Opened : Boolean := False;
   begin
      if Session = Null_Unbounded_String then
         return "";
      end if;
      Requests := Requests + 1;

      if Ada.Directories.Exists (Resp_File) then
         Ada.Directories.Delete_File (Resp_File);
      end if;

      Create (File, Out_File, Cmd_File);
      Put (File, "{" & '"' & "command" & '"' & ":" & '"' & Command & '"');
      if Parameters'Length > 0 then
         Put (File, "," & Parameters);
      end if;
      Put (File, "}");
      Close (File);

      declare
         Deadline : constant Ada.Calendar.Time :=
           Ada.Calendar.Clock + Command_Deadline;
         Answer : Unbounded_String := Null_Unbounded_String;
      begin
         loop
            if Ada.Directories.Exists (Resp_File) then
               Open (File, In_File, Resp_File);
               Opened := True;
               while not End_Of_File (File) loop
                  Append (Answer, Get_Line (File));
               end loop;
               Close (File);
               Opened := False;
               return To_String (Answer);
            end if;
            exit when Ada.Calendar.Clock > Deadline;
            delay 0.01;
         end loop;
      end;
      return "";
   exception
      when others =>
         if Opened and then Is_Open (File) then
            Close (File);
         end if;
         return "";
   end Run_Command;

   function Widget_Tree return String is
     (Run_Command ("widget_tree", ""));

   function Ok (Response : String) return Boolean is
     (Index (Response, '"' & "status" & '"' & ":" & '"' & "ok" & '"') /= 0);

   --  Reads --------------------------------------------------------------

   function Member_Value (Object, Key : String) return String is
      Pattern : constant String := '"' & Key & '"' & ":" & '"';
      From    : constant Natural := Index (Object, Pattern);
   begin
      if From = 0 then
         return "";
      end if;
      declare
         First : constant Positive := From + Pattern'Length;
         Last  : Natural := First;
      begin
         while Last <= Object'Last
           and then not (Object (Last) = '"' and then Object (Last - 1) /= '\')
         loop
            Last := Last + 1;
         end loop;
         if Last > Object'Last then
            return "";
         end if;
         return Object (First .. Last - 1);
      end;
   end Member_Value;

   function Member_Number (Object, Key : String) return Natural is
      Pattern : constant String := '"' & Key & '"' & ":";
      From    : constant Natural := Index (Object, Pattern);
   begin
      if From = 0 then
         return 0;
      end if;
      declare
         First : constant Positive := From + Pattern'Length;
         Last  : Natural := First;
      begin
         while Last <= Object'Last and then Object (Last) in '0' .. '9' loop
            Last := Last + 1;
         end loop;
         if Last = First then
            return 0;
         end if;
         return Natural'Value (Object (First .. Last - 1));
      end;
   exception
      when Constraint_Error =>
         return 0;
   end Member_Number;

   function Member_Boolean (Object, Key : String) return Natural is
      Pattern : constant String := '"' & Key & '"' & ":";
      From    : constant Natural := Index (Object, Pattern);
   begin
      if From = 0 then
         return 0;
      end if;
      declare
         Pos : constant Natural := From + Pattern'Length;
      begin
         if Pos > Object'Last then
            return 0;
         end if;
         return (if Object (Pos) = 't' then 2 else 1);
      end;
   end Member_Boolean;

   function Contains (Text, Pattern : String) return Boolean is
      use Ada.Characters.Handling;
   begin
      return Pattern'Length = 0
        or else Index (To_Lower (Text), To_Lower (Pattern)) /= 0;
   end Contains;

   --  Snapshot ------------------------------------------------------------

   function Parse (Response : String) return Node_Vectors.Vector is
      Nodes : Node_Vectors.Vector;
      Tree_Open : constant Natural := Index (Response, '"' & "tree" & '"');
      Overlays  : constant Natural := Index (Response, '"' & "overlays" & '"');
      Stop : constant Natural :=
        (if Overlays = 0 then Response'Last else Overlays - 1);

      --  The last index of the object's own members: a widget object
      --  ends its own members at its "children" key, or at its closing
      --  brace when it has none. A quote aware scan keeps the string
      --  values (paths, texts) from steering it.
      function Own_End (From : Positive) return Natural is
         I       : Natural := From;
         Depth   : Natural := 0;
         In_Str  : Boolean := False;
         Escaped : Boolean := False;
         Children : Natural := 0;
      begin
         while I <= Stop loop
            if In_Str then
               if Escaped then
                  Escaped := False;
               elsif Response (I) = '\' then
                  Escaped := True;
               elsif Response (I) = '"' then
                  In_Str := False;
               end if;
            else
               case Response (I) is
                  when '"' =>
                     In_Str := True;
                     if Children = 0 and then Depth = 1
                       and then I + 10 <= Stop
                       and then Response (I .. I + 9) = '"' & "children" & '"'
                     then
                        Children := I;
                     end if;
                  when '{' =>
                     Depth := Depth + 1;
                  when '}' =>
                     if Depth = 0 then
                        return I - 1;
                     end if;
                     Depth := Depth - 1;
                     if Depth = 0 then
                        return (if Children = 0 then I - 1 else Children - 1);
                     end if;
                  when others =>
                     null;
               end case;
            end if;
            I := I + 1;
         end loop;
         return Stop;
      end Own_End;

      Start : Natural := Tree_Open;
      Open : Natural;
   begin
      if Tree_Open = 0 then
         return Nodes;
      end if;
      loop
         exit when Start >= Stop;
         Open := Index (Response (Start .. Stop), "{" & '"' & "type" & '"');
         exit when Open = 0;
         declare
            Own : constant String :=
              Response (Open .. Own_End (Open));
            States_First : constant Natural :=
              Index (Own, '"' & "states" & '"');
            Square_Open : constant Natural :=
              (if States_First = 0
               then 0
               else Index (Own (States_First .. Own'Last), "["));
            Square_Close : constant Natural :=
              (if Square_Open = 0
               then 0
               else Index (Own (Square_Open .. Own'Last), "]"));
            States : constant String :=
              (if Square_Open = 0 or else Square_Close = 0
               then ""
               else Own (Square_Open .. Square_Close));
         begin
            Nodes.Append
              (Node'
                 (Path        => To_Unbounded_String
                                   (Member_Value (Own, "path")),
                  Kind        => To_Unbounded_String
                                   (Member_Value (Own, "type")),
                  Id          => Member_Number (Own, "id"),
                  Text        => To_Unbounded_String
                                   (Member_Value (Own, "text")),
                  Visible     => Member_Boolean (Own, "visible") = 2,
                  Disabled    => Index (States, "disabled") /= 0,
                  Selected    => Index (States, "selected") /= 0,
                  Child_Count => Member_Number (Own, "child_count")));
         end;
         Start := Open + 1;
      end loop;
      return Nodes;
   end Parse;

   function Find_By_Path
     (Nodes : Node_Vectors.Vector; Path : String) return Natural is
   begin
      for I in Nodes.First_Index .. Nodes.Last_Index loop
         if To_String (Nodes.Element (I).Path) = Path then
            return I;
         end if;
      end loop;
      return 0;
   end Find_By_Path;

   function Parent_Path (Path : String) return String is
      Dot : constant Natural := Index (Path, ".", Ada.Strings.Backward);
   begin
      if Path'Length = 0 or else Dot = 0 then
         return "";
      end if;
      return Path (Path'First .. Dot - 1);
   end Parent_Path;

   function Is_Descendant (Path, Ancestor : String) return Boolean is
   begin
      if Ancestor'Length = 0 then
         return Path'Length > 0;
      end if;
      return Path'Length > Ancestor'Length
        and then Path (Path'First .. Path'First + Ancestor'Length - 1)
                 = Ancestor
        and then Path (Path'First + Ancestor'Length) = '.';
   end Is_Descendant;

   function Shown
     (Nodes : Node_Vectors.Vector; Index : Natural) return Boolean
   is
      Path : Unbounded_String;
   begin
      if Index = 0 or else Index > Nodes.Last_Index then
         return False;
      end if;
      if not Nodes.Element (Index).Visible then
         return False;
      end if;
      Path := Nodes.Element (Index).Path;
      loop
         Path := To_Unbounded_String (Parent_Path (To_String (Path)));
         exit when Path = Null_Unbounded_String;
         declare
            Parent : constant Natural :=
              Find_By_Path (Nodes, To_String (Path));
         begin
            exit when Parent = 0;
            if not Nodes.Element (Parent).Visible then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Shown;

end BBT.Backends.Adi2.Bridge;
