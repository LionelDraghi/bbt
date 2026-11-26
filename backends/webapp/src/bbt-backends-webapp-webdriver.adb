-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Environment_Variables;

with BBT.Backends.Webapp.Dom;
with BBT.Backends.Webapp.Http;

package body BBT.Backends.Webapp.WebDriver is

   use Ada.Strings.Unbounded;
   use type GNAT.OS_Lib.Process_Id;
   use type GNAT.OS_Lib.String_Access;
   --  Both are private: the invalid-pid and the null-path comparisons
   --  need their operators directly visible (the Unbounded_String "="
   --  of the use clause above hides the predefined ones)

   Element_Key : constant String := "element-6066-11e4-a52e-4f735466cecf";
   --  The W3C element reference key of the protocol

   Driver_First_Port : constant := 9_515;
   Driver_Last_Port  : constant := 9_595;
   --  The loopback port range the backend looks for a free driver port

   Start_Deadline : constant Duration := 15.0;
   --  The driver gets this long to answer on its port: chromedriver
   --  starts its HTTP server before it is ready to spawn a browser

   function Img (Value : Natural) return String is
      Text : constant String := Natural'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Img;

   function Env (Name : String; Default : String := "") return String is
   begin
      if Ada.Environment_Variables.Exists (Name) then
         return Ada.Environment_Variables.Value (Name);
      end if;
      return Default;
   end Env;

   function Located (Program : String) return String is
      --  The absolute path of Program found in the PATH, "" when absent
      Found : GNAT.OS_Lib.String_Access :=
                GNAT.OS_Lib.Locate_Exec_On_Path (Program);
   begin
      if Found = null then
         return "";
      end if;
      declare
         Path : constant String := Found.all;
      begin
         GNAT.OS_Lib.Free (Found);
         return Path;
      end;
   end Located;

   function First_Browser return String is
      --  The probe order: the Chromium package names first (Debian,
      --  then the others), the Google Chrome binaries last
      Path : Unbounded_String := To_Unbounded_String (Located ("chromium"));
   begin
      if Path = Null_Unbounded_String then
         Path := To_Unbounded_String (Located ("chromium-browser"));
      end if;
      if Path = Null_Unbounded_String then
         Path := To_Unbounded_String (Located ("google-chrome"));
      end if;
      if Path = Null_Unbounded_String then
         Path := To_Unbounded_String (Located ("chrome"));
      end if;
      return To_String (Path);
   end First_Browser;

   function From_Environment return Options is
   begin
      return (Driver_Path  => To_Unbounded_String
                (Env ("BBT_WEBAPP_DRIVER", "chromedriver")),
              Browser_Path => To_Unbounded_String
                (Env ("BBT_WEBAPP_BROWSER", First_Browser)),
              Browser_Args => To_Unbounded_String
                (Env ("BBT_WEBAPP_BROWSER_ARGS")),
              Headless     => Env ("BBT_WEBAPP_HEADLESS", "1") /= "0");
   end From_Environment;

   --  JSON ---------------------------------------------------------------------
   --  The protocol answers are JSON documents; these helpers read the
   --  few members the backend needs (session id, element reference,
   --  value, message) and build the requests, without dragging a JSON
   --  library into the crate.

   function Json_Literal (Text : String) return String is
      Result : Unbounded_String := To_Unbounded_String ("""");
   begin
      for C of Text loop
         case C is
            when '"'  => Append (Result, "\""");
            when '\'  => Append (Result, "\\");
            when Ada.Characters.Latin_1.LF => Append (Result, "\n");
            when Ada.Characters.Latin_1.CR => Append (Result, "\r");
            when Ada.Characters.Latin_1.HT => Append (Result, "\t");
            when others => Append (Result, C);
         end case;
      end loop;
      Append (Result, """");
      return To_String (Result);
   end Json_Literal;

   procedure Append_UTF_8 (Result : in out Unbounded_String; Code : Natural);

   procedure Append_UTF_8 (Result : in out Unbounded_String; Code : Natural) is
      function Byte (Offset : Natural) return Character is
        (Character'Val ((Code / 64 ** (3 - Offset)) mod 64 + 16#80#));
   begin
      if Code < 16#80# then
         Append (Result, Character'Val (Code));
      elsif Code < 16#800# then
         Append (Result, Character'Val (16#C0# + Code / 64));
         Append (Result, Byte (3));
      elsif Code < 16#1_0000# then
         Append (Result, Character'Val (16#E0# + Code / 4096));
         Append (Result, Byte (2));
         Append (Result, Byte (3));
      else
         Append (Result, Character'Val (16#F0# + Code / 262_144));
         Append (Result, Byte (1));
         Append (Result, Byte (2));
         Append (Result, Byte (3));
      end if;
   end Append_UTF_8;

   function Decoded (Raw : String) return String is
      --  The JSON string content Raw, its escapes decoded
      Result : Unbounded_String;
      I      : Integer := Raw'First;

      function Hex (C : Character) return Natural is
        (if    C in '0' .. '9' then Character'Pos (C) - Character'Pos ('0')
         elsif C in 'a' .. 'f' then Character'Pos (C) - Character'Pos ('a') + 10
         elsif C in 'A' .. 'F' then Character'Pos (C) - Character'Pos ('A') + 10
         else 0);

   begin
      while I <= Raw'Last loop
         if Raw (I) = '\' and then I < Raw'Last then
            case Raw (I + 1) is
               when '"'  =>
                  Append (Result, '"');
                  I := I + 2;
               when '\'  =>
                  Append (Result, '\');
                  I := I + 2;
               when '/'  =>
                  Append (Result, '/');
                  I := I + 2;
               when 'b'  =>
                  Append (Result, Ada.Characters.Latin_1.BS);
                  I := I + 2;
               when 'f'  =>
                  Append (Result, Ada.Characters.Latin_1.FF);
                  I := I + 2;
               when 'n'  =>
                  Append (Result, Ada.Characters.Latin_1.LF);
                  I := I + 2;
               when 'r'  =>
                  Append (Result, Ada.Characters.Latin_1.CR);
                  I := I + 2;
               when 't'  =>
                  Append (Result, Ada.Characters.Latin_1.HT);
                  I := I + 2;
               when 'u'  =>
                  declare
                     Code : Natural := 0;
                  begin
                     if I + 5 <= Raw'Last then
                        for K in I + 2 .. I + 5 loop
                           Code := Code * 16 + Hex (Raw (K));
                        end loop;
                        I := I + 6;
                        --  a surrogate pair carries a code point above
                        --  the basic plane in two escapes
                        if Code in 16#D800# .. 16#DBFF#
                          and then I + 5 <= Raw'Last
                          and then Raw (I) = '\'
                          and then Raw (I + 1) = 'u'
                        then
                           declare
                              Low : Natural := 0;
                           begin
                              for K in I + 2 .. I + 5 loop
                                 Low := Low * 16 + Hex (Raw (K));
                              end loop;
                              if Low in 16#DC00# .. 16#DFFF# then
                                 Code := 16#1_0000#
                                   + (Code - 16#D800#) * 16#400#
                                   + (Low - 16#DC00#);
                                 I := I + 6;
                              end if;
                           end;
                        end if;
                        Append_UTF_8 (Result, Code);
                     else
                        I := Raw'Last + 1;
                     end if;
                  end;
               when others =>
                  Append (Result, Raw (I));
                  I := I + 1;
            end case;
         else
            Append (Result, Raw (I));
            I := I + 1;
         end if;
      end loop;
      return To_String (Result);
   end Decoded;

   function Value_At (Json : String; From : Integer) return String is
      --  The JSON value starting at From: a string decoded, or the
      --  literal (number, true/false/null) as it is written
      I : Integer := From;
   begin
      while I <= Json'Last
        and then Json (I) in ' ' | Ada.Characters.Latin_1.HT
                             | Ada.Characters.Latin_1.CR
                             | Ada.Characters.Latin_1.LF
      loop
         I := I + 1;
      end loop;
      if I > Json'Last then
         return "";
      end if;
      if Json (I) = '"' then
         declare
            Start : constant Integer := I + 1;
            J     : Integer := Start;
         begin
            while J <= Json'Last loop
               if Json (J) = '\' then
                  J := J + 2;
               elsif Json (J) = '"' then
                  exit;
               else
                  J := J + 1;
               end if;
            end loop;
            if J > Json'Last then
               return "";
            end if;
            return Decoded (Json (Start .. J - 1));
         end;
      else
         declare
            J : Integer := I;
         begin
            while J <= Json'Last
              and then Json (J) not in ',' | '}' | ']' | ' '
                                    | Ada.Characters.Latin_1.HT
                                    | Ada.Characters.Latin_1.CR
                                    | Ada.Characters.Latin_1.LF
            loop
               J := J + 1;
            end loop;
            return Json (I .. J - 1);
         end;
      end if;
   end Value_At;

   function Member
     (Json : String; Key : String; Found : out Boolean) return String;
   --  The value of the first Json member named Key

   function Member
     (Json : String; Key : String; Found : out Boolean) return String
   is
      I : Integer := Json'First;
   begin
      Found := False;
      while I <= Json'Last loop
         if Json (I) = '"' then
            declare
               Start : constant Integer := I + 1;
               J     : Integer := Start;
               After : Integer;
            begin
               while J <= Json'Last loop
                  if Json (J) = '\' then
                     J := J + 2;
                  elsif Json (J) = '"' then
                     exit;
                  else
                     J := J + 1;
                  end if;
               end loop;
               After := J + 1;
               while After <= Json'Last
                 and then Json (After) in ' ' | Ada.Characters.Latin_1.HT
                                         | Ada.Characters.Latin_1.CR
                                         | Ada.Characters.Latin_1.LF
               loop
                  After := After + 1;
               end loop;
               if After <= Json'Last and then Json (After) = ':' then
                  if Decoded (Json (Start .. J - 1)) = Key then
                     Found := True;
                     return Value_At (Json, After + 1);
                  end if;
                  I := After + 1;
               else
                  I := J + 1;
               end if;
            end;
         else
            I := I + 1;
         end if;
      end loop;
      return "";
   end Member;

   function Member (Json : String; Key : String) return String is
      Found : Boolean;
      Value : constant String := Member (Json, Key, Found);
   begin
      return (if Found then Value else "");
   end Member;

   --  The protocol --------------------------------------------------------------

   function Session_Path (S : Session; Path : String) return String is
   begin
      if S.Id = Null_Unbounded_String then
         return Path;
      end if;
      return "/session/" & To_String (S.Id) & Path;
   end Session_Path;

   function Call
     (S      : Session;
      Method : String;
      Path   : String;
      Json   : String := "") return Http.Response is
   begin
      if S.Port = 0 then
         raise Connection_Error with "no WebDriver session";
      end if;
      return Http.Request (S.Port, Method, Session_Path (S, Path), Json,
                           Content_Type => "application/json");
   exception
      when Http.Connection_Error =>
         raise Connection_Error
           with "the WebDriver on port " & Img (S.Port) & " is gone";
   end Call;

   procedure Check_OK (Reply : Http.Response; What : String) is
      Message : constant String := Member (To_String (Reply.Payload), "message");
      Error   : constant String := Member (To_String (Reply.Payload), "error");
   begin
      if Reply.Status in 200 .. 299 then
         return;
      end if;
      if Error in "no such window" | "invalid session id" then
         --  The page is gone: the window semantics (G095), not a
         --  protocol failure.
         raise Connection_Error with What & ": " & Error;
      end if;
      raise Protocol_Error
        with What & " (" & Img (Reply.Status) & "): "
          & (if Message /= "" then Message else Error);
   end Check_OK;

   --  The session ---------------------------------------------------------------

   function Active (S : Session) return Boolean is
     (S.Open);

   procedure Start (S : in out Session; Opt : Options) is
      Driver : constant String := Located (To_String (Opt.Driver_Path));
      Port   : Natural := Driver_First_Port;
      Args   : GNAT.OS_Lib.Argument_List (1 .. 1);
      Pid    : GNAT.OS_Lib.Process_Id;

      function Available_Port return Natural is
      begin
         for Candidate in Driver_First_Port .. Driver_Last_Port loop
            if not Http.Port_Open (Candidate) then
               return Candidate;
            end if;
         end loop;
         return 0;
      end Available_Port;

      function Capabilities return String is
         Args_Json : Unbounded_String;
         procedure Add (Item : String) is
         begin
            if Args_Json /= Null_Unbounded_String then
               Append (Args_Json, ",");
            end if;
            Append (Args_Json, Json_Literal (Item));
         end Add;
         Extra : constant String := To_String (Opt.Browser_Args);
         From  : Natural := Extra'First;
      begin
         if Opt.Headless then
            Add ("--headless=new");
         end if;
         --  the extra arguments, blank separated
         while From <= Extra'Last loop
            if Extra (From) = ' ' then
               From := From + 1;
            else
               declare
                  Last : Natural := From;
               begin
                  while Last < Extra'Last and then Extra (Last + 1) /= ' ' loop
                     Last := Last + 1;
                  end loop;
                  Add (Extra (From .. Last));
                  From := Last + 1;
               end;
            end if;
         end loop;
         return "{""capabilities"":{""alwaysMatch"":{"
           & """browserName"":""chrome"","
           & """goog:chromeOptions"":{"
           & """binary"":" & Json_Literal (To_String (Opt.Browser_Path)) & ","
           & """args"":[" & To_String (Args_Json) & "]}}}}";
      end Capabilities;

      Reply : Http.Response;
   begin
      Stop (S);

      if Driver = "" then
         raise Protocol_Error
           with "no WebDriver driver found for "
             & To_String (Opt.Driver_Path)
             & " (install chromedriver, or set BBT_WEBAPP_DRIVER)";
      end if;
      if To_String (Opt.Browser_Path) = "" then
         raise Protocol_Error
           with "no browser found (install chromium or google-chrome,"
             & " or set BBT_WEBAPP_BROWSER)";
      end if;

      Port := Available_Port;
      if Port = 0 then
         raise Protocol_Error
           with "no free port for the WebDriver in "
             & Img (Driver_First_Port) & ".." & Img (Driver_Last_Port);
      end if;

      Args (1) := new String'("--port=" & Img (Port));
      Pid := GNAT.OS_Lib.Non_Blocking_Spawn (Driver, Args);
      if Pid = GNAT.OS_Lib.Invalid_Pid then
         raise Protocol_Error with "cannot start " & Driver;
      end if;
      S.Driver_Pid := Pid;
      S.Port := Port;

      --  the driver answers on its port once its server runs
      declare
         Attempts : constant Natural :=
                      Natural (Start_Deadline / 0.1);
         Up       : Boolean := False;
      begin
         for Attempt in 1 .. Attempts loop
            if Http.Port_Open (Port) then
               Up := True;
               exit;
            end if;
            delay 0.1;
         end loop;
         if not Up then
            Stop (S);
            raise Protocol_Error
              with "the WebDriver did not start on port " & Img (Port);
         end if;
      end;

      Reply := Http.Request
        (Port, "POST", "/session", Capabilities,
         Content_Type => "application/json");
      if Reply.Status not in 200 .. 299 then
         declare
            Message : constant String :=
              Member (To_String (Reply.Payload), "message");
         begin
            Stop (S);
            raise Protocol_Error
              with "cannot open the browser session: " & Message;
         end;
      end if;

      S.Id := To_Unbounded_String (Member (To_String (Reply.Payload), "sessionId"));
      if S.Id = Null_Unbounded_String then
         Stop (S);
         raise Protocol_Error
           with "the WebDriver opened no session (no sessionId)";
      end if;
      S.Open := True;
   end Start;

   procedure Stop (S : in out Session) is
   begin
      if S.Open and then S.Port /= 0 then
         begin
            declare
               Reply : constant Http.Response :=
                 Call (S, "DELETE", "");
               pragma Unreferenced (Reply);
            begin
               null;
            end;
         exception
            when Connection_Error | Protocol_Error =>
               null;
         end;
      end if;
      if S.Driver_Pid /= GNAT.OS_Lib.Invalid_Pid then
         GNAT.OS_Lib.Kill (S.Driver_Pid);  --  SIGKILL: the session is over
      end if;
      S.Open       := False;
      S.Port       := 0;
      S.Id         := Null_Unbounded_String;
      S.Driver_Pid := GNAT.OS_Lib.Invalid_Pid;
   end Stop;

   --  The page ------------------------------------------------------------------

   procedure Navigate (S : in out Session; URL : String) is
      Reply : constant Http.Response :=
        Call (S, "POST", "/url", "{""url"":" & Json_Literal (URL) & "}");
   begin
      Check_OK (Reply, "navigating to " & URL);
   end Navigate;

   function Execute_String (S : in out Session; Script : String) return String is
      Reply : constant Http.Response :=
        Call (S, "POST", "/execute/sync",
              "{""script"":" & Json_Literal (Script) & ",""args"":[]}");
   begin
      Check_OK (Reply, "running a script");
      return Member (To_String (Reply.Payload), "value");
   end Execute_String;

   function Execute_Bool (S : in out Session; Script : String) return Boolean is
     (Execute_String (S, Script) = "true");

   function Execute_Number (S : in out Session; Script : String) return Natural
   is
      Value : constant String := Execute_String (S, Script);
   begin
      if Value = "" then
         return 0;
      end if;
      return Natural'Value (Value);
   exception
      when Constraint_Error =>
         return 0;
   end Execute_Number;

   function Title (S : in out Session) return String is
      Reply : constant Http.Response := Call (S, "GET", "/title");
   begin
      Check_OK (Reply, "reading the window title");
      return Member (To_String (Reply.Payload), "value");
   end Title;

   function Ready (S : in out Session) return Boolean is
     (Execute_Bool (S, Dom.Ready_Script));

   procedure Close_Window (S : in out Session) is
      Reply : constant Http.Response := Call (S, "DELETE", "/window");
   begin
      Check_OK (Reply, "closing the window");
   end Close_Window;

   function Find (S : in out Session; Selector : String) return String is
      Reply : constant Http.Response :=
        Call (S, "POST", "/element",
              "{""using"":""css selector"",""value"":"
              & Json_Literal (Selector) & "}");
   begin
      if Reply.Status = 404 then
         return "";
      end if;
      Check_OK (Reply, "looking up " & Selector);
      return Member (To_String (Reply.Payload), Element_Key);
   end Find;

   procedure Click (S : in out Session; Element : String) is
      Reply : constant Http.Response :=
        Call (S, "POST", "/element/" & Element & "/click", "{}");
   begin
      Check_OK (Reply, "pressing the element");
   end Click;

   procedure Send_Return (S : in out Session; Element : String) is
      --  "\ue007" is the WebDriver code point of the Return key
      Reply : constant Http.Response :=
        Call (S, "POST", "/element/" & Element & "/value",
              "{""text"":""\ue007""}");
   begin
      Check_OK (Reply, "sending the Return key");
   end Send_Return;

end BBT.Backends.Webapp.WebDriver;
