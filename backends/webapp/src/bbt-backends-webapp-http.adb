-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Strings.Fixed;

with GNAT.Sockets;

package body BBT.Backends.Webapp.Http is

   use Ada.Strings.Unbounded;

   CRLF : constant String :=
     Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;

   function Img (Value : Natural) return String is
      Text : constant String := Natural'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Img;

   function Header_End (Raw : String) return Natural is
      --  Index of the last character of the blank line ending the
      --  response head, 0 when the head is incomplete
      Pos : constant Natural := Ada.Strings.Fixed.Index (Raw, CRLF & CRLF);
   begin
      if Pos = 0 then
         return 0;
      end if;
      return Pos + CRLF'Length + 1;
   end Header_End;

   function Header_Value (Head : String; Name : String) return String is
      --  The value of the named head header, "" when absent. The name
      --  is matched at a line start, case sensitively: the drivers
      --  send the canonical spelling.
      Pattern : constant String := Name & ":";
      From    : Natural := Head'First;
   begin
      loop
         declare
            Pos : constant Natural :=
              Ada.Strings.Fixed.Index (Head (From .. Head'Last), Pattern);
         begin
            exit when Pos = 0;
            if Pos = Head'First
              or else Head (Pos - 1) = Ada.Characters.Latin_1.LF
            then
               declare
                  First : Natural := Pos + Pattern'Length;
                  Last  : Natural := Head'Last;
               begin
                  while First <= Head'Last
                    and then Head (First) in
                      ' ' | Ada.Characters.Latin_1.HT
                  loop
                     First := First + 1;
                  end loop;
                  for I in First .. Head'Last loop
                     if Head (I) in Ada.Characters.Latin_1.CR
                                  | Ada.Characters.Latin_1.LF
                     then
                        Last := I - 1;
                        exit;
                     end if;
                  end loop;
                  if First > Last then
                     return "";
                  end if;
                  return Head (First .. Last);
               end;
            end if;
            From := Pos + Pattern'Length;
         end;
      end loop;
      return "";
   end Header_Value;

   function Complete (Text : String) return Boolean is
      --  The whole response is in: the head, then the body its framing
      --  announces (the Content-Length, or the null chunk). A driver
      --  keeps the connection open, so waiting for the peer to close
      --  would stall every request.
      Stop : constant Natural := Header_End (Text);
   begin
      if Stop = 0 then
         return False;
      end if;
      declare
         Head : constant String := Text (Text'First .. Stop);
      begin
         if Ada.Strings.Fixed.Index (Head, "chunked") /= 0 then
            return Text'Length > Stop - Text'First + 4
              and then Text (Text'Last - 4 .. Text'Last) = "0" & CRLF & CRLF;
         end if;
         declare
            Length_Text : constant String :=
                            Header_Value (Head, "Content-Length");
         begin
            if Length_Text = "" then
               --  delimited by the peer closing: the receive timeout
               --  ends the read
               return False;
            end if;
            return Text'Last - Stop >= Natural'Value (Length_Text);
         end;
      end;
   exception
      when Constraint_Error =>
         return False;
   end Complete;

   function Decoded_Chunked (Raw : String) return String is
      --  The chunked transfer decoding (RFC 7230 4.1): the chunk size
      --  in hexadecimal, then the chunk, until the null chunk
      Result : Unbounded_String;
      Cursor : Natural := Raw'First;
   begin
      loop
         exit when Cursor > Raw'Last;
         declare
            Line_End : constant Natural :=
              Ada.Strings.Fixed.Index (Raw (Cursor .. Raw'Last), CRLF);
            Size     : Natural := 0;
         begin
            if Line_End = 0 then
               exit;
            end if;
            for I in Cursor .. Line_End - 1 loop
               declare
                  C : constant Character := Raw (I);
               begin
                  exit when C in ' ' | ';';
                  if C in '0' .. '9' then
                     Size := Size * 16
                       + Character'Pos (C) - Character'Pos ('0');
                  elsif C in 'a' .. 'f' then
                     Size := Size * 16
                       + Character'Pos (C) - Character'Pos ('a') + 10;
                  elsif C in 'A' .. 'F' then
                     Size := Size * 16
                       + Character'Pos (C) - Character'Pos ('A') + 10;
                  end if;
               end;
            end loop;
            exit when Size = 0;
            Cursor := Line_End + CRLF'Length;
            if Cursor + Size - 1 > Raw'Last then
               --  truncated last chunk: keep what is there
               Append (Result, Raw (Cursor .. Raw'Last));
               exit;
            end if;
            Append (Result, Raw (Cursor .. Cursor + Size - 1));
            Cursor := Cursor + Size + CRLF'Length;
         end;
      end loop;
      return To_String (Result);
   end Decoded_Chunked;

   function Port_Open (Port : Natural) return Boolean is
      Socket : GNAT.Sockets.Socket_Type;
      Opened : Boolean := False;
   begin
      GNAT.Sockets.Create_Socket (Socket, GNAT.Sockets.Family_Inet,
                                  GNAT.Sockets.Socket_Stream);
      Opened := True;
      GNAT.Sockets.Connect_Socket
        (Socket, (GNAT.Sockets.Family_Inet,
                  GNAT.Sockets.Inet_Addr ("127.0.0.1"),
                  GNAT.Sockets.Port_Type (Port)));
      GNAT.Sockets.Close_Socket (Socket);
      return True;
   exception
      when GNAT.Sockets.Socket_Error =>
         if Opened then
            begin
               GNAT.Sockets.Close_Socket (Socket);
            exception
               when GNAT.Sockets.Socket_Error =>
                  null;
            end;
         end if;
         return False;
   end Port_Open;

   function Request
     (Port         : Natural;
      Method       : String;
      Path         : String;
      Content      : String := "";
      Content_Type : String := "") return Response
   is
      use Ada.Streams;

      Socket : GNAT.Sockets.Socket_Type;
      Opened : Boolean := False;
      Head   : Unbounded_String;
      Buffer : Stream_Element_Array (1 .. 8192);
      Last   : Stream_Element_Offset;
      Raw    : Unbounded_String;

      procedure Send_Text (Text : String) is
         Item : Stream_Element_Array
           (1 .. Stream_Element_Offset (Text'Length));
         Sent : Stream_Element_Offset;
         From : Stream_Element_Offset := 1;
      begin
         for I in Text'Range loop
            Item (Stream_Element_Offset (I - Text'First + 1)) :=
              Stream_Element (Character'Pos (Text (I)));
         end loop;
         while From <= Item'Last loop
            GNAT.Sockets.Send_Socket (Socket, Item (From .. Item'Last), Sent);
            exit when Sent < From;
            From := Sent + 1;
         end loop;
      end Send_Text;

   begin
      GNAT.Sockets.Create_Socket (Socket, GNAT.Sockets.Family_Inet,
                                  GNAT.Sockets.Socket_Stream);
      Opened := True;
      GNAT.Sockets.Set_Socket_Option
        (Socket, GNAT.Sockets.Socket_Level,
         (Name => GNAT.Sockets.Receive_Timeout, Timeout => Receive_Timeout));
      GNAT.Sockets.Connect_Socket
        (Socket, (GNAT.Sockets.Family_Inet,
                  GNAT.Sockets.Inet_Addr ("127.0.0.1"),
                  GNAT.Sockets.Port_Type (Port)));

      Append (Head, Method & " " & Path & " HTTP/1.1" & CRLF);
      Append (Head, "Host: 127.0.0.1:" & Img (Port) & CRLF);
      Append (Head, "Connection: close" & CRLF);
      Append (Head, "Accept: application/json" & CRLF);
      if Content_Type /= "" then
         Append (Head, "Content-Type: " & Content_Type & CRLF);
      end if;
      if Method in "POST" | "PUT" | "PATCH" then
         Append (Head, "Content-Length: " & Img (Content'Length) & CRLF);
      end if;
      Append (Head, CRLF);
      Send_Text (To_String (Head));
      if Content /= "" then
         Send_Text (Content);
      end if;

      loop
         begin
            GNAT.Sockets.Receive_Socket (Socket, Buffer, Last);
         exception
            when GNAT.Sockets.Socket_Error =>
               --  The receive timeout: the answer is what has already
               --  arrived, the driver keeping the connection open
               exit when Complete (To_String (Raw));
               raise;
         end;
         exit when Last < Buffer'First;
         for I in Buffer'First .. Last loop
            Append (Raw, Character'Val (Buffer (I)));
         end loop;
         exit when Complete (To_String (Raw));
      end loop;

      GNAT.Sockets.Close_Socket (Socket);
      Opened := False;

      declare
         All_Text  : constant String := To_String (Raw);
         Head_Stop : constant Natural := Header_End (All_Text);
      begin
         if Head_Stop = 0 then
            raise Connection_Error
              with "malformed HTTP answer from the WebDriver";
         end if;

         declare
            Head_Text : constant String :=
              All_Text (All_Text'First .. Head_Stop);
            Space     : constant Natural :=
              Ada.Strings.Fixed.Index (Head_Text, " ");
            Status    : Natural := 0;
            Payload   : Unbounded_String;
         begin
            begin
               Status := Natural'Value
                 (Ada.Strings.Fixed.Trim
                    (Head_Text (Space + 1 .. Space + 3), Ada.Strings.Both));
            exception
               when Constraint_Error =>
                  raise Connection_Error
                    with "malformed HTTP status line from the WebDriver";
            end;

            if Head_Stop < All_Text'Last then
               if Ada.Strings.Fixed.Index (Head_Text, "chunked") /= 0 then
                  Append (Payload,
                          Decoded_Chunked
                            (All_Text (Head_Stop + 1 .. All_Text'Last)));
               else
                  declare
                     Length_Text : constant String :=
                       Header_Value (Head_Text, "Content-Length");
                     Length      : Natural :=
                       All_Text'Last - Head_Stop;
                  begin
                     if Length_Text /= "" then
                        begin
                           Length := Natural'Value (Length_Text);
                        exception
                           when Constraint_Error =>
                              null;
                        end;
                     end if;
                     Append
                       (Payload,
                        All_Text
                          (Head_Stop + 1 ..
                             Natural'Min (Head_Stop + Length, All_Text'Last)));
                  end;
               end if;
            end if;

            return (Status => Status, Payload => Payload);
         end;
      end;

   exception
      when Connection_Error =>
         if Opened then
            begin
               GNAT.Sockets.Close_Socket (Socket);
            exception
               when GNAT.Sockets.Socket_Error =>
                  null;
            end;
         end if;
         raise;
      when E : GNAT.Sockets.Socket_Error =>
         if Opened then
            begin
               GNAT.Sockets.Close_Socket (Socket);
            exception
               when GNAT.Sockets.Socket_Error =>
                  null;
            end;
         end if;
         raise Connection_Error
           with "cannot reach the WebDriver on port " & Img (Port)
             & ": " & Ada.Exceptions.Exception_Message (E);
   end Request;

end BBT.Backends.Webapp.Http;
