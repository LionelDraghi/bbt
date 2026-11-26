-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

--  The minimal HTTP/1.1 client the WebDriver protocol needs: one
--  request per connection, on the loopback interface, with a bounded
--  receive timeout. No dependency is added to the crate for that: the
--  W3C WebDriver protocol is plain JSON over local HTTP, and the
--  backend crate stays self-contained (the same reason the gtk4
--  backend carries its own libatspi binding).
--
--  Only what the protocol uses is implemented: the status line, the
--  body, and the two framing headers (Content-Length, chunked
--  transfer encoding), the latter because a driver is free to use it.

with Ada.Strings.Unbounded;

package BBT.Backends.Webapp.Http is

   Connection_Error : exception;
   --  Raised when the peer cannot be reached, or the dialogue breaks:
   --  the backend turns it into the "no such window" semantics when
   --  the page went away

   type Response is record
      Status : Natural := 0;
      Payload : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   --  Status is the HTTP status code, Payload the decoded response body

   Receive_Timeout : constant Duration := 10.0;
   --  No request may hang the harness: a WebDriver call either
   --  answers quickly or the driver is broken

   function Request
     (Port         : Natural;
      Method       : String;
      Path         : String;
      Content      : String := "";
      Content_Type : String := "") return Response;
   --  Sends one request to 127.0.0.1:Port and returns the response.
   --  Raises Connection_Error when the server is unreachable.

   function Port_Open (Port : Natural) return Boolean;
   --  True when something listens on 127.0.0.1:Port

end BBT.Backends.Webapp.Http;
