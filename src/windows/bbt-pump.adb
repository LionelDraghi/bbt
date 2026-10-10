-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Calendar;
with Interfaces.C;
with System;

with Util.Streams.Raw;

package body BBT.Pump is

   use type Interfaces.C.int;
   use type Interfaces.C.unsigned;

   -- -----------------------------------------------------------------------
   --  The command output and error descriptors are the read ends of
   --  anonymous pipes, that the wait functions do not support: the
   --  handles of CreatePipe are synchronous, and WaitForSingleObject
   --  on them has an undefined behavior. The canonical way of watching
   --  an anonymous pipe is PeekNamedPipe, a non blocking inquiry of the
   --  bytes available, completed by a small sleep in the waiting loop.

   subtype DWORD is Interfaces.C.unsigned;

   --  The windows HANDLE is defined as a void* in the C API,
   --  and the utilada File_Type is that handle.
   function Peek_Named_Pipe (Pipe      : in Interfaces.C.ptrdiff_t;
                             Buffer    : in System.Address;
                             Size      : in DWORD;
                             Read      : in System.Address;
                             Available : access DWORD;
                             Left      : in System.Address) return Interfaces.C.int;
   pragma Import (Stdcall, Peek_Named_Pipe, "PeekNamedPipe");

   procedure Sleep (Milliseconds : in DWORD);
   pragma Import (Stdcall, Sleep, "Sleep");

   function Get_Last_Error return Interfaces.C.int;
   pragma Import (Stdcall, Get_Last_Error, "GetLastError");

   Error_Broken_Pipe : constant := 109;
   --  the write end of the pipe is closed: the end of the stream

   Poll_Interval : constant Duration := 0.005;
   --  the sleep between two inquiries, as the pipe cannot be waited on

   -- -----------------------------------------------------------------------
   procedure Wait (Streams : in out Watched_Array;
                   Timeout : in Duration) is
      use type Ada.Calendar.Time;
      Deadline  : constant Ada.Calendar.Time := Ada.Calendar.Clock + Timeout;
      Available : aliased DWORD;
   begin
      loop
         for I in Streams'Range loop
            if Streams (I).State = No_Data then
               Available := 0;
               if Peek_Named_Pipe (Pipe      => Streams (I).Raw.Get_File,
                                   Buffer    => System.Null_Address,
                                   Size      => 0,
                                   Read      => System.Null_Address,
                                   Available => Available'Access,
                                   Left      => System.Null_Address) /= 0
               then
                  if Available > 0 then
                     Streams (I).State := Data_Available;
                  end if;
               elsif Get_Last_Error = Error_Broken_Pipe then
                  --  the write end is closed: nothing remains to read
                  Streams (I).State := End_Of_Stream;
               end if;
            end if;
         end loop;
         exit when (for some S of Streams => S.State /= No_Data)
           or else Timeout = 0.0
           or else Ada.Calendar.Clock >= Deadline;
         --  No stream changed: wait a moment before inquiring again
         delay Poll_Interval;
      end loop;
   end Wait;

   -- -----------------------------------------------------------------------
   function Has_Data (Stream : Watched_Stream) return Boolean is
      Available : aliased DWORD := 0;
   begin
      --  A non blocking check: data is already available
      return Peek_Named_Pipe (Pipe      => Stream.Raw.Get_File,
                              Buffer    => System.Null_Address,
                              Size      => 0,
                              Read      => System.Null_Address,
                              Available => Available'Access,
                              Left      => System.Null_Address) /= 0
        and then Available > 0;
   end Has_Data;

end BBT.Pump;
