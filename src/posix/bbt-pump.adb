-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Interfaces.C;

package body BBT.Pump is

   use type Interfaces.C.int;

   -- -----------------------------------------------------------------------
   --  Thin binding on the C poll() function, to watch the command output
   --  and error descriptors that the ada-util event model does not
   --  expose: the reads on the library streams are blocking

   --  poll() event bits: a modular type, as Ada has no bitwise
   --  operators on signed integers
   type Event_Bits is mod 2 ** 16;
   Pollin  : constant Event_Bits := 16#001#;
   Pollerr : constant Event_Bits := 16#008#;
   Pollhup : constant Event_Bits := 16#010#;
   Watched : constant Event_Bits := Pollin or Pollerr or Pollhup;

   type Poll_Fd is record
      Fd      : Interfaces.C.int;
      Events  : Interfaces.C.short;
      Revents : Interfaces.C.short;
   end record;
   pragma Convention (C, Poll_Fd);

   function Poll (Fds     : not null access Poll_Fd;
                  Nfds    : Interfaces.C.unsigned;
                  Timeout : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Poll, "poll");

   -- -----------------------------------------------------------------------
   procedure Wait (Streams : in out Watched_Array;
                   Timeout : in Duration) is
      Fds : array (Streams'Range) of aliased Poll_Fd;
   begin
      --  poll() tells which descriptors are readable, reached their end
      --  (POLLHUP) or are in error (POLLERR): the caller reads only the
      --  watched ones, the end of stream being discovered when the
      --  drained read returns nothing.
      for I in Streams'Range loop
         Fds (I) := (Fd      => Interfaces.C.int (Streams (I).Raw.Get_File),
                     Events  => Interfaces.C.short (Pollin),
                     Revents => 0);
      end loop;
      if Poll (Fds (Fds'First)'Access,
               Interfaces.C.unsigned (Streams'Length),
               Interfaces.C.int (Timeout * 1000.0)) <= 0
      then
         --  No stream changed during the timeout, or poll error
         return;
      end if;
      for I in Streams'Range loop
         if (Event_Bits (Fds (I).Revents) and Watched) /= 0 then
            Streams (I).State := Data_Available;
         end if;
      end loop;
   end Wait;

   -- -----------------------------------------------------------------------
   function Has_Data (Stream : Watched_Stream) return Boolean is
      Fd : aliased Poll_Fd :=
             (Fd      => Interfaces.C.int (Stream.Raw.Get_File),
              Events  => Interfaces.C.short (Pollin),
              Revents => 0);
   begin
      --  A non blocking check: data is already available
      return Poll (Fd'Access, 1, 0) > 0
        and then (Event_Bits (Fd.Revents) and Watched) /= 0;
   end Has_Data;

end BBT.Pump;
