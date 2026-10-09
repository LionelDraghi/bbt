-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Util.Streams.Raw;

private package BBT.Pump is

   type Stream_State is (No_Data, Data_Available, End_Of_Stream);
   --  The state of one watched command output stream after a wait:
   --  nothing came (No_Data), data is available (Data_Available), or
   --  the stream is closed for good, with nothing left to read
   --  (End_Of_Stream)

   type Watched_Stream is record
      Raw   : Util.Streams.Raw.Raw_Stream_Access := null;
      State : Stream_State := No_Data;
   end record;

   type Watched_Array is array (Positive range <>) of Watched_Stream;

   --  Wait until one of the streams has data available, or reaches its
   --  end, at most Timeout seconds. A zero timeout only checks the
   --  current states, without waiting.
   --  The end of stream is discovered during the read on some
   --  platforms (the watch only reports the data available), in which
   --  case End_Of_Stream is never returned, and the caller detects it
   --  when draining: only read a stream after Data_Available, never
   --  after No_Data, the reads being blocking.
   procedure Wait (Streams : in out Watched_Array;
                   Timeout : in Duration);

   --  Tell whether data is already available on the stream, without
   --  waiting, so that the caller can drain it without blocking.
   function Has_Data (Stream : Watched_Stream) return Boolean;

end BBT.Pump;
