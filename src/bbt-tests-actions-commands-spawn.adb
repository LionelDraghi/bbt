-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO;
with BBT.Pump;
with BBT.Settings;
with BBT.Terminal;

with Ada.Calendar;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Interfaces.C;

with Util.Processes,
     Util.Streams,
     Util.Streams.Raw;

use Ada, BBT, Interfaces.C;
use Util.Processes;

package body BBT.Tests.Actions.Commands.Spawn is

   procedure Put_Debug_Line
     (Item      : String;
      Location  : IO.Location_Type    := IO.No_Location;
      Verbosity : IO.Verbosity_Levels := IO.Debug;
      Topic     : IO.Extended_Topics  := IO.Step_Actions)
      renames IO.Put_Line;
   pragma Warnings (Off, Put_Debug_Line);

   -- --------------------------------------------------------------------------
   --  Execution of the commands, based on the ada-util library
   --  (Util.Processes). The standard output and standard error of the
   --  command are pumped into files by polling the descriptors, so that
   --  the output checks steps read the same files as before.
   --  When a scenario sends text to the command (Type_Text / Enter_Text
   --  steps), the command runs across steps on a pseudo terminal
   --  allocated by the library (raw mode: no echo, no canonical mode,
   --  no CR LF translation), and each input step resets the output
   --  baseline: the output checks following an input step apply only
   --  to the output produced after it.

   Running_Cmd : Boolean := False;
   --  the last command is still running (not yet reaped)

   Cmd_Output_Name : Unbounded_String;
   Cmd_Err_Name    : Unbounded_String;
   Error_Merged          : Boolean := True;
   --  the standard error goes to the output file unless an error output
   --  file was requested

   Output_Byte_Count : Natural := 0;
   Err_Byte_Count    : Natural := 0;
   --  bytes written so far, used to detect when the command is quiet

   Output_Line_Count : Natural := 0;
   Err_Line_Count    : Natural := 0;
   --  complete lines written so far

   Output_Line_Offset : Natural := 0;
   Err_Line_Offset    : Natural := 0;
   --  lines produced before the last input step

   Input_Byte_Count : Natural := 0;
   --  bytes produced when the last input was sent, or at the start of
   --  the command if no input was sent yet: the response to check is
   --  the output produced after that point

   Last_Return_Code : Integer := 0;
   --  Set when the command is reaped, read by the exit code checks of
   --  the following steps (each step is run by a separate Runner.Run_Step
   --  call).

   Scenario_Deadline : Ada.Calendar.Time := Ada.Calendar.Clock;
   --  expiry time of the current scenario when the scenario timeout is
   --  armed; the bound applies only when Settings.Scenario_Timeout > 0
   --  (cf. the design discussion D6)

   use type Ada.Calendar.Time;

   function Timeout_Expired return Boolean is
     (Settings.Scenario_Timeout > 0.0
      and then Ada.Calendar.Clock >= Scenario_Deadline);

   Output_Stream : Ada.Streams.Stream_IO.File_Type;
   Err_Stream    : Ada.Streams.Stream_IO.File_Type;
   --  Open for the whole command duration, and flushed at each chunk,
   --  so that output checks can read the files between two callbacks,
   --  without paying the file open cost at each chunk.

   function Running return Boolean is (Running_Cmd);

   function Last_Exit_Code return Integer is (Last_Return_Code);

   function Output_Bytes return Natural is (Output_Byte_Count);

   function Err_Bytes return Natural is (Err_Byte_Count);

   function Input_Bytes return Natural is (Input_Byte_Count);

   -- ------------------------------------------------------------------------
   procedure Set_Deadline is
   begin
      if Settings.Scenario_Timeout > 0.0 then
         Scenario_Deadline := Ada.Calendar.Clock + Settings.Scenario_Timeout;
      end if;
   end Set_Deadline;

   -- ------------------------------------------------------------------------
   procedure Append_Data (File_Name : String;
                          Data      : Ada.Streams.Stream_Element_Array)
   is
      F : Ada.Streams.Stream_IO.File_Type;
   begin
      --  Fallback used when no stream is open: the file is opened and
      --  closed at each call, so that output checks can read it between
      --  two callbacks.
      if Ada.Directories.Exists (File_Name) then
         Ada.Streams.Stream_IO.Open
           (F, Ada.Streams.Stream_IO.Append_File, File_Name);
      else
         Ada.Streams.Stream_IO.Create
           (F, Ada.Streams.Stream_IO.Out_File, File_Name);
      end if;
      Ada.Streams.Stream_IO.Write (F, Data);
      Ada.Streams.Stream_IO.Close (F);
   end Append_Data;

   -- ------------------------------------------------------------------------
   procedure Write_Chunk (Stream    : Ada.Streams.Stream_IO.File_Type;
                          File_Name : String;
                          Data      : Ada.Streams.Stream_Element_Array) is
   begin
      --  When a stream is open, the chunk is written through it, and
      --  flushed, so that output checks can read the file between two
      --  callbacks without paying the file open cost at each chunk;
      --  otherwise the fallback opens and closes the file at each call.
      if Ada.Streams.Stream_IO.Is_Open (Stream) then
         Ada.Streams.Stream_IO.Write (Stream, Data);
         Ada.Streams.Stream_IO.Flush (Stream);
      else
         Append_Data (File_Name, Data);
      end if;
   end Write_Chunk;

   -- ------------------------------------------------------------------------
   procedure Close_Stream (Stream : in out Ada.Streams.Stream_IO.File_Type) is
   begin
      --  The streams are flushed at each chunk: this final flush is a
      --  safety net, e.g. for a command that could not start.
      if Ada.Streams.Stream_IO.Is_Open (Stream) then
         Ada.Streams.Stream_IO.Flush (Stream);
         Ada.Streams.Stream_IO.Close (Stream);
      end if;
   end Close_Stream;

   -- ------------------------------------------------------------------------
   procedure Write_Output (Data : Ada.Streams.Stream_Element_Array) is
      use Ada.Streams;
   begin
      Write_Chunk (Output_Stream, To_String (Cmd_Output_Name), Data);
      Output_Byte_Count := @ + Natural (Data'Length);
      for C of Data loop
         if C = Character'Pos (Ada.Characters.Latin_1.LF) then
            Output_Line_Count := @ + 1;
         end if;
      end loop;
   end Write_Output;

   procedure Write_Error (Data : Ada.Streams.Stream_Element_Array) is
      use Ada.Streams;
   begin
      --  When the standard error is merged, it goes to the output file
      if Error_Merged then
         Write_Chunk (Output_Stream, To_String (Cmd_Output_Name), Data);
      else
         Write_Chunk (Err_Stream, To_String (Cmd_Err_Name), Data);
      end if;
      Err_Byte_Count := @ + Natural (Data'Length);
      for C of Data loop
         if C = Character'Pos (Ada.Characters.Latin_1.LF) then
            Err_Line_Count := @ + 1;
         end if;
      end loop;
   end Write_Error;

   -- ------------------------------------------------------------------------
   use type BBT.Pump.Stream_State;
   use type Util.Streams.Raw.Raw_Stream_Access;

   type Process_Access is access Util.Processes.Process;
   procedure Free_Process is new Ada.Unchecked_Deallocation
     (Object => Util.Processes.Process, Name => Process_Access);

   The_Process : Process_Access;
   --  The process run by the last Start. A new process object is
   --  created at each Start, and freed at the next one, once
   --  terminated and reaped: the ada-util library keeps no process
   --  map surviving the command, contrary to the Spawn monitor.

   Out_Raw : Util.Streams.Raw.Raw_Stream_Access;
   Err_Raw : Util.Streams.Raw.Raw_Stream_Access;
   --  The raw streams on the command output and error descriptors,
   --  watched by the pump; Err_Raw is null when the standard error is
   --  merged into the standard output

   Out_Closed : Boolean := True;
   Err_Closed : Boolean := True;
   --  end of file reached on the output / error stream


   -- -----------------------------------------------------------------------
   procedure Reap_Command is
   --  Wait for the terminated command, collect its exit status, and
   --  mark it no more running. Called once both output streams have
   --  reached end of file: the command has then terminated, or has
   --  closed its outputs while remaining unable to produce any more
   --  output, in which case the blocking Wait returns at its
   --  termination, as the pump loop did before.
   begin
      Wait (The_Process.all);
      Running_Cmd := False;
      Last_Return_Code := Get_Exit_Status (The_Process.all);
      Put_Debug_Line ("  command finished, exit code"
                      & Last_Return_Code'Image);
   end Reap_Command;

   -- -----------------------------------------------------------------------
   procedure Drain (Raw      : Util.Streams.Raw.Raw_Stream_Access;
                    Is_Error : Boolean;
                    Closed   : in out Boolean) is
      use Ada.Streams;
      Buffer : Stream_Element_Array (1 .. 2 ** 14);
      Last   : Stream_Element_Offset;
      Watch  : constant BBT.Pump.Watched_Stream
                 := (Raw => Raw, State => BBT.Pump.No_Data);
      Eof    : Boolean := False;
   begin
      --  Read the stream until it is drained: the first read is called
      --  on a descriptor that the caller knows readable, and the
      --  following ones only when more data is already available, so
      --  that the blocking reads never wait. The stream is marked
      --  closed on end of file only: a read pause just means that the
      --  command is still producing its output.
      while not Eof loop
         Raw.Read (Into => Buffer, Last => Last);
         Eof := Last < Buffer'First;
         exit when Eof;
         --  End of file: the command closed the stream
         if Is_Error then
            Write_Error (Buffer (Buffer'First .. Last));
         else
            Write_Output (Buffer (Buffer'First .. Last));
         end if;
         exit when not BBT.Pump.Has_Data (Watch);
         --  No more data already available
      end loop;
      Closed := Eof;
   end Drain;

   -- -----------------------------------------------------------------------
   procedure Poll (Timeout : Duration) is
      Watch : BBT.Pump.Watched_Array (1 .. 2);
      Count : Natural := 0;
   begin
      if The_Process = null or else not Running_Cmd then
         return;
      end if;
      if not Out_Closed then
         Count := @ + 1;
         Watch (Count) := (Raw => Out_Raw, State => BBT.Pump.No_Data);
      end if;
      if Err_Raw /= null and then not Err_Closed then
         Count := @ + 1;
         Watch (Count) := (Raw => Err_Raw, State => BBT.Pump.No_Data);
      end if;
      if Count = 0 then
         --  Both streams are at end of file: the command is
         --  terminated, but was not reaped yet
         Reap_Command;
         return;
      end if;
      BBT.Pump.Wait (Watch (1 .. Count), Timeout);
      for I in 1 .. Count loop
         if Watch (I).Raw = Out_Raw then
            if Watch (I).State = BBT.Pump.Data_Available then
               Drain (Raw      => Out_Raw,
                      Is_Error => False,
                      Closed   => Out_Closed);
            elsif Watch (I).State = BBT.Pump.End_Of_Stream then
               Out_Closed := True;
            end if;
         else
            if Watch (I).State = BBT.Pump.Data_Available then
               Drain (Raw      => Err_Raw,
                      Is_Error => True,
                      Closed   => Err_Closed);
            elsif Watch (I).State = BBT.Pump.End_Of_Stream then
               Err_Closed := True;
            end if;
         end if;
      end loop;
      if Out_Closed and then (Err_Raw = null or else Err_Closed) then
         Reap_Command;
      end if;
   end Poll;

   -- -----------------------------------------------------------------------
   procedure Pump (Timeout : Duration) is
      Deadline : constant Ada.Calendar.Time :=
                   Ada.Calendar.Clock + Timeout;
   begin
      while Running_Cmd loop
         if Timeout > 0.0 and then Ada.Calendar.Clock >= Deadline then
            return;
         end if;
         exit when Timeout_Expired;
         Poll (Quiet_Step);
      end loop;
   end Pump;

   -- -----------------------------------------------------------------------
   procedure Kill is
   begin
      if Running_Cmd and then The_Process /= null then
         Stop (The_Process.all, 9);
         Running_Cmd := False;
      end if;
   end Kill;

   -- -----------------------------------------------------------------------
   procedure Kill_And_Drain is
   begin
      if Running_Cmd and then The_Process /= null then
         Put_Debug_Line ("  killing the command on scenario timeout");
         Stop (The_Process.all, 9);
         --  Pump a moment, so that the command output is drained and
         --  the command reaped.
         Pump (0.5);
      end if;
   end Kill_And_Drain;

   -- ------------------------------------------------------------------------
   procedure Dispose is
   begin
      --  Close the output streams in any case: they were flushed at
      --  each chunk, so the files are already complete and readable.
      Close_Stream (Output_Stream);
      Close_Stream (Err_Stream);

      if The_Process = null then
         return;
      end if;
      if Running_Cmd then
         --  The command did not terminate by itself: kill it, and
         --  reap it; SIGKILL guarantees the termination
         Put_Debug_Line ("  disposing a still running command");
         Stop (The_Process.all, 9);
         Wait (The_Process.all);
         Running_Cmd := False;
      end if;
      Free_Process (The_Process);
   end Dispose;

   -- ------------------------------------------------------------------------
   procedure Start
     (Args        :     Util.Strings.Vectors.Vector;
      Merged      :     Boolean;
      Interactive :     Boolean;
      Out_Name    :     String;
      Err_Name    :     String;
      OK          : out Boolean)
   is
   begin
      OK := False;

      --  Truncate the output files, and reset the state.
      --
      --  Unless the command is to be fed interactively, the streams stay
      --  open for the whole command, and are flushed at each chunk:
      --  reopening a file at each chunk costs around 0.7 s on some
      --  Windows configurations, which made commands producing their
      --  output in many chunks, such as a nested bbt help grammar, take
      --  minutes instead of milliseconds. The streams are closed as
      --  soon as the command is terminated, or at the next command, so
      --  that the following checks can read the files.
      --
      --  For an interactive command, the output is instead appended by
      --  open-and-close at each chunk: the checks read the files
      --  between two chunks while the command is still running, which
      --  requires the file to be closed between the chunks.
      if Interactive then
         declare
            F : Ada.Streams.Stream_IO.File_Type;
         begin
            Ada.Streams.Stream_IO.Create
              (F, Ada.Streams.Stream_IO.Out_File, Out_Name);
            Ada.Streams.Stream_IO.Close (F);
            if Err_Name /= "" then
               Ada.Streams.Stream_IO.Create
                 (F, Ada.Streams.Stream_IO.Out_File, Err_Name);
               Ada.Streams.Stream_IO.Close (F);
            end if;
         end;
      else
         Ada.Streams.Stream_IO.Create
           (Output_Stream, Ada.Streams.Stream_IO.Out_File, Out_Name);
         if Err_Name /= "" then
            Ada.Streams.Stream_IO.Create
              (Err_Stream, Ada.Streams.Stream_IO.Out_File, Err_Name);
         end if;
      end if;
      Cmd_Output_Name := To_Unbounded_String (Out_Name);
      Error_Merged       := Merged;
      if not Error_Merged then
         Cmd_Err_Name := To_Unbounded_String (Err_Name);
      end if;
      Output_Byte_Count       := 0;
      Err_Byte_Count          := 0;
      Output_Line_Count       := 0;
      Err_Line_Count          := 0;
      Output_Line_Offset := 0;
      Err_Line_Offset    := 0;
      Input_Byte_Count        := 0;
      Running_Cmd            := False;

      The_Process := new Util.Processes.Process;
      Set_Shell (The_Process.all, "");
      --  No /bin/sh: the process is spawned directly, either with the
      --  arguments split by bbt, or with the command string as a single
      --  argument of the configured shell
      if Interactive then
         --  The pseudo terminal makes the command prompts visible
         --  before it waits for input, without fflush; the library
         --  configures the terminal slave in raw mode: no echo, no
         --  canonical mode, no CR LF translation
         --  (cf. docs/proposed_features/pty.md)
         Set_Allocate_TTY (The_Process.all);
      end if;
      --  The command inherits the bbt environment, including the
      --  variables set or unset by the environment variable steps:
      --  Set_Default_Environment imports the bbt current environment
      --  at spawn time, and thus reflects those steps.
      Set_Default_Environment (The_Process.all);
      if Interactive then
         --  There is no pseudo terminal on Windows: the command stdin
         --  is a pipe, on which the input steps pump the input. Commit
         --  to the command that its stdin will be fed, so that its own
         --  prompts are asked instead of being refused on the non
         --  interactive stdin (cf. Confirm in File_Operations).
         Util.Processes.Set_Environment
           (The_Process.all, "BBT_FED_STDIN", "1");
      end if;

      begin
         Util.Processes.Spawn (The_Process.all,
                Arguments => Args,
                Mode      => (if Error_Merged
                              then Util.Processes.READ_WRITE_ALL
                              else Util.Processes.READ_WRITE_ALL_SEPARATE));
         Running_Cmd := True;
      exception
         when E : others =>
            Running_Cmd := False;
            OK := False;
            --  The command could not start: close the streams, so that
            --  the following checks can read the empty output files.
            Close_Stream (Output_Stream);
            Close_Stream (Err_Stream);
            Put_Debug_Line ("  cannot start the command: " &
                              Ada.Exceptions.Exception_Information (E));
            return;
      end;

      --  Watch the command output and error descriptors
      Out_Raw := Util.Streams.Raw.Raw_Stream_Access
                   (Get_Output_Stream (The_Process.all));
      Err_Raw := Util.Streams.Raw.Raw_Stream_Access
                   (Get_Error_Stream (The_Process.all));
      Out_Closed := False;
      Err_Closed := Err_Raw = null;
      --  Merged standard error: no error stream to watch

      if Interactive then
         --  Give the pseudo terminal its window size: an unset size
         --  (0x0) would make any program querying its terminal size
         --  misbehave, and the simulated terminal must be
         --  indistinguishable from a real one
         --  (cf. docs/proposed_features/pty.md)
         BBT.Terminal.Set_Size (int (Out_Raw.Get_File));
      end if;

      OK := True;
   end Start;

   -- ------------------------------------------------------------------------
   procedure Send (Data : String; OK : out Boolean) is
      use Ada.Streams;
      Data_Bytes : Stream_Element_Array (1 .. Data'Length);
   begin
      for J in Data'Range loop
         Data_Bytes (Stream_Element_Offset (J)) := Character'Pos (Data (J));
      end loop;
      begin
         Get_Input_Stream (The_Process.all).Write (Data_Bytes);
         OK := True;
      exception
         when E : others =>
            Put_Debug_Line ("  cannot send input to the command: " &
                              Ada.Exceptions.Exception_Information (E));
            OK := False;
      end;
   end Send;

   -- ------------------------------------------------------------------------
   procedure Reset is
   begin
      if Running_Cmd then
         Pump (1.0);
         if Running_Cmd then
            Put_Debug_Line ("  killing a command still running at the " &
                              "end of the scenario");
            Stop (The_Process.all, 9);
            Running_Cmd := False;
         end if;
      end if;
      Output_Line_Offset := 0;
      Err_Line_Offset    := 0;
      Dispose;
   end Reset;

   -- ------------------------------------------------------------------------
   procedure Close_Output_Files is
   begin
      Close_Stream (Output_Stream);
      Close_Stream (Err_Stream);
   end Close_Output_Files;

   -- ------------------------------------------------------------------------
   procedure Set_Input_Mark is
   begin
      Output_Line_Offset := Output_Line_Count;
      Err_Line_Offset    := Err_Line_Count;
      Input_Byte_Count        := Output_Byte_Count + Err_Byte_Count;
   end Set_Input_Mark;

   -- ------------------------------------------------------------------------
   function Output_Since_Input (Output : Text) return Text is
      Result : Text := Empty_Text;
      Skip   : Natural := Output_Line_Offset;
   begin
      for Line of Output loop
         if Skip > 0 then
            Skip := @ - 1;
         else
            Result.Append (Line);
         end if;
      end loop;
      return Result;
   end Output_Since_Input;

   -- ------------------------------------------------------------------------
   function Stderr_Since_Input (Output : Text) return Text is
      Result : Text := Empty_Text;
      Skip   : Natural := Err_Line_Offset;
   begin
      for Line of Output loop
         if Skip > 0 then
            Skip := @ - 1;
         else
            Result.Append (Line);
         end if;
      end loop;
      return Result;
   end Stderr_Since_Input;

end BBT.Tests.Actions.Commands.Spawn;
