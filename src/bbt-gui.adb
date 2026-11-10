-----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-----------------------------------------------------------------------------

with Ada.Directories;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Sockets;

with Gnoga;
with UXStrings;
with Gnoga.Application;
with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.View;
with Gnoga.Gui.Window;
with Gnoga.Server;

with BBT.Execution_Control;
use BBT.Execution_Control;

with BBT.IO;
with BBT.Status_Bar;
with BBT.Tests.Runner;
with Text_Utilities;
use Text_Utilities;

package body BBT.GUI is

   --  Heading anchors, for the ToC links: the slug of each markdown
   --  heading mapped to its line number in the currently displayed
   --  file. Rebuilt by Show_File at each file load, read by Beautify.
   package String_Natural_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => String,
        Element_Type => Natural);
   Headings : String_Natural_Maps.Map;

   function Slug (S : String) return String is
      --  Anchor normalization: letters (lower cased) and digits only,
      --  everything else dropped. Formatting insensitive, so that ToC
      --  hrefs and headings match whatever the flattening style is.
      Buffer : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for C of S loop
         if C in 'a' .. 'z' or C in '0' .. '9' then
            Ada.Strings.Unbounded.Append (Buffer, [C]);
         elsif C in 'A' .. 'Z' then
            Ada.Strings.Unbounded.Append
              (Buffer, [Character'Val (Character'Pos (C) + 32)]);
         end if;
      end loop;
      return Ada.Strings.Unbounded.To_String (Buffer);
   end Slug;

   function Trimmed_Image (N : Natural) return String is
     (Ada.Strings.Fixed.Trim (N'Image, Ada.Strings.Both));

   function Heading_Line (Key : String) return String is
   begin
      if Headings.Contains (Key) then
         return Trimmed_Image (Headings.Element (Key));
      else
         return "";
      end if;
   end Heading_Line;

   --------------------------------------------------------------------------
   --  Runtime state shared between the runner task, the gnoga
   --  connection tasks and Run :
   --------------------------------------------------------------------------

   protected UI_State is
      procedure Set_Wired (On : Boolean);
      procedure Set_Done;
      function  Wired return Boolean;
      entry Wait_Done;
   private
      Wired_Flag : Boolean := False;
      Run_Done   : Boolean := False;
   end UI_State;

   protected body UI_State is
      procedure Set_Wired (On : Boolean) is
      begin
         Wired_Flag := On;
      end Set_Wired;

      procedure Set_Done is
      begin
         Run_Done := True;
      end Set_Done;

      function Wired return Boolean is (Wired_Flag);

      entry Wait_Done when Run_Done is
      begin
         null;
      end Wait_Done;
   end UI_State;

   --------------------------------------------------------------------------
   --  The debugger page widgets. Elements are accesses, allocated by
   --  On_Connect on every connection: gnoga never re-runs the connect
   --  handler for a socket reconnect, and a reloaded tab is a new
   --  connection anyway, so the page must be rebuildable and state is
   --  replayed to a late connection. One live page at a time: the
   --  last connected tab is the debugged one.
   --------------------------------------------------------------------------

   Loc_Msg     : Gnoga.Gui.Element.Common.P_Access;
   --  The "file > feature > scenario" location line, below the buttons.

   Status_Text : Gnoga.Gui.Element.Common.P_Access;
   Progress    : Gnoga.Gui.Element.Common.DIV_Access;
   Progress_Fill : Gnoga.Gui.Element.Common.DIV_Access;
   --  The progress bar: a thin track below the locbar, filled in
   --  proportion to the scenarios completed so far in the whole run.

   Content     : Gnoga.Gui.Element.Common.DIV_Access;
   Step_Btn    : Gnoga.Gui.Element.Common.Button_Access;
   Scen_Btn    : Gnoga.Gui.Element.Common.Button_Access;
   Feat_Btn    : Gnoga.Gui.Element.Common.Button_Access;
   File_Btn    : Gnoga.Gui.Element.Common.Button_Access;
   Finish_Btn  : Gnoga.Gui.Element.Common.Button_Access;
   Stop_Toggle : Gnoga.Gui.Element.Common.Button_Access;

   Stop_On_Err : Boolean := True;
   --  Mirror of the runtime Pause_On_Error switch, for the toggle
   --  button label.

   Cur_File : Ada.Strings.Unbounded.Unbounded_String;
   --  The file currently displayed

   Last_Loc_File : Ada.Strings.Unbounded.Unbounded_String;
   --  The normalized file name of the last location (the form the
   --  source store is keyed on), kept for the late connection replay.

   Last_File    : Ada.Strings.Unbounded.Unbounded_String;
   Last_Feature : Ada.Strings.Unbounded.Unbounded_String;
   Last_Scen    : Ada.Strings.Unbounded.Unbounded_String;
   Last_Status  : Ada.Strings.Unbounded.Unbounded_String;
   Last_Line    : Natural := 0;
   --  The last displayed location (file, feature, scenario), status
   --  line and highlighted line, kept to replay the state to late
   --  connections.

   --------------------------------------------------------------------------
   function Escape (S : String) return Ada.Strings.Unbounded.Unbounded_String is
      --  Escape HTML special characters in a file content line
      Buffer : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for C of S loop
         case C is
            when '&'    => Append (Buffer, "&amp;");
            when '<'    => Append (Buffer, "&lt;");
            when '>'    => Append (Buffer, "&gt;");
            when others => Append (Buffer, [C]);
         end case;
      end loop;
      return Buffer;
   end Escape;


   --------------------------------------------------------------------------
   --  Update_Locbar composes the "file > feature > scenario" line from
   --  the recorded location state.
   --------------------------------------------------------------------------

   procedure Update_Locbar is
      Sep : constant String := " &gt; ";
      Result : Ada.Strings.Unbounded.Unbounded_String;

      procedure Add (S : String; Escape_It : Boolean := False) is
      begin
         if Length (Result) > 0 then
            Append (Result, Sep);
         end if;
         if Escape_It then
            Append (Result, To_String (Escape (S)));
         else
            Append (Result, S);
         end if;
      end Add;

   begin
      if Length (Last_File) > 0 then
         Add (To_String (Last_File));
      end if;
      if Length (Last_Feature) > 0 then
         Add (To_String (Last_Feature), Escape_It => True);
      end if;
      if Length (Last_Scen) > 0 then
         Add (To_String (Last_Scen), Escape_It => True);
      end if;
      Loc_Msg.Inner_HTML (UXStrings.From_UTF_8 (To_String (Result)));
   end Update_Locbar;


   --------------------------------------------------------------------------
   --  Beautify escapes HTML and applies a light markdown to a source
   --  line for display: headings, inline code, bold, list bullets,
   --  blockquotes and links. In-document links (ToC) are turned into
   --  scroll-to-line anchors (data-ln attribute), using the Headings
   --  map built by Show_File.
   --------------------------------------------------------------------------

   function Beautify (S : String) return Ada.Strings.Unbounded.Unbounded_String is
      Text : Ada.Strings.Unbounded.Unbounded_String := Escape (S);

      procedure Wrap_All (Mark, Open_Tag, Close_Tag : String) is
         From : Positive := 1;
         P1, P2 : Natural;
      begin
         loop
            P1 := Index (Text, Mark, From);
            exit when P1 = 0;
            P2 := Index (Text, Mark, P1 + Mark'Length);
            exit when P2 = 0;
            Replace_Slice (Text, P2, P2 + Mark'Length - 1, Close_Tag);
            Replace_Slice (Text, P1, P1 + Mark'Length - 1, Open_Tag);
            From := P1 + Open_Tag'Length + (P2 - P1 - Mark'Length) + Close_Tag'Length;
         end loop;
      end Wrap_All;

      procedure Make_Links is
         From : Positive := 1;
         P1, P2, P3, P4 : Natural;
      begin
         loop
            P1 := Index (Text, "[", From);
            exit when P1 = 0;
            P2 := Index (Text, "]", From => P1 + 1);
            exit when P2 = 0;
            P3 := Index (Text, "(", From => P2 + 1);
            exit when P3 = 0;
            P4 := Index (Text, ")", From => P3 + 1);
            exit when P4 = 0;
            declare
               Link_Text : constant String :=
                             Ada.Strings.Unbounded.Slice (Text, P1 + 1, P2 - 1);
               Url       : constant String :=
                             Ada.Strings.Unbounded.Slice (Text, P3 + 1, P4 - 1);
               Tail      : constant Ada.Strings.Unbounded.Unbounded_String :=
                             Ada.Strings.Unbounded.Unbounded_Slice (Text, P4 + 1, Length (Text));
               Head      : constant Ada.Strings.Unbounded.Unbounded_String :=
                             Ada.Strings.Unbounded.Unbounded_Slice (Text, 1, P1 - 1);
            begin
               if Url'Length > 0 and then Url (Url'First) = '#' then
                  --  In-document anchor: scroll-to-line link, resolved
                  --  against the Headings map; unresolved targets stay
                  --  inert (no data-ln, the click handler ignores them)
                  Text := Head
                    & "<a class=""toc"" href=""#"""
                    & (if Heading_Line (Slug (Url (Url'First + 1 .. Url'Last))) /= ""
                       then " data-ln="""
                            & Heading_Line (Slug (Url (Url'First + 1 .. Url'Last)))
                            & """"
                       else "")
                    & ">" & Link_Text & "</a>" & Tail;
               else
                  Text := Head
                    & "<a href=""" & Url & """ target=""_blank"">"
                    & Link_Text & "</a>" & Tail;
               end if;
            end;
            From := P1 + 1;
            exit when From > Length (Text);
         end loop;
      end Make_Links;

   begin
      Wrap_All ("`", "<code>", "</code>");
      Wrap_All ("**", "<b>", "</b>");
      Make_Links;

      --  Headings: strip the leading #'s, wrap in a span.
      if Length (Text) > 0 and then Element (Text, 1) = '#' then
         declare
            Level : Natural := 0;
            Cut   : Natural;
            --  One past the last stripped character: the #'s and, if
            --  present, the space that follows them.
         begin
            while Level < Length (Text)
              and then Element (Text, Level + 1) = '#'
            loop
               Level := Level + 1;
            end loop;
            Level := Natural'Min (Level, 3);
            Cut   := Level
                       + (if Level < Length (Text)
                            and then Element (Text, Level + 1) = ' '
                          then 1 else 0);
            if Cut > 0 then
               Replace_Slice (Text, 1, Cut, "");
            end if;
            Text := "<span class=""md_h"
              & Ada.Strings.Fixed.Trim (Level'Image, Ada.Strings.Both)
              & """>" & Text & "</span>";
         end;
      elsif Length (Text) >= 8
        and then Slice (Text, 1, 5) = "&gt; "
      then
         --  Blockquote (the '>' was escaped by Beautify's Escape)
         Replace_Slice (Text, 1, 5, "<span class=""quote"">");
         Text := Text & "</span>";
      elsif Length (Text) >= 2
        and then Element (Text, 1) = '-'
        and then Element (Text, 2) = ' '
      then
         Replace_Slice (Text, 1, 2, "&bull; ");
      end if;

      return Text;
   end Beautify;

   --------------------------------------------------------------------------
   procedure Show_File (File_Name : String) is
      Source : constant Text := Get_Source (File_Name);
      --  The verbatim content registered by the runtime at analysis
      --  time. The Text holds one element per file line, at the file
      --  line rank, so the div ids (ln_n) and the step locations match.
      Page   : Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String ("");

      In_Fence    : Boolean := False;
      Is_Mermaid  : Boolean := False;
      MM_Sequence : Natural := 0;
      --  Fence state: lines inside a code fence keep their raw text
      --  (class fence), and lines inside a mermaid fence are grouped
      --  (data-mm id) so that page.js can join and render them.

      --  -----------------------------------------------------------------------
      procedure Append_Line (Line : Natural; Text : String) is
      begin
         if Text'Length >= 3
           and then Text (Text'First .. Text'First + 2) = "```"
         then
            --  A fence marker: switch the state, and show the
            --  marker line itself as a fence line
            In_Fence := not In_Fence;
            if In_Fence then
               Is_Mermaid := Text'Length >= 10
                             and then Text (Text'First + 3 .. Text'First + 9)
                                         = "mermaid";
               if Is_Mermaid then
                  MM_Sequence := MM_Sequence + 1;
               end if;
            end if;
            Append (Page, "<div class=""line fence"" id=""ln_"
                          & Trimmed_Image (Line) & """>" & To_String (Escape (Text)) & "</div>");

         elsif In_Fence and then Is_Mermaid then
            Append (Page, "<div class=""line mm"" id=""ln_"
                          & Trimmed_Image (Line) & """ data-mm=""mm_"
                          & Trimmed_Image (MM_Sequence)
                          & """>" & To_String (Escape (Text)) & "</div>");

         elsif In_Fence then
            Append (Page, "<div class=""line fence"" id=""ln_"
                          & Trimmed_Image (Line) & """>" & To_String (Escape (Text)) & "</div>");

         else
            Append (Page, "<div class=""line"" id=""ln_"
                          & Trimmed_Image (Line) & """>" & To_String (Beautify (Text)) & "</div>");
         end if;
      end Append_Line;

   begin
      Headings.Clear;
      for I in Source.First_Index .. Source.Last_Index loop
         declare
            Text : constant String := Source.Element (I);
         begin
            if Text'Length > 0 and then Text (Text'First) = '#' then
               Headings.Include (Key => Slug (Text), New_Item => Natural (I));
            end if;
            Append_Line (Natural (I), Text);
         end;
      end loop;

      Content.Inner_HTML (UXStrings.From_UTF_8 (To_String (Page)));
      Cur_File := To_Unbounded_String (File_Name);
   end Show_File;

   --------------------------------------------------------------------------
   --  Highlights are applied in the browser by the bbt:highlight
   --  handler installed by page.js: it removes the current step and
   --  failure highlights, tags the target line, and scrolls the
   --  #content view so that the line is centered. We just fire the
   --  event, with the line number and the classes to add.
   --------------------------------------------------------------------------

   procedure Highlight_Line (Line : Ada.Text_IO.Count;
                             Hl   : Boolean := False;
                             Fail : Boolean := False) is
      Cls : constant String :=
        (if Hl and Fail   then "'hl','fail'"
         elsif Hl         then "'hl'"
         elsif Fail       then "'fail'"
         else "");
   begin
      Content.jQuery_Execute
        (UXStrings.From_UTF_8
           ("trigger ('bbt:highlight', [" & Trimmed_Image (Natural (Line))
            & ", [" & Cls & "]]);"));
   end Highlight_Line;

   --------------------------------------------------------------------------
   --  The line number of a location as an integer: the accessor has a
   --  Count and a String flavor, the String image avoids the ambiguity.
   --------------------------------------------------------------------------

   function Location_Line (Loc : BBT.IO.Location_Type) return Natural is
     (Natural'Value (BBT.IO.Line (Loc)));

   --------------------------------------------------------------------------
   --  The progress bar: filled in proportion to the scenarios
   --  completed so far in the whole run. The counters come from
   --  BBT.Status_Bar, at scenario granularity (the bar advances at
   --  scenario boundaries, matching the terminal status bar).
   --------------------------------------------------------------------------

   procedure Update_Progress is
      Done  : constant Natural := BBT.Status_Bar.Scenarios_Done;
      Total : constant Natural := BBT.Status_Bar.Scenarios_Total;
      Pct   : constant Natural :=
                (if Total > 0 then (100 * Done) / Total else 0);
   begin
      Progress_Fill.Style
        ("width", UXStrings.From_UTF_8 (Trimmed_Image (Pct) & "%"));
   end Update_Progress;

   --------------------------------------------------------------------------
   --  The controller: called by the runner task. gnoga's message
   --  dispatch is multi tasking safe: any task may update elements.
   --------------------------------------------------------------------------

   --------------------------------------------------------------------------
   --  Set_Running switches the status line out of the paused state: any
   --  release command sets it, so that the page does not claim a pause
   --  that no longer exists while the run traverses halt points.

   procedure Set_Running (Message : String) is
   begin
      Last_Status := To_Unbounded_String (Message);
      if UI_State.Wired then
         Status_Text.Inner_HTML (UXStrings.From_UTF_8 (Message));
      end if;
   end Set_Running;

   --------------------------------------------------------------------------
   --  Show_Location displays the file the location points to: the page
   --  content is served from the runtime's source store, and the line
   --  is highlighted, debugger style. Used for every stop that carries
   --  a location: the highlighted line is the step about to run at a
   --  step stop, and the declaration line at a file, feature or
   --  scenario stop. A zero line (a document location) just skips the
   --  highlight.

   procedure Show_Location (Loc : BBT.IO.Location_Type) is
   begin
      if Cur_File /= To_Unbounded_String (BBT.IO.File (Loc))
        and then not Is_Empty (Get_Source (BBT.IO.File (Loc)))
      then
         Show_File (BBT.IO.File (Loc));
      end if;
      if Location_Line (Loc) > 0 then
         Highlight_Line (BBT.IO.Line (Loc), Hl => True);
      end if;
   end Show_Location;

   type Gui_Controller is new BBT.Execution_Control.Controller with null record;

   overriding procedure Handle (C : in out Gui_Controller; E : Event) is
      pragma Unreferenced (C);

   begin
      --  Record the display state first: it is replayed to late
      --  connections even when no page is wired yet. The statuses
      --  describe the pause reason; the location itself is on the
      --  locbar, no point in repeating it. Last_Loc_File keeps the
      --  normalized file name that keys the source store, for the
      --  replay.
      case E.Kind is
         when Execution_Started =>
            Last_File      := Null_Unbounded_String;
            Last_Loc_File  := Null_Unbounded_String;
            Last_Feature   := Null_Unbounded_String;
            Last_Scen      := Null_Unbounded_String;
            Last_Line      := 0;
            Last_Status    := To_Unbounded_String ("Execution started");
         when Before_File =>
            Last_File      := E.Label;
            Last_Loc_File  := To_Unbounded_String (BBT.IO.File (E.Location));
            Last_Line      := Location_Line (E.Location);
            Last_Status    := To_Unbounded_String ("paused on file");
         when Before_Feature =>
            Last_Feature   := E.Feature;
            Last_Loc_File  := To_Unbounded_String (BBT.IO.File (E.Location));
            Last_Line      := Location_Line (E.Location);
            Last_Status    := To_Unbounded_String ("paused on feature");
         when Before_Scenario =>
            Last_Scen      := E.Label;
            Last_Loc_File  := To_Unbounded_String (BBT.IO.File (E.Location));
            Last_Line      := Location_Line (E.Location);
            Last_Status    := To_Unbounded_String ("paused on scenario");
         when Before_Step =>
            Last_Status    := To_Unbounded_String ("paused on step");
            Last_Line      := Location_Line (E.Location);
            Last_Loc_File  := To_Unbounded_String (BBT.IO.File (E.Location));
            Last_File      := Last_Loc_File;
            if Length (E.Feature) > 0 then
               Last_Feature := E.Feature;
            end if;
            if Length (E.Label) > 0 then
               Last_Scen := E.Label;
            end if;
         when After_Step =>
            null;
         when Execution_Finished =>
            Last_Status := To_Unbounded_String ("Execution finished");
      end case;

      --  Events before the first connection: no page to update.
      if not UI_State.Wired then
         return;
      end if;

      --  Live page update.
      Update_Locbar;
      Update_Progress;

      --  All stops display the same way: the file the location points
      --  to, the line highlighted (a step line at a step stop, the
      --  declaration line at a file, feature or scenario stop), and
      --  the pause reason on the status line.
      case E.Kind is
         when Before_File | Before_Feature
           | Before_Scenario | Before_Step =>
            Show_Location (E.Location);
            Status_Text.Inner_HTML
              (UXStrings.From_UTF_8 (To_String (Last_Status)));

         when Execution_Started | Execution_Finished =>
            Status_Text.Inner_HTML
              (UXStrings.From_UTF_8 (To_String (Last_Status)));

         when After_Step =>
            if E.Failed then
               Highlight_Line (BBT.IO.Line (E.Location), Fail => True);
               if BBT.Execution_Control.Stopping_On_Error then
                  Last_Status :=
                    To_Unbounded_String ("paused after failed step");
                  Status_Text.Inner_HTML
                    (UXStrings.From_UTF_8 (To_String (Last_Status)));
               end if;
            end if;

      end case;
   end Handle;

   The_Controller : aliased Gui_Controller;

   --------------------------------------------------------------------------
   --  Command buttons
   --------------------------------------------------------------------------

   --  Button vocabulary follows the debugger's: the "Next ..." buttons
   --  release the stop for the next halt point of the matching scope,
   --  and "Continue" releases it for good, running to the end, as a
   --  debugger's continue runs to termination in the absence of
   --  breakpoints. Once breakpoints exist, "Continue" will mean run to
   --  the next breakpoint instead, and run to the very end will be the
   --  Stop on error switch set to OFF, followed by another Continue,
   --  so that no halt, not even an error, stops the run.

   procedure On_Step (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Continue (To => Next_Step);
      Set_Running ("running to next stop");
   end On_Step;

   procedure On_Continue_Scenario (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Continue (To => Next_Scenario);
      Set_Running ("running to next scenario");
   end On_Continue_Scenario;

   procedure On_Continue_Feature (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Continue (To => Next_Feature);
      Set_Running ("running to next feature");
   end On_Continue_Feature;

   procedure On_Continue_File (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Continue (To => Next_File);
      Set_Running ("running to next file");
   end On_Continue_File;

   procedure On_Finish (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Finish;
      Set_Running ("running to the end, no more stops");
   end On_Finish;

   procedure On_Toggle_Stop_On_Error (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Stop_On_Err := not Stop_On_Err;
      Set_Pause_On_Error (On => Stop_On_Err);
      Stop_Toggle.Inner_HTML
        (UXStrings.From_UTF_8 ("Stop on error: " & (if Stop_On_Err then "ON" else "OFF")));
   end On_Toggle_Stop_On_Error;

   --------------------------------------------------------------------------
   --  Deployment of the js and html assets next to the executable:
   --  gnoga looks for them relative to the application directory
   --  (cf. gui/README.md). Idempotent: re-copied at each --gui run.
   --------------------------------------------------------------------------

   procedure Deploy_Assets is
      Exe_Dir : constant String := UXStrings.To_UTF_8 (Gnoga.Server.Executable_Directory);

      function Source_Dir return String is
      begin
         if Ada.Directories.Exists (Exe_Dir & "gui/html/boot.html") then
            return Exe_Dir & "gui";
         elsif Ada.Directories.Exists (Exe_Dir & "../gui/html/boot.html") then
            return Exe_Dir & "../gui";
         else
            return "";
         end if;
      end Source_Dir;

      Src : constant String := Source_Dir;

   begin
      if Src = "" then
         --  No assets around: the boot page will display a warning
         return;
      end if;

      Ada.Directories.Create_Path (Exe_Dir & "html");
      Ada.Directories.Create_Path (Exe_Dir & "js");
      begin
         Ada.Directories.Copy_File (Src & "/html/boot.html",
                                    Exe_Dir & "html/boot.html");
         Ada.Directories.Copy_File (Src & "/js/boot.js",
                                    Exe_Dir & "js/boot.js");
         Ada.Directories.Copy_File (Src & "/js/jquery.min.js",
                                    Exe_Dir & "js/jquery.min.js");
         Ada.Directories.Copy_File (Src & "/js/mermaid.min.js",
                                    Exe_Dir & "js/mermaid.min.js");
         Ada.Directories.Copy_File (Src & "/js/page.js",
                                    Exe_Dir & "js/page.js");
      exception
         when Ada.Directories.Name_Error =>
            null;
      end;
   end Deploy_Assets;

   --------------------------------------------------------------------------
   --  The connection handler (server side): rebuilds the debugger page
   --  for every connection. Elements are accesses allocated here: gnoga
   --  never re-runs the connect handler for a socket reconnect, and a
   --  reloaded tab is a new connection anyway. One live page at a
   --  time: the last connected tab is the debugged one, previous
   --  pages keep a stale display (their buttons still work, they
   --  just receive no updates).
   --------------------------------------------------------------------------

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      View : Gnoga.Gui.View.View_Type;
      Bar  : Gnoga.Gui.Element.Common.DIV_Type;
      Locbar : Gnoga.Gui.Element.Common.DIV_Type;
   begin
      Loc_Msg     := new Gnoga.Gui.Element.Common.P_Type;
      Status_Text := new Gnoga.Gui.Element.Common.P_Type;
      Progress    := new Gnoga.Gui.Element.Common.DIV_Type;
      Progress_Fill := new Gnoga.Gui.Element.Common.DIV_Type;
      Content     := new Gnoga.Gui.Element.Common.DIV_Type;
      Step_Btn    := new Gnoga.Gui.Element.Common.Button_Type;
      Scen_Btn    := new Gnoga.Gui.Element.Common.Button_Type;
      Feat_Btn    := new Gnoga.Gui.Element.Common.Button_Type;
      File_Btn    := new Gnoga.Gui.Element.Common.Button_Type;
      Finish_Btn  := new Gnoga.Gui.Element.Common.Button_Type;
      Stop_Toggle := new Gnoga.Gui.Element.Common.Button_Type;

      View.Create (Main_Window);

      Bar.Create (View, "", "topbar");

      Step_Btn.Create (Bar, "Next step", "step");
      Step_Btn.On_Click_Handler (On_Step'Unrestricted_Access);

      Scen_Btn.Create (Bar, "Next scenario", "to_scenario");
      Scen_Btn.On_Click_Handler (On_Continue_Scenario'Unrestricted_Access);

      Feat_Btn.Create (Bar, "Next feature", "to_feature");
      Feat_Btn.On_Click_Handler (On_Continue_Feature'Unrestricted_Access);

      File_Btn.Create (Bar, "Next file", "to_file");
      File_Btn.On_Click_Handler (On_Continue_File'Unrestricted_Access);

      Finish_Btn.Create (Bar, "Continue", "finish");
      Finish_Btn.On_Click_Handler (On_Finish'Unrestricted_Access);

      Stop_Toggle.Create (Bar, UXStrings.From_UTF_8 (if Stop_On_Err
                                                     then "Stop on error: ON"
                                                     else "Stop on error: OFF"),
                          "stop_toggle");
      Stop_Toggle.On_Click_Handler (On_Toggle_Stop_On_Error'Unrestricted_Access);

      Status_Text.Create (Bar, "starting", "status");

      Locbar.Create (View, "", "locbar");
      Loc_Msg.Create (Locbar, "", "loc");

      Progress.Create (Locbar, "", "progress");
      --  A thin filled bar under the location line.
      Progress_Fill.Create (Progress.all, "", "progress_fill");

      Content.Create (View, "", "content");

      --  Replay the display state for a late or reloaded tab: the
      --  connection typically arrives after the run paused at the
      --  first halt point, so what has to be restored is not only
      --  the locbar and status texts, but the whole page content.
      Update_Locbar;
      if Length (Last_Loc_File) > 0
        and then not Is_Empty (Get_Source (To_String (Last_Loc_File)))
      then
         Show_File (To_String (Last_Loc_File));
         if Last_Line > 0 then
            Highlight_Line (Ada.Text_IO.Count (Last_Line), Hl => True);
         end if;
      end if;
      if Length (Last_Status) > 0 then
         Status_Text.Inner_HTML (UXStrings.From_UTF_8 (To_String (Last_Status)));
      end if;
      Update_Progress;

      --  Mark the session as wired BEFORE holding the connection:
      --  Connection.Hold blocks until the connection is released (i.e.
      --  closed), so anything after it would only run on a dead page.
      --  The state to replay is already in Last_*, and the page just
      --  rebuilt it, so live updates from now on can target it.
      UI_State.Set_Wired (True);

      Connection.Hold;
      --  Prevents this connection's finalization while the run keeps
      --  referencing the page elements.
   end On_Connect;

   --------------------------------------------------------------------------
   --  The run task
   --------------------------------------------------------------------------

   task type Run_Task;

   task body Run_Task is
   begin
      BBT.Tests.Runner.Run_All;
      UI_State.Set_Done;
   exception
      when others =>
         --  Run_All catches step level exceptions; any exception
         --  breaking out of here would be a bbt bug: keep the
         --  debugger session alive, and let BBT.IO report on the
         --  terminal.
         UI_State.Set_Done;
   end Run_Task;

   --------------------------------------------------------------------------
   --  The gnoga HTTP server task. Connection.Initialize only registers
   --  the connection handler and prints the listen banner: the socket
   --  is created when the message loop runs. Its own task keeps Run
   --  free to wait for the run task completion.
   --------------------------------------------------------------------------

   task type Server_Task;
   --  Blocks in the message loop until End_Application serves Stop.

   task body Server_Task is
   begin
      Gnoga.Application.Multi_Connect.Message_Loop;
   end Server_Task;

   The_Server : access Server_Task;
   --  Never read: the allocation in Run starts the server task.
   pragma Warnings (Off, The_Server);

   The_Run : access Run_Task;
   --  Never read: the allocation in Run starts the run task, which
   --  is all we need.
   pragma Warnings (Off, The_Run);

   --------------------------------------------------------------------------
   procedure Run is
      Port : Natural := 8080;

      --------------------------------------------------------------------
      --  Port_In_Use probes the loopback port: a successful client
      --  connect means some listener already owns it. Each --gui
      --  session picks the first free port from 8080 on, so that
      --  several bbt instances never share an ambiguous endpoint:
      --  a shared port dispatches browsers and commands between
      --  unrelated sessions at random.
      --------------------------------------------------------------------

      function Port_In_Use (Port : Natural) return Boolean is
         use GNAT.Sockets;
      begin
         declare
            Socket : Socket_Type;
         begin
            Create_Socket (Socket);
            Connect_Socket (Socket,
                            (Family_Inet, Inet_Addr ("127.0.0.1"),
                             Port_Type (Port)));
            Close_Socket (Socket);
            return True;
         exception
            when Socket_Error =>
               Close_Socket (Socket);
               return False;
         end;
      end Port_In_Use;

   begin
      Deploy_Assets;

      while Port_In_Use (Port) loop
         Port := Port + 1;
         exit when Port > 8099;
      end loop;
      --  A used port means another debug session is still around: we
      --  take the next free one rather than fight for 8080.

      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "Debug session on http://127.0.0.1:"
         & Ada.Strings.Fixed.Trim (Port'Image, Ada.Strings.Both));
      --  On the error output: the session url is an interactive
      --  prompt, not a result. In particular, bbt's generated reports
      --  capture the standard output, and would swallow it verbatim.

      Gnoga.Application.Title ("bbt debugger");
      Gnoga.Application.HTML_On_Close
        (UXStrings.From_UTF_8 ("<h2>The bbt debug session is closed</h2>The run goes on in "
         & "the terminal if any."));
      Gnoga.Application.Open_URL
        (UXStrings.From_UTF_8 ("http://127.0.0.1:"
                               & Ada.Strings.Fixed.Trim (Port'Image, Ada.Strings.Both)));
      --  Before Initialize, following Gnoga.Application
      --  recommendations for browser launchers.

      Gnoga.Application.Multi_Connect.Initialize
        (Event => On_Connect'Unrestricted_Access, Port => Port);
      --  The session port. The connection handler builds the
      --  debugger page; the message loop runs in its own task, Run
      --  waits for the run task completion instead.

      Set_Controller (The_Controller'Access);

      The_Server := new Server_Task;
      --  From here on, the browser gets a listening socket on 8080.

      The_Run := new Run_Task;
      UI_State.Wait_Done;
      --  The session is over: whatever the state, the run task is
      --  done, the debugger browser page is closed or will be closed
      --  by the process exit.

      Gnoga.Application.Multi_Connect.End_Application;
      --  Serves Stop to the server task: closes the listener, releases
      --  Server_Wait, and lets the message loop task terminate.
   end Run;

end BBT.GUI;
