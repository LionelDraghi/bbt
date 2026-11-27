-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

--  ToDoMVC, the Adi2 projection (cf. ../ToDoMVC.md): the third witness
--  of the shared GUI suite, driven over Adi2's own development bridge
--  (Adi.MCP, file IPC) instead of the accessibility bus, cf.
--  ../../../backends/adi2.
--
--  Widget naming, per the conventions the bridge allows (it serializes
--  one text field per widget, no accessible-name channel): a text
--  widget is named by its text — a button is what it displays — and a
--  widget carrying none is named by the label that immediately
--  precedes it in tree order, which is how the two fields and the two
--  lists are named. The window title is the root widget's own label.
--
--  Layout: the creation line and the filters on top, the two lists in
--  the middle, the footer at the bottom. The selection controls
--  (Complete, Edit, Delete) live in a bar of their own rather than in
--  the footer: the spec hides the footer when there is no todo, and
--  they have to stay exposed — disabled — on a fresh app.
--
--  Probe activation: the bridge is a development-profile build of
--  Adi2. This application calls Adi.MCP.Initialize, a no-op in
--  validation and release builds, which compile the documented stub.
-- -----------------------------------------------------------------------------

pragma Ada_2022;

with Ada.Strings.Unbounded;

with Adi.App;
with Adi.CSS_Styles;         use Adi.CSS_Styles;
with Adi.Layout_Util;
with Adi.MCP;
with Adi.Widget;             use Adi.Widget;
with Adi.Widget.Box;         use Adi.Widget.Box;
with Adi.Widget.Button;      use Adi.Widget.Button;
with Adi.Widget.Label;       use Adi.Widget.Label;
with Adi.Widget.List_Box;
with Adi.Widget.Text_Input;
with Adi.Widget_Styles;      use Adi.Widget_Styles;
with Adi.Window;             use Adi.Window;

with TodoMVC_Fields;
with TodoMVC_Model;
with TodoMVC_Store;

procedure TodoMVC is

   package Label_List is new Adi.Widget.List_Box
     (Adi.Widget.Label.Label_Widget);

   use type TodoMVC_Fields.Field_Handle;
   use TodoMVC_Model;

   Max_Todos : constant := 256;

   type Index_Array is array (1 .. Max_Todos) of Natural;

   --  Palette: a sober light theme, the two todo states told apart by
   --  colour (blue to do, green done), as the spec asks.

   Canvas   : constant Color_Value := RGB (250, 250, 250);
   Panel    : constant Color_Value := RGB (238, 240, 243);
   Field_Bg : constant Color_Value := RGB (255, 255, 255);
   Ink      : constant Color_Value := RGB (33, 37, 41);
   Ink_Dim  : constant Color_Value := RGB (108, 117, 125);
   Edge     : constant Color_Value := RGB (206, 212, 218);
   Todo_Ink : constant Color_Value := RGB (25, 90, 200);
   Done_Ink : constant Color_Value := RGB (32, 122, 60);

   App         : Adi.App.App;
   Live_Window : Window_Handle := Null_Window_Handle;

   Root          : Adi.Widget.Box.Box_Handle;
   Creation_Row  : Adi.Widget.Box.Box_Handle;
   New_Label     : Label_Handle;
   New_Task      : TodoMVC_Fields.Field_Handle;
   Create_Btn    : Button_Handle;

   Filter_Row    : Adi.Widget.Box.Box_Handle;
   Filter_Button : array (Filter_Kind) of Button_Handle;

   Edit_Row      : Adi.Widget.Box.Box_Handle;
   Edit_Label    : Label_Handle;
   Edit_Task     : TodoMVC_Fields.Field_Handle;

   Selection_Row : Adi.Widget.Box.Box_Handle;
   Complete_Btn  : Button_Handle;
   Edit_Btn      : Button_Handle;
   Delete_Btn    : Button_Handle;

   Lists         : Adi.Widget.Box.Box_Handle;
   To_Do_Section : Adi.Widget.Box.Box_Handle;
   Done_Section  : Adi.Widget.Box.Box_Handle;
   To_Do_Title   : Label_Handle;
   Done_Title    : Label_Handle;
   To_Do_List    : Label_List.List_Box_Handle;
   Done_List     : Label_List.List_Box_Handle;

   Footer        : Adi.Widget.Box.Box_Handle;
   Mark_All_Btn  : Button_Handle;
   Counter       : Label_Handle;
   Clear_Btn     : Button_Handle;

   Todos    : Todo_List;
   Selected : Natural := 0;
   --  The selected todo, by position in the model (0 = no selection)
   Editing  : Natural := 0;
   --  The todo whose edition is open (0 = no edition)

   To_Do_Rows : Index_Array := [others => 0];
   Done_Rows  : Index_Array := [others => 0];
   To_Do_Last : Natural := 0;
   Done_Last  : Natural := 0;
   --  The model index of each list row, for the selection mapping

   Refreshing : Boolean := False;

   --  Styles ---------------------------------------------------------------

   procedure Set_Dress
     (H : Widget_Handle;
      P : Part_Kind;
      Base, Hover, Press, Focus : Style_Rules) is
   begin
      Set_Part_Style
        (H, P,
         Create.Base (Base).On_Hover (Hover).On_Press (Press)
                    .On_Focus (Focus).Build);
   end Set_Dress;

   function Surface (Background : Color_Value) return Style_Rules is
     (Empty_Style with Delta
        Background_Color => Opt_Bg_Color.Val (Background),
        Border_Width     => [others => Opt_Length.Val (Px (1.0))],
        Border_Color     => [others => Opt_Edge_Color.Val (Edge)],
        Border_Radius    => [others => Opt_Length.Val (Px (6.0))],
        Padding          => [others => Opt_Length.Val (Px (8.0))],
        Gap              => Opt_Gap.Val (Gap (Px (8.0))));

   function List_Surface (Tint : Color_Value) return Style_Rules is
     (Surface (Field_Bg) with Delta
        Border_Color => [others => Opt_Edge_Color.Val (Tint)],
        Height       => Opt_Size.Val (Size (Px (150.0))),
        Min_Height   => Opt_Size.Val (Size (Px (60.0))));

   function No_Title return Style_Rules is
     (Empty_Style with Delta Color => Opt_Text_Color.Val (RGBA (0, 0, 0, 0.0)));
   --  The root's label is the only channel the bridge gives the window
   --  title: it is declared, never displayed, since the window
   --  decoration already carries it

   function Row_Style return Style_Rules is
     (Empty_Style with Delta
        Display        => Opt_Display.Val (Flex),
        Flex_Direction => Opt_Flex_Dir.Val (Adi.CSS_Styles.Row),
        Align_Items    => Opt_Align_Items.Val (Center),
        Gap            => Opt_Gap.Val (Gap (Px (10.0))));

   function Root_Style return Style_Rules is
     (Empty_Style with Delta
        Display          => Opt_Display.Val (Flex),
        Flex_Direction   => Opt_Flex_Dir.Val (Adi.CSS_Styles.Column),
        Background_Color => Opt_Bg_Color.Val (Canvas),
        Padding          => [others => Opt_Length.Val (Px (16.0))],
        Gap              => Opt_Gap.Val (Gap (Px (10.0))));

   function Column_Style return Style_Rules is
     (Empty_Style with Delta
        Display        => Opt_Display.Val (Flex),
        Flex_Direction => Opt_Flex_Dir.Val (Adi.CSS_Styles.Column),
        Gap            => Opt_Gap.Val (Gap (Px (10.0))));

   procedure Dress_Text (H : Widget_Handle; Tint : Color_Value) is
      Rules : constant Style_Rules :=
        (Empty_Style with Delta Color => Opt_Text_Color.Val (Tint));
   begin
      Set_Dress (H, Label_Part, Rules, Empty_Style, Empty_Style, Empty_Style);
   end Dress_Text;

   procedure Dress_Button (B : Button_Handle; Active : Boolean := False) is
      Base : constant Style_Rules :=
        (Empty_Style with Delta
           Display          => Opt_Display.Val (Inline_Flex),
           Background_Color =>
             Opt_Bg_Color.Val ((if Active then Todo_Ink else Panel)),
           Color            =>
             Opt_Text_Color.Val ((if Active then Field_Bg else Ink)),
           Border_Width     => [others => Opt_Length.Val (Px (1.0))],
           Border_Color     => [others => Opt_Edge_Color.Val (Edge)],
           Border_Radius    => [others => Opt_Length.Val (Px (4.0))],
           Padding          =>
             [Top    => Opt_Length.Val (Px (6.0)),
              Right  => Opt_Length.Val (Px (14.0)),
              Bottom => Opt_Length.Val (Px (6.0)),
              Left   => Opt_Length.Val (Px (14.0))]);
   begin
      Set_Dress (Widget_Handle'(+B), Main_Part, Base, Empty_Style,
                 Empty_Style, Empty_Style);
      Dress_Text (Widget_Handle'(+B),
                  (if Active then Field_Bg else Ink));
   end Dress_Button;

   procedure Dress_Field (F : TodoMVC_Fields.Field_Handle) is
      Field : constant Widget_Handle := TodoMVC_Fields."+" (F);
      Base : constant Style_Rules :=
        (Empty_Style with Delta
           Background_Color => Opt_Bg_Color.Val (Field_Bg),
           Border_Width     => [others => Opt_Length.Val (Px (1.0))],
           Border_Color     => [others => Opt_Edge_Color.Val (Edge)],
           Border_Radius    => [others => Opt_Length.Val (Px (4.0))],
           Padding          =>
             [Top    => Opt_Length.Val (Px (6.0)),
              Right  => Opt_Length.Val (Px (10.0)),
              Bottom => Opt_Length.Val (Px (6.0)),
              Left   => Opt_Length.Val (Px (10.0))]);
      Focused : constant Style_Rules :=
        (Empty_Style with Delta
           Border_Color => [others => Opt_Edge_Color.Val (Todo_Ink)]);
      Text : constant Style_Rules :=
        (Empty_Style with Delta Color => Opt_Text_Color.Val (Ink));
      --  The cursor and the selection are panels whose colour is the
      --  part style and nothing else: an unstyled part paints nothing
      --  at all, so a field without these two shows neither a caret
      --  nor the text it selects
      Cursor : constant Style_Rules :=
        (Empty_Style with Delta Background_Color => Opt_Bg_Color.Val (Ink));
      Chosen : constant Style_Rules :=
        (Empty_Style with Delta
           Background_Color => Opt_Bg_Color.Val (RGBA (25, 90, 200, 0.22)));
   begin
      Set_Dress (Field, Main_Part, Base, Empty_Style, Empty_Style, Focused);
      Set_Dress (Field, Text_Part, Text, Empty_Style, Empty_Style, Empty_Style);
      Set_Dress (Field, Cursor_Part, Cursor,
                 Empty_Style, Empty_Style, Empty_Style);
      Set_Dress (Field, Selected_Part, Chosen,
                 Empty_Style, Empty_Style, Empty_Style);
   end Dress_Field;

   --  Refresh --------------------------------------------------------------

   procedure Refresh;
   --  The lists, the selection controls and the footer, from the model

   function Row_Label
     (Title : String; Done_State : Boolean) return Label_Handle
   is
      H : constant Label_Handle := Adi.Widget.Label.Create_Handle (Title);
   begin
      Dress_Text (Widget_Handle'(+H),
                  (if Done_State then Done_Ink else Todo_Ink));
      return H;
   end Row_Label;

   procedure Refresh is
      Disabled : constant Boolean :=
        Selected = 0 or else Selected > Length (Todos);
   begin
      if Refreshing then
         return;
      end if;
      Refreshing := True;

      Label_List.Clear_Rows (To_Do_List);
      Label_List.Clear_Rows (Done_List);
      To_Do_Last := 0;
      Done_Last  := 0;

      for I in 1 .. Length (Todos) loop
         if Visible (Todos, I) then
            declare
               Item : constant Todo := Todo_At (Todos, I);
               Row  : constant Label_Handle :=
                 Row_Label (Ada.Strings.Unbounded.To_String (Item.Title),
                            Item.Done);
            begin
               if Item.Done then
                  if Done_Last < Max_Todos then
                     Done_Last := Done_Last + 1;
                     Done_Rows (Done_Last) := I;
                     Label_List.Append_Row (Done_List, Widget_Handle'(+Row));
                  end if;
               else
                  if To_Do_Last < Max_Todos then
                     To_Do_Last := To_Do_Last + 1;
                     To_Do_Rows (To_Do_Last) := I;
                     Label_List.Append_Row (To_Do_List, Widget_Handle'(+Row));
                  end if;
               end if;
            end;
         end if;
      end loop;

      --  The selection highlight follows the model selection
      for R in 1 .. To_Do_Last loop
         if To_Do_Rows (R) = Selected then
            Label_List.Select_Row (To_Do_List, R);
         end if;
      end loop;
      for R in 1 .. Done_Last loop
         if Done_Rows (R) = Selected then
            Label_List.Select_Row (Done_List, R);
         end if;
      end loop;

      Set_Disabled (Widget_Handle'(+Complete_Btn), Disabled);
      Set_Disabled (Widget_Handle'(+Edit_Btn), Disabled);
      Set_Disabled (Widget_Handle'(+Delete_Btn), Disabled);

      Adi.Widget.Label.Set_Text (Counter, Counter_Text (Todos));
      Set_Visible (Widget_Handle'(+Footer), Length (Todos) > 0);

      for K in Filter_Kind loop
         Dress_Button (Filter_Button (K), Active => Filter (Todos) = K);
      end loop;

      Refreshing := False;

      if Live_Window /= Null_Window_Handle then
         Request_Redraw (Live_Window);
      end if;
   end Refresh;

   procedure Changed is
   begin
      TodoMVC_Store.Save (Todos);
      Refresh;
   end Changed;

   --  Edition --------------------------------------------------------------

   procedure Commit_Edition is
      Edited : constant Natural := Editing;
      Text   : constant String :=
        Adi.Widget.Text_Input.Get_Text
          (TodoMVC_Fields.As_Text_Input (Edit_Task));
   begin
      if Edited = 0 then
         return;
      end if;
      Editing := 0;
      Set_Visible (Widget_Handle'(+Edit_Row), False);
      Set_Title (Todos, Edited, Text);
      --  An empty title destroys the todo: the selection goes with it
      if Selected > Length (Todos) then
         Selected := 0;
      end if;
      Changed;
   end Commit_Edition;

   procedure Start_Edition is
      Item : constant Todo := Todo_At (Todos, Selected);
   begin
      Editing := Selected;
      Adi.Widget.Text_Input.Set_Text
        (TodoMVC_Fields.As_Text_Input (Edit_Task),
         Ada.Strings.Unbounded.To_String (Item.Title));
      Set_Visible (Widget_Handle'(+Edit_Row), True);
      Set_Focused (Widget_Handle'(+Edit_Task));
      Request_Redraw (Live_Window);
   end Start_Edition;

   procedure Selection_Changed (To_Do : Boolean);
   --  The list selection moved: To_Do = True for the `To do` list

   procedure Selection_Changed (To_Do : Boolean) is
      List   : constant Label_List.List_Box_Handle :=
        (if To_Do then To_Do_List else Done_List);
      Other  : constant Label_List.List_Box_Handle :=
        (if To_Do then Done_List else To_Do_List);
      Last   : constant Natural := (if To_Do then To_Do_Last else Done_Last);
      Chosen : Natural := 0;
   begin
      if Refreshing then
         return;
      end if;
      for R in 1 .. Last loop
         if Label_List.Is_Row_Selected (List, R) then
            Chosen := (if To_Do then To_Do_Rows (R) else Done_Rows (R));
         end if;
      end loop;
      if Chosen = 0 or else Chosen = Selected then
         return;
      end if;
      --  Leaving a todo commits the open edition (a real user's blur)
      if Editing /= 0 then
         Commit_Edition;
      end if;
      Selected := Chosen;
      Label_List.Clear_Selection (Other);
      Refresh;
   end Selection_Changed;

   procedure On_To_Do_Selection (W : Widget_Handle) is
      pragma Unreferenced (W);
   begin
      Selection_Changed (True);
   end On_To_Do_Selection;

   procedure On_Done_Selection (W : Widget_Handle) is
      pragma Unreferenced (W);
   begin
      Selection_Changed (False);
   end On_Done_Selection;

   --  Callbacks ------------------------------------------------------------

   procedure On_Create (W : Widget_Handle) is
      pragma Unreferenced (W);
   begin
      Add (Todos,
           Adi.Widget.Text_Input.Get_Text
             (TodoMVC_Fields.As_Text_Input (New_Task)));
      Adi.Widget.Text_Input.Set_Text
        (TodoMVC_Fields.As_Text_Input (New_Task), "");
      Changed;
   end On_Create;

   procedure On_New_Task_Commit (Text : String) is
   begin
      Add (Todos, Text);
      Adi.Widget.Text_Input.Set_Text
        (TodoMVC_Fields.As_Text_Input (New_Task), "");
      Changed;
   end On_New_Task_Commit;

   procedure On_Edit_Commit (Text : String) is
      pragma Unreferenced (Text);
   begin
      Commit_Edition;
   end On_Edit_Commit;

   procedure On_Complete (W : Widget_Handle) is
      pragma Unreferenced (W);
   begin
      if Selected /= 0 and then Selected <= Length (Todos) then
         Toggle (Todos, Selected);
         Changed;
      end if;
   end On_Complete;

   procedure On_Edit (W : Widget_Handle) is
      pragma Unreferenced (W);
   begin
      if Editing /= 0 then
         --  Pressing Edit again cancels the edition, title untouched
         Editing := 0;
         Set_Visible (Widget_Handle'(+Edit_Row), False);
         Request_Redraw (Live_Window);
      elsif Selected /= 0 and then Selected <= Length (Todos) then
         Start_Edition;
      end if;
   end On_Edit;

   procedure On_Delete (W : Widget_Handle) is
      pragma Unreferenced (W);
   begin
      if Selected /= 0 and then Selected <= Length (Todos) then
         Delete (Todos, Selected);
         Selected := 0;
         Changed;
      end if;
   end On_Delete;

   procedure On_Mark_All (W : Widget_Handle) is
      pragma Unreferenced (W);
   begin
      Mark_All (Todos);
      Changed;
   end On_Mark_All;

   procedure On_Clear_Completed (W : Widget_Handle) is
      pragma Unreferenced (W);
   begin
      Clear_Completed (Todos);
      Selected := 0;
      Changed;
   end On_Clear_Completed;

   procedure On_All_Click (W : Widget_Handle) is
      pragma Unreferenced (W);
   begin
      Set_Filter (Todos, All_Filter);
      Selected := 0;
      Changed;
   end On_All_Click;

   procedure On_Active_Click (W : Widget_Handle) is
      pragma Unreferenced (W);
   begin
      Set_Filter (Todos, Active_Filter);
      Selected := 0;
      Changed;
   end On_Active_Click;

   procedure On_Completed_Click (W : Widget_Handle) is
      pragma Unreferenced (W);
   begin
      Set_Filter (Todos, Completed_Filter);
      Selected := 0;
      Changed;
   end On_Completed_Click;

   procedure Keep_Presented (DT : Duration) is
      pragma Unreferenced (DT);
   begin
      Request_Redraw (Live_Window);
   end Keep_Presented;

begin
   TodoMVC_Store.Load (Todos);

   App.Init;
   Adi.Layout_Util.Set_Px_Maps_To_Dip (True);
   App.Set_Target_FPS (60);

   declare
      W : constant Window_Handle :=
        Create_Window_Handle
          ("ToDoMVC", Adi.Window.Extent (Px (720.0), Px (560.0)));
   begin
      Root := Adi.Widget.Box.Create_Handle;
      --  The bridge serializes no window title: the root widget's own
      --  label is the one name-carrying channel it exposes (a box's
      --  text falls back to its declared label), so the title goes
      --  there, and stays invisible: a widget label is also painted,
      --  at the widget's own corner, which would duplicate in the body
      --  the title the window decoration already shows
      Set_Label (Widget_Handle'(+Root), "ToDoMVC");
      Set_Dress (Widget_Handle'(+Root), Main_Part, Root_Style,
                 Empty_Style, Empty_Style, Empty_Style);
      Set_Dress (Widget_Handle'(+Root), Label_Part, No_Title,
                 Empty_Style, Empty_Style, Empty_Style);

      --  Creation line: a label, the field it names, then Create
      Creation_Row := Adi.Widget.Box.Create_Handle;
      Set_Dress (Widget_Handle'(+Creation_Row), Main_Part, Row_Style,
                 Empty_Style, Empty_Style, Empty_Style);
      New_Label := Adi.Widget.Label.Create_Handle ("New task");
      Dress_Text (Widget_Handle'(+New_Label), Ink);
      New_Task := TodoMVC_Fields.Create_Handle;
      Dress_Field (New_Task);
      Create_Btn := Adi.Widget.Button.Create_Handle ("Create");
      Dress_Button (Create_Btn);
      Add_Child (Creation_Row, Widget_Handle'(+New_Label));
      Add_Child (Creation_Row, TodoMVC_Fields."+" (New_Task));
      Add_Child (Creation_Row, Widget_Handle'(+Create_Btn));
      Connect_Clicked (Create_Btn, On_Create'Unrestricted_Access);
      TodoMVC_Fields.Set_On_Commit
        (New_Task, On_New_Task_Commit'Unrestricted_Access);

      --  Filters: toggle controls, named by their own label
      Filter_Row := Adi.Widget.Box.Create_Handle;
      Set_Dress (Widget_Handle'(+Filter_Row), Main_Part, Row_Style,
                 Empty_Style, Empty_Style, Empty_Style);
      for K in Filter_Kind loop
         Filter_Button (K) :=
           Adi.Widget.Button.Create_Handle
             ((case K is
                 when All_Filter       => "All",
                 when Active_Filter    => "Active",
                 when Completed_Filter => "Completed"));
         Dress_Button (Filter_Button (K));
         Add_Child (Filter_Row, Widget_Handle'(+Filter_Button (K)));
      end loop;
      Connect_Clicked
        (Filter_Button (All_Filter), On_All_Click'Unrestricted_Access);
      Connect_Clicked
        (Filter_Button (Active_Filter), On_Active_Click'Unrestricted_Access);
      Connect_Clicked
        (Filter_Button (Completed_Filter),
         On_Completed_Click'Unrestricted_Access);

      --  Edition line: built once, shown while an edition is open
      Edit_Row := Adi.Widget.Box.Create_Handle;
      Set_Dress (Widget_Handle'(+Edit_Row), Main_Part, Row_Style,
                 Empty_Style, Empty_Style, Empty_Style);
      Edit_Label := Adi.Widget.Label.Create_Handle ("Edit todo");
      Dress_Text (Widget_Handle'(+Edit_Label), Ink);
      Edit_Task := TodoMVC_Fields.Create_Handle;
      Dress_Field (Edit_Task);
      Add_Child (Edit_Row, Widget_Handle'(+Edit_Label));
      Add_Child (Edit_Row, TodoMVC_Fields."+" (Edit_Task));
      TodoMVC_Fields.Set_On_Commit
        (Edit_Task, On_Edit_Commit'Unrestricted_Access);
      Set_Visible (Widget_Handle'(+Edit_Row), False);

      --  Selection controls: always exposed, insensitive with no selection
      Selection_Row := Adi.Widget.Box.Create_Handle;
      Set_Dress (Widget_Handle'(+Selection_Row), Main_Part, Row_Style,
                 Empty_Style, Empty_Style, Empty_Style);
      Complete_Btn := Adi.Widget.Button.Create_Handle ("Complete");
      Edit_Btn     := Adi.Widget.Button.Create_Handle ("Edit");
      Delete_Btn   := Adi.Widget.Button.Create_Handle ("Delete");
      Dress_Button (Complete_Btn);
      Dress_Button (Edit_Btn);
      Dress_Button (Delete_Btn);
      Add_Child (Selection_Row, Widget_Handle'(+Complete_Btn));
      Add_Child (Selection_Row, Widget_Handle'(+Edit_Btn));
      Add_Child (Selection_Row, Widget_Handle'(+Delete_Btn));
      Connect_Clicked (Complete_Btn, On_Complete'Unrestricted_Access);
      Connect_Clicked (Edit_Btn, On_Edit'Unrestricted_Access);
      Connect_Clicked (Delete_Btn, On_Delete'Unrestricted_Access);

      --  The two lists, each under the title that names it. The title
      --  is a widget of its own: a container's own label is painted
      --  inside its clipped content box, where it is cut by the frame
      --  and covers the first row
      Lists := Adi.Widget.Box.Create_Handle;
      Set_Dress (Widget_Handle'(+Lists), Main_Part, Column_Style,
                 Empty_Style, Empty_Style, Empty_Style);
      To_Do_Section := Adi.Widget.Box.Create_Handle;
      Done_Section  := Adi.Widget.Box.Create_Handle;
      Set_Dress (Widget_Handle'(+To_Do_Section), Main_Part, Column_Style,
                 Empty_Style, Empty_Style, Empty_Style);
      Set_Dress (Widget_Handle'(+Done_Section), Main_Part, Column_Style,
                 Empty_Style, Empty_Style, Empty_Style);
      To_Do_Title := Adi.Widget.Label.Create_Handle ("To do");
      Done_Title  := Adi.Widget.Label.Create_Handle ("Done");
      Dress_Text (Widget_Handle'(+To_Do_Title), Todo_Ink);
      Dress_Text (Widget_Handle'(+Done_Title), Done_Ink);
      To_Do_List := Label_List.Create_Handle;
      Done_List  := Label_List.Create_Handle;
      Label_List.Set_Selection_Mode (To_Do_List, Label_List.Single_Selection);
      Label_List.Set_Selection_Mode (Done_List, Label_List.Single_Selection);
      --  A list is a display zone with a minimal height (spec), its
      --  frame in the zone's own colour, and scrolls its rows past
      --  that: without a size the rows land outside their container
      Set_Dress
        (Label_List."+" (To_Do_List), Main_Part, List_Surface (Todo_Ink),
         Empty_Style, Empty_Style, Empty_Style);
      Set_Dress
        (Label_List."+" (Done_List), Main_Part, List_Surface (Done_Ink),
         Empty_Style, Empty_Style, Empty_Style);
      Label_List.Connect_Selection_Changed
        (To_Do_List, On_To_Do_Selection'Unrestricted_Access);
      Label_List.Connect_Selection_Changed
        (Done_List, On_Done_Selection'Unrestricted_Access);
      Add_Child (To_Do_Section, Widget_Handle'(+To_Do_Title));
      Add_Child (To_Do_Section, Label_List."+" (To_Do_List));
      Add_Child (Done_Section, Widget_Handle'(+Done_Title));
      Add_Child (Done_Section, Label_List."+" (Done_List));
      Add_Child (Lists, Widget_Handle'(+To_Do_Section));
      Add_Child (Lists, Widget_Handle'(+Done_Section));

      --  Footer: mark-all, the counter and the purge, side by side
      Footer := Adi.Widget.Box.Create_Handle;
      Set_Dress (Widget_Handle'(+Footer), Main_Part, Row_Style,
                 Empty_Style, Empty_Style, Empty_Style);
      Mark_All_Btn :=
        Adi.Widget.Button.Create_Handle ("Mark all as complete");
      Clear_Btn := Adi.Widget.Button.Create_Handle ("Clear completed");
      Counter   := Adi.Widget.Label.Create_Handle ("0 items left");
      Dress_Button (Mark_All_Btn);
      Dress_Button (Clear_Btn);
      Dress_Text (Widget_Handle'(+Counter), Ink_Dim);
      Add_Child (Footer, Widget_Handle'(+Mark_All_Btn));
      Add_Child (Footer, Widget_Handle'(+Counter));
      Add_Child (Footer, Widget_Handle'(+Clear_Btn));
      Connect_Clicked (Mark_All_Btn, On_Mark_All'Unrestricted_Access);
      Connect_Clicked (Clear_Btn, On_Clear_Completed'Unrestricted_Access);

      Add_Child (Root, Widget_Handle'(+Creation_Row));
      Add_Child (Root, Widget_Handle'(+Filter_Row));
      Add_Child (Root, Widget_Handle'(+Edit_Row));
      Add_Child (Root, Widget_Handle'(+Selection_Row));
      Add_Child (Root, Widget_Handle'(+Lists));
      Add_Child (Root, Widget_Handle'(+Footer));

      Set_Root (W, Widget_Handle'(+Root));
      Adi.MCP.Initialize (W);

      Live_Window := W;
      Refresh;
      --  A frame presents when it draws: ask for one every frame, so a
      --  change made through the bridge shows without a mouse motion
      Adi.Window.Connect_Tick (W, Keep_Presented'Unrestricted_Access);

      App.Add_Window (W);
      --  The creation line holds the keyboard on a fresh app (spec):
      --  the window-level focus, not the widget's own state only
      Adi.Window.Set_Focus (W, Widget_Handle'(+New_Task));
      App.Run;
      Adi.MCP.Finalize;
   end;
end TodoMVC;
