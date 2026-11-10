-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------

with Ada.Containers.Indefinite_Ordered_Maps;

package body BBT.Execution_Control is

   use type Text_Utilities.Text;

   The_Controller : access Controller'Class := null;

   --  The verbatim source texts of the analyzed files, keyed on the
   --  normalized file names that locations carry (case sensitive: both
   --  the store key and the lookup keys come from the same location
   --  normalization).
   package Source_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => String,
        Element_Type => Text_Utilities.Text);

   protected Source_Store is
      procedure Set (File_Name : String; Content : Text_Utilities.Text);
      function  Get (File_Name : String) return Text_Utilities.Text;
   private
      Sources : Source_Maps.Map;
   end Source_Store;

   protected body Source_Store is

      procedure Set (File_Name : String; Content : Text_Utilities.Text) is
      begin
         Sources.Include (Key      => File_Name,
                          New_Item => Content);
      end Set;

      function Get (File_Name : String) return Text_Utilities.Text is
      begin
         if Sources.Contains (File_Name) then
            return Sources.Element (File_Name);
         else
            return Text_Utilities.Empty_Text;
         end if;
      end Get;

   end Source_Store;

   -- --------------------------------------------------------------------------
   procedure Set_Source (File_Name : String;
                         Content   : Text_Utilities.Text) is
   begin
      Source_Store.Set (File_Name, Content);
   end Set_Source;

   -- --------------------------------------------------------------------------
   function Get_Source (File_Name : String) return Text_Utilities.Text is
     (Source_Store.Get (File_Name));

   protected Control_State is
      procedure Registered;
      procedure Do_Continue (To : Scope);
      procedure Do_Finish;
      procedure Set_Pause_On_Err (On : Boolean);
      procedure Set_Stopped;
      function  Mode_Interactive  return Boolean;
      function  Stopped           return Boolean;
      function  Pending_For (Where : Scope) return Boolean;
      function  Halt_Errors       return Boolean;
      function  Paused_On_Error   return Boolean;
   private
      Interactive       : Boolean := False;
      Run_To            : Scope   := Next_Step;
      Finished          : Boolean := False;
      Paused_On_Err     : Boolean := True;
      We_Are_Stopped    : Boolean := False;
   end Control_State;

   protected body Control_State is

      procedure Registered is
      begin
         Interactive := True;
      end Registered;

      procedure Do_Continue (To : Scope) is
      begin
         Run_To         := To;
         We_Are_Stopped := False;
      end Do_Continue;

      procedure Do_Finish is
      begin
         Finished       := True;
         We_Are_Stopped := False;
      end Do_Finish;

      procedure Set_Pause_On_Err (On : Boolean) is
      begin
         Paused_On_Err := On;
      end Set_Pause_On_Err;

      procedure Set_Stopped is
      begin
         We_Are_Stopped := True;
      end Set_Stopped;

      function Mode_Interactive return Boolean is (Interactive);

      function Stopped return Boolean is (We_Are_Stopped);

      function Pending_For (Where : Scope) return Boolean is
        ((not Finished) and then (Where = Run_To));

      function Halt_Errors return Boolean is
        ((not Finished) and then Paused_On_Err);

      function Paused_On_Error return Boolean is (Paused_On_Err);

   end Control_State;

   -- --------------------------------------------------------------------------
   procedure Set_Controller (C : not null access Controller'Class) is
   begin
      The_Controller := C.all'Access;
      Control_State.Registered;
      --  Run_To is initialized to Next_Step: the runtime will stop at
      --  the first executable step, unless the controller issues a
      --  Continue command first.
   end Set_Controller;

   -- --------------------------------------------------------------------------
   procedure Notify (E : Event) is
   begin
      if The_Controller /= null then
         The_Controller.all.Handle (E);
      end if;
   end Notify;

   -- --------------------------------------------------------------------------
   procedure Continue (To : Scope := Next_Step) is
   begin
      Control_State.Do_Continue (To);
   end Continue;

   -- --------------------------------------------------------------------------
   procedure Finish is
   begin
      Control_State.Do_Finish;
   end Finish;

   -- --------------------------------------------------------------------------
   procedure Set_Pause_On_Error (On : Boolean) is
   begin
      Control_State.Set_Pause_On_Err (On);
   end Set_Pause_On_Error;

   -- --------------------------------------------------------------------------
   function Pause_On_Error return Boolean is (Control_State.Paused_On_Error);

   -- --------------------------------------------------------------------------
   function Stopping_At (Where : Scope) return Boolean is
     (Control_State.Mode_Interactive
      and then Control_State.Pending_For (Where));

   -- --------------------------------------------------------------------------
   function Stopping_On_Error return Boolean is
     (Control_State.Halt_Errors);

   -- --------------------------------------------------------------------------
   function Controls_Execution return Boolean is (Control_State.Mode_Interactive);

   -- --------------------------------------------------------------------------
   procedure Halt (Where : Scope) is
   begin
      if not Control_State.Mode_Interactive then
         return;
      end if;

      if Control_State.Pending_For (Where) then
         Control_State.Set_Stopped;
         while Control_State.Stopped loop
            delay 0.005;
         end loop;
      end if;
   end Halt;

   -- --------------------------------------------------------------------------
   procedure Halt_On_Error is
   begin
      if Control_State.Mode_Interactive
        and then Control_State.Halt_Errors
      then
         Control_State.Set_Stopped;
         while Control_State.Stopped loop
            delay 0.005;
         end loop;
      end if;
   end Halt_On_Error;

end BBT.Execution_Control;
