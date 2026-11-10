-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- -----------------------------------------------------------------------------

--  Execution events and interactive control.
--
--  The runtime emits an event before and after each executable step,
--  and around the whole run.
--
--  Once a controller is registered with Set_Controller, the runtime
--  stops by default at every halt point, i.e. before each executable
--  step, before each scenario, feature or file, and before finishing.
--  A stopped runtime is released by a Continue command, and runs up
--  to the next halt point of the given scope. Until a controller is
--  registered, no halt point ever stops, and execution stays purely
--  sequential.
--
--  The Pause_On_Error switch selects how a failed step is treated:
--  when set (the default), the runtime stops right after the failed
--  step, as a debugger would; when clear, execution proceeds as with
--  the --keep_going option.
--
--  Finish differs from Continue to the last halt point: it releases
--  the current stop and ignores any further halt point, running the
--  remaining execution in keep going mode.

with BBT.IO;

with Ada.Strings.Unbounded;
with Text_Utilities;

private package BBT.Execution_Control is

   type Event_Kind is (Execution_Started, Before_File, Before_Feature,
                       Before_Scenario, Before_Step, After_Step,
                       Execution_Finished);

   type Event is record
      Kind     : Event_Kind;
      Location : BBT.IO.Location_Type := BBT.IO.No_Location;
      Failed   : Boolean := False;
      Label    : Ada.Strings.Unbounded.Unbounded_String :=
                 Ada.Strings.Unbounded.Null_Unbounded_String;
      Feature  : Ada.Strings.Unbounded.Unbounded_String :=
                 Ada.Strings.Unbounded.Null_Unbounded_String;
      --  Plain text attached to progress events, e.g. the current
      --  scenario on Before_Scenario and step events, empty otherwise.
   end record;

   type Scope is (Next_Step, Next_Scenario, Next_Feature, Next_File);

   type Controller is limited interface;
   --  Implemented by the controller clients of the runtime (the
   --  debugger window, a script, ...). An interface, so that clients
   --  outside this package can implement it.

   procedure Handle (C : in out Controller; E : Event) is abstract;

   procedure Set_Controller (C : not null access Controller'Class);
   --  Register a controller; the designated object must outlive the
   --  registration (a library level object). Passing another
   --  controller replaces the previous one. This actually switches
   --  the runtime to interactive mode.

   procedure Notify (E : Event);
   --  Dispatch E to the registered controller, if any.

   --  Source texts, read once by the runtime at document analysis time,
   --  and kept in memory so that display clients never need file access
   --  (bbt itself may hold the scenario files open).

   procedure Set_Source (File_Name : String; Content : Text_Utilities.Text);
   --  Store the verbatim content of the analyzed file File_Name, keyed
   --  on the normalized name that locations carry.

   function Get_Source (File_Name : String) return Text_Utilities.Text;
   --  The stored content of File_Name, or an empty text if unknown.

   --  Commands, to be issued by the controller, typically in reaction
   --  to an event:

   procedure Continue (To : Scope := Next_Step);
   --  Release the current stop, and run until the next halt point of
   --  the given scope: next executable step, scenario, feature or
   --  file.

   procedure Finish;
   --  Runs everything that is left to completion, ignoring halt
   --  points: failures no longer stop the execution, i.e. this runs
   --  the rest in keep going mode.

   procedure Set_Pause_On_Error (On : Boolean);
   --  When On (the default), the runtime stops right after a failed
   --  step. When Off, failed steps are treated as with --keep_going.

   function Pause_On_Error return Boolean;
   --  Current setting of the above switch.

   --  Halt points, called by the runtime:

   function Controls_Execution return Boolean;
   --  True as soon as a controller is registered: the runtime then
   --  bypasses its own --stop_on_error / --keep_going policy, which
   --  is now governed by the controller (Continue, Finish, and the
   --  Pause_On_Error switch).

   function Stopping_At (Where : Scope) return Boolean;
   --  True if a halt point of the given scope would stop: i.e. the run
   --  target matches, and Finish was issued. This lets the runtime
   --  emit only the progress events that a stop will actually display,
   --  instead of flashing the display while running through halt
   --  points.

   function Stopping_On_Error return Boolean;
   --  True if a failed step would halt the run right after it: i.e.
   --  Finish was not issued, and Pause_On_Error is set.

   procedure Halt (Where : Scope);
   --  A halt point: when a controller is registered, and running was
   --  not released with Finish, this blocks until the controller
   --  issues a Continue or Finish command. Without a registered
   --  controller, it returns immediately.

   procedure Halt_On_Error;
   --  A halt point that only stops when the Pause_On_Error switch is
   --  set, and Finish was not issued: the debugger stops after a
   --  failed step.

end BBT.Execution_Control;
