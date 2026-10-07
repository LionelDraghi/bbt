-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Writers;                       use BBT.Writers;

with Ada.Containers.Indefinite_Vectors;
with Ada.Environment_Variables;

use Ada;

package body BBT.Tests.Actions.Environment is

   procedure Put_Debug_Line
     (Item      : String;
      Location  : IO.Location_Type    := IO.No_Location;
      Verbosity : IO.Verbosity_Levels := IO.Debug;
      Topic     : IO.Extended_Topics  := IO.Step_Actions)
      renames IO.Put_Line;
   pragma Warnings (Off, Put_Debug_Line);
   -- scenario, with the value (or the absence) they had before, so that
   -- Restore_Environment can give them back.
   type Saved_Variable (Name_Length, Value_Length : Natural) is record
      Name    : String (1 .. Name_Length);
      Present : Boolean;
      Value   : String (1 .. Value_Length);
   end record;

   package Saved_Variable_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Saved_Variable);
   Saved_Variables : Saved_Variable_Lists.Vector;

   procedure Remember (Name : String) is
   begin
      for V of Saved_Variables loop
         if V.Name = Name then
            return; -- keep the value from before the first change
         end if;
      end loop;
      if Environment_Variables.Exists (Name) then
         declare
            Old : constant String := Environment_Variables.Value (Name);
         begin
            Saved_Variables.Append
              (Saved_Variable'(Name_Length  => Name'Length,
                Value_Length => Old'Length,
                Name         => Name,
                Present      => True,
                Value        => Old));
         end;
      else
         Saved_Variables.Append
           (Saved_Variable'(Name_Length  => Name'Length,
             Value_Length => 0,
             Name         => Name,
             Present      => False,
             Value        => ""));
      end if;
   end Remember;

   -- --------------------------------------------------------------------------
   procedure Set_Env_Var (Step      : Step_Type'Class;
                          Verbosity : Verbosity_Levels) is
      Name  : constant String := +Step.Data.Subject_String;
      Value : constant String := +Step.Data.Object_String;
      OK    : Boolean := True;
   begin
      Put_Debug_Line ("Set_Env_Var " & Name & " = " & Value);
      Remember (Name);
      begin
         Environment_Variables.Set (Name, Value);
      exception
         when Constraint_Error | Program_Error =>
            -- Illegal name (empty or with '=') or rejected by the OS
            OK := False;
      end;
      Put_Step_Result (Step      => Step,
                       Success   => OK,
                       Fail_Msg  => "Unable to set environment variable "
                                    & Name'Image,
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Set_Env_Var;

   -- --------------------------------------------------------------------------
   procedure Unset_Env_Var (Step      : Step_Type'Class;
                            Verbosity : Verbosity_Levels) is
      Name : constant String := +Step.Data.Subject_String;
      OK   : Boolean := True;
   begin
      Put_Debug_Line ("Unset_Env_Var " & Name);
      Remember (Name);
      begin
         Environment_Variables.Clear (Name);
      exception
         when Constraint_Error | Program_Error =>
            OK := False;
      end;
      Put_Step_Result (Step      => Step,
                       Success   => OK,
                       Fail_Msg  => "Unable to unset environment variable "
                                    & Name'Image,
                       Loc       => Step.Location,
                       Verbosity => Verbosity);
   end Unset_Env_Var;

   -- --------------------------------------------------------------------------
   procedure Restore_Environment is
   begin
      for V of Saved_Variables loop
         if V.Present then
            Environment_Variables.Set (V.Name, V.Value);
         else
            Environment_Variables.Clear (V.Name);
         end if;
      end loop;
      Saved_Variables.Clear;
   end Restore_Environment;

end BBT.Tests.Actions.Environment;
