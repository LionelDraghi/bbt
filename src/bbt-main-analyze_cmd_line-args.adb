-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Command_Line;

separate (BBT.Main.Analyze_Cmd_Line)

package body Args is

   -- --------------------------------------------------------------------------
   procedure Go_Next_Arg is
   begin
      Arg_Counter := @ + 1;
      -- Put_Line ("Go_Next " & Arg_Counter'Image);
   end Go_Next_Arg;

   -- --------------------------------------------------------------------------
   procedure Go_Previous_Arg is
   begin
      Arg_Counter := @ - 1;
      -- Put_Line ("Go_Previous_Arg " & Arg_Counter'Image);
   end Go_Previous_Arg;

   -- --------------------------------------------------------------------------
   function On_Last_Arg return Boolean is
      L : constant Boolean := Arg_Counter = Ada.Command_Line.Argument_Count;
   begin
      -- Put_Line ("Last " & L'Image);
      return L;
   end On_Last_Arg;

   -- --------------------------------------------------------------------------
   function Current_Arg return String is
      S : constant String := Ada.Command_Line.Argument (Arg_Counter);
   begin
      -- Put_Line ("Current " & S'Image);
      return S;
   end Current_Arg;

   -- --------------------------------------------------------------------------
   function More_Args return Boolean is
      B : constant Boolean := Arg_Counter <= Ada.Command_Line.Argument_Count;
   begin
      -- Put_Line ("More_Arg " & B'Image);
      return B;
   end More_Args;


end Args;
