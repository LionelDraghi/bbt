-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Command_Line;

separate (BBT.Main.Analyze_Cmd_Line)

package body Arg is

   -- --------------------------------------------------------------------------
   procedure Go_Next is
   begin
      Arg_Counter := @ + 1;
      -- Put_Line ("Go_Next " & Arg_Counter'Image);
   end Go_Next;

   -- --------------------------------------------------------------------------
   procedure Go_Back is
   begin
      Arg_Counter := @ - 1;
      -- Put_Line ("Go_Back " & Arg_Counter'Image);
   end Go_Back;

   -- --------------------------------------------------------------------------
   function Last return Boolean is
      L : constant Boolean := Arg_Counter = Ada.Command_Line.Argument_Count;
   begin
      -- Put_Line ("Last " & L'Image);
      return L;
   end Last;

   -- --------------------------------------------------------------------------
   function Current return String is
      S : constant String := Ada.Command_Line.Argument (Arg_Counter);
   begin
      -- Put_Line ("Current " & S'Image);
      return S;
   end Current;

   -- --------------------------------------------------------------------------
   function More_Arg return Boolean is
      B : constant Boolean := Arg_Counter <= Ada.Command_Line.Argument_Count;
   begin
      -- Put_Line ("More_Arg " & B'Image);
      return B;
   end More_Arg;


end Arg;
