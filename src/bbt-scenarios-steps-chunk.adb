-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

separate (BBT.Scenarios.Steps)

package body Chunk is

   -- Internal state for chunk parsing
   Current_Verb      : Verbs          := No_Verb;
   Subject_Part_Ended : Boolean        := False;
   Object_Part_Ended  : Boolean        := False;

   -- --------------------------------------------------------------------------
   procedure Initialize is
   begin
      Current_Verb      := No_Verb;
      Subject_Part_Ended := False;
      Object_Part_Ended  := False;
   end Initialize;

   -- --------------------------------------------------------------------------
   procedure Set_Verb (V : Verbs; Loc : Location_Type := No_Location) is
   begin
      -- Original logic: only allow setting a verb for the first time,
      -- or changing to the negative form of the same verb
      if Current_Verb = No_Verb
        or else (Current_Verb = Get      and V = Get_No)
        or else (Current_Verb = Contains and V = Does_Not_Contain)
        or else (Current_Verb = Matches  and V = Does_Not_Match)
        or else (Current_Verb = Is_V     and V = Is_No)
      then
         -- Setting a verb for the first time, or changing it
         -- to the negative form is OK.
         Current_Verb := V;
         -- When a verb is set, the subject part is ended
         Subject_Part_Ended := True;
      else
         -- But if a different verb is detected after the current one,
         -- warn the user that the step wording may be ambiguous.
         IO.Put_Warning
           ("Verb is '" & Image (Current_Verb)
            & "', ignoring following '" & Image (V) & "'",
            Loc);
      end if;
   end Set_Verb;

   -- --------------------------------------------------------------------------
   function Verb return Verbs is
   begin
      return Current_Verb;
   end Verb;

   -- --------------------------------------------------------------------------
   function In_Subject_Part return Boolean is
   begin
      return not Subject_Part_Ended;
   end In_Subject_Part;

   -- --------------------------------------------------------------------------
   function In_Object_Part return Boolean is
   begin
      return Subject_Part_Ended and then not Object_Part_Ended;
   end In_Object_Part;

end Chunk;