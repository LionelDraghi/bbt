-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
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
      --  Only allow setting a verb for the first time. The run verb
      --  qualifiers ("shell", "successfully") and the negation words
      --  are not distinct verbs: they set flags instead (cf. Parse).
      if Current_Verb = No_Verb then
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
      Put_Debug_Line ("Set_Verb (" & Image (V) & ")",
                      Location => Loc);
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