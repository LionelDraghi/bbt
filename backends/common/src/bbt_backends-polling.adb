-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Calendar;

package body BBT_Backends.Polling is

   function Eventually (Timeout : Duration := Harness_Deadline)
                        return Boolean is
      use Ada.Calendar;
      Deadline : constant Time := Clock + Timeout;
      Found    : Boolean := False;
   begin
      loop
         Found := Attempt;
         exit when Found;
         exit when Clock >= Deadline;
         delay Poll_Interval;
      end loop;
      return Found;
   end Eventually;

   function Eventually_Get (Timeout : Duration := Harness_Deadline)
                            return Boolean is
      use Ada.Calendar;
      Deadline : constant Time := Clock + Timeout;
   begin
      loop
         if Present then
            return Observed;
         end if;
         exit when Clock >= Deadline;
         delay Poll_Interval;
      end loop;
      return False;
   end Eventually_Get;

end BBT_Backends.Polling;
