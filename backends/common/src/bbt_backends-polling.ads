-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

--  Bounded polling (the "eventually" semantics of the GUI harness):
--  GUI state changes are asynchronous, no GUI check may be performed
--  once: every backend check polls within the harness deadline.

package BBT_Backends.Polling is

   generic
      with function Attempt return Boolean;
      --  One attempt at observing the expected state: read without
      --  parameter, in the backend's own state (the checks run
      --  sequentially, the harness is single threaded)
   function Eventually (Timeout : Duration := Harness_Deadline)
                        return Boolean;
   --  Calls Attempt once, re-tries every Poll_Interval until it
   --  yields True, and gives up after Timeout seconds. What has been
   --  observed meanwhile (titles, text...) stays available to the
   --  backend for the failure diagnostics.

   generic
      with function Present return Boolean;
      --  The target state: polling stops there
      with function Observed return Boolean;
      --  The outcome, read once Present holds
   function Eventually_Get (Timeout : Duration := Harness_Deadline)
                            return Boolean;
   --  Polls until Present yields True, then returns Observed: the
   --  presence is waited for (the widget tree builds asynchronously),
   --  the outcome is reported as observed at that moment and is not
   --  re-polled (e.g. a press action must not be replayed on retry)

end BBT_Backends.Polling;
