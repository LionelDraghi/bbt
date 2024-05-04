-- -----------------------------------------------------------------------------
-- BBT, the BlackBox tester (http://lionel.draghi.free.fr/BBT/)
-- © 2018, 2019 Lionel Draghi <lionel.draghi@free.fr>
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- http://www.apache.org/licenses/LICENSE-2.0
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: BBT specification
--
-- Purpose:
--    This package is empty. Child units do the real job :
--
--    procedure <BBT.Main> - is in charge of
--       controlling the execution flow according to the command line
--    package <BBT.Cmd_Line> - do the command line analysis
--    package <BBT.Settings> - global settings, resulting mainly from
--       cmd line analysis (and env. variables in the future)
--    ...
--
-- Effects:
--
-- Limitations:
--
-- Performance:
--
-- -----------------------------------------------------------------------------

package BBT is

   pragma Pure;

end BBT;
