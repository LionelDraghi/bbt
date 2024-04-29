-- -----------------------------------------------------------------------------
-- bbt, the BlackBox tester (http://lionel.draghi.free.fr/bbt/)
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

with bbt.Files.File_Lists;

procedure bbt.Files.Put (File_List     : File_Lists.Map;
                         Prefix        : String  := "";
                         Print_Sources : Boolean := False;
                         Print_Targets : Boolean := False;
                         Print_Unused  : Boolean := False);
-- Print files matching one of the boolean, in a one per line way.
-- If Settings.Filter_System_Files, then ignore
-- /lib /usr /etc /opt etc. files
