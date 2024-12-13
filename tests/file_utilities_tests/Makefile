# ------------------------------------------------------------------------------
# smk, the smart make (http://lionel.draghi.free.fr/smk/)
#  © 2018, 2019 Lionel Draghi <lionel.draghi@free.fr>
# SPDX-License-Identifier: APSL-2.0
# ------------------------------------------------------------------------------
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# ------------------------------------------------------------------------------


.PHONY : clean test_file_utilities

all: check doc

test_file_utilities:
	## gprbuild -q --create-missing-dirs -P test_file_utilities.gpr
	alr build

check: test_file_utilities
	@ ./test_file_utilities > ../../docs/tests/test_file_utilities.md && echo 3. File_Utilities unit tests OK ========== || echo 3. File_Utilities unit tests KO "******"
	@ echo
	
doc:

clean:
	## - gprclean -q -P test_file_utilities.gpr
	- alr clean
	- rm testrec.md 
