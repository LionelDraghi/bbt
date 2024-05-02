# ------------------------------------------------------------------------------
# smk, the smart make (http://lionel.draghi.free.fr/smk/)
#  © 2018 Lionel Draghi <lionel.draghi@free.fr>
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

.SILENT:
all: build check

build:
	alr build
	cd tests/uut && alr build

check: bbt
	# We run one by one scenario with test that should fail

	./bbt tests/file_is_string.md > output.1
	sdiff -sbB expected.1 output.1 || meld expected.1 output.1

	rm config.ini
	./bbt tests/file_test_and_creation.md > output.2
	sdiff -sbB expected.2 output.2 || meld expected.2 output.2

	./bbt tests/successfully.md > output.3
	sdiff -sbB expected.3 output.3 || meld expected.3 output.3

	./bbt tests/contains_multiline.md > output.4
	sdiff -sbB expected.4 output.4 || meld expected.4 output.4

	# then we run all scenarii that should pass

	./bbt tests/stderr.md tests/return_code.md tests/stdout.md tests/contains_line.md tests/as_in.md tests/robustness.md

install: bbt
	cp -p bbt ~/bin

.PHONY : clean
clean:
	alr clean
	cd tests/uut && alr clean
	- rm output.* tests/*.out config.ini
	
