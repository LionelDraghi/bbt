PLATFORM = $(shell uname -s)
# Not used yet:
# ARCH := $(shell uname -m)
# PROC := $(shell uname -p)

ifeq ($(OS), Windows_NT)
	# PLATFORM := $(shell uname -s) -> should be Linux, Windows or Darwin
	# But is MSYS_NT-10.0-26100 on Windows, and I don't want the DIR name
	# to change with the MSYS or Windows version number.
	# This is why I override it on Windows:
	PLATFORM := Windows
	EXE_SUFFIX := .exe
	# On Windows, ln -fs silently falls back on a timestamp-preserving
	# copy: an explicit cp is more honest, and sets a fresh mtime at
	# each refresh, so that the staleness is observable
	REFRESH := cp -f
else
	EXE_SUFFIX :=
	REFRESH := ln -fs
endif

RESULTS_DIR := ../docs/tests_results/$(PLATFORM)

all: check

.PHONY : clean distclean setup check features examples non_reg unit_testing doc

# To keep the trace of the run on each platform, the test results
# are stored in a directory per platform (Linux, Windows, Darwin).
# Note that we can't just append the platform to the file name and put all
# the results in the same directory, because we need to separate what is 
# submitted to mlc (the tools to check the links in md files), otherwise it 
# will raise a lot of error on Windows files when run on Linux box, and vice
# versa. (and mlc command line is not flexible enough to run  
# "--ignore *_Windows.md")

build:
	@ $(MAKE) -s build --directory=file_utilities_tests

setup: ../sut$(EXE_SUFFIX) ../bbt$(EXE_SUFFIX)
	mkdir -p $(RESULTS_DIR)
    
	@$(REFRESH) ../sut$(EXE_SUFFIX) sut$(EXE_SUFFIX)
	@$(REFRESH) ../bbt$(EXE_SUFFIX) bbt$(EXE_SUFFIX)
	@$(REFRESH) ../rpl$(EXE_SUFFIX) rpl$(EXE_SUFFIX)

refresh_bbt: ../bbt$(EXE_SUFFIX)
	@$(REFRESH) ../bbt$(EXE_SUFFIX) bbt$(EXE_SUFFIX)
	# Called by the root build, so that a direct ./bbt run in tests/
	# after a rebuild never uses a stale binary
# Manage differences between Windows and Unix
ifeq ($(PLATFORM), Windows)
EXCLUDE := --exclude Unix_Only

else ifeq ($(PLATFORM), Darwin)
# Unindented on purpose: this is a make directive, exported to every recipe
# and its children (bbt and the spawned commands), not a recipe line.
# GNAT on a case-insensitive filesystem (default on macOS) otherwise
# lowercases file names at run time, breaking the self-tests.
# cf. https://forum.ada-lang.io/t/name-file-casing-error-on-darwin/1795
export GNAT_FILE_NAME_CASE_SENSITIVE=1
EXCLUDE := --exclude Windows_Only

else ifeq ($(PLATFORM), Linux)
# Seems that the PATH is not passed to spawned process on Linux :
	@ln -fs /usr/bin/gcc gcc
EXCLUDE = --exclude Windows_Only

else
	@echo "Unknown platform $(PLATFORM)"
	@exit 1

endif

features: setup
	@ # Then test all the features
	@ echo 1. features tests ================
	@ ./bbt --version
	./bbt --yes --keep_going \
	--exclude @Flaky     \
	--cleanup $(EXCLUDE) \
	-q --human_match \
	--generate_badge $(RESULTS_DIR)/badge.url   \
	--junit $(RESULTS_DIR)/features_results.xml \
	--index $(RESULTS_DIR)/features_results.md  \
	../docs/features 
	@ echo
	@ - rm -f verbose_output_NOK.md verbose_output_OK.md

examples: setup
	@ # Check examples
	@ echo 2. examples check ================
	@ ./bbt --version
	./bbt --yes -k -c -q -hm \
	--index $(RESULTS_DIR)/examples_results.md \
	../docs/examples ../docs/examples/gcc_hello_world.md || echo KO, check the availability of gcc on your platform "********"
	@ echo

non_reg: setup
	@ # Then run non reg tests
	@ echo 3. non regression tests ==========
	@ ./bbt --version
	./bbt --yes --keep_going --cleanup -q \
	--human_match \
	--index $(RESULTS_DIR)/non_reg_tests.md \
	../tests/non_reg_tests 
	@ echo

unit_testing:
	@ # Unit testing
	@ echo 4. unit testing ==================
	$(MAKE) -s check EXE_SUFFIX=$(EXE_SUFFIX) --directory=file_utilities_tests && echo OK || echo ***** KO
	@ echo

check: setup bbt$(EXE_SUFFIX) sut$(EXE_SUFFIX) features examples non_reg unit_testing
	@ # Some examples rely on external exe : 
	@ # - gcc

	@ # =====================================================================
	@ # WARNING : remember that bbt output file must be output directly in
	@ # the right directory, and NOT in the current directory and then moved to 
	@ # docs/test_results, otherwise relative path in the results file to the 
	@ # scenario files will be wrong!
	@ # =====================================================================
	
	@ - rm -f *.out *.err *.xml file1.tmp main cp tmp.txt output2.txt tmp.md index_*.md expected_grammar expected_keywords input.*
	@ $(MAKE) -s clean --directory=file_utilities_tests

doc: $(RESULTS_DIR)
	@ $(MAKE) -s doc DOC_DIR=../$(RESULTS_DIR) EXE_SUFFIX=$(EXE_SUFFIX) --directory=file_utilities_tests
	
	@ ../rpl --quiet bbt $(PLATFORM) $(RESULTS_DIR)/badge.url
	@ echo fetching badge ...
	@ curl -s "$$(cat $(RESULTS_DIR)/badge.url)" -o $(RESULTS_DIR)/badge.svg


# clean removes the test run artifacts only, keeping the links.
# --cleanup tracks only the files created in Given steps, so the
# files created by When steps remain: they are listed below.
# The list may become incomplete when new tests create new files:
# any file not listed here, for instance a file of your own, is preserved.
clean:
	@ - rm -f *.out *.err main cp hello optimize *.created_files tmp.txt output2.txt tmp.md index_*.md expected_grammar expected_keywords input.*
	@ - rm -f all_results.xml result2.xml time_attribute.xml result.xml windows_results.xml file1.tmp
	@ - rm -f time_attribute.md escape_test.md simple_test.md file? log.?
	@ - rm -rf dir* suite_?.md whatever.txt background.md robustness.md filtered_step.md verbose_output_*.md
	@ - rm -f file_utilities_tests/test_file_utilities.md
	@ - rm -f blaise_cendrars.txt flowers1.txt flowers2.txt flowers3.txt help_message.txt le_Cid_1.txt le_Cid_2.txt le_Cid_3.txt list_1.txt list_2.txt
	@ - rm -f text.1 text.2 text.3 text.4 text.ref tmp.1 tmp.2 tmp.3 tmp.4
	@ - rm -f cmd_not_found.md compare.md compare_crlf.md contains.md contains_multiline.md create_exec.md failed_doesnt_1.md failed_doesnt_2.md
	@ - rm -f is.md scenario1.md scenario_with_assertion_error.md scenario_with_syntax_error.md skipped_count_test.md step_markers.md t1.md test_that_should_fail.md
	@ - rm -f file_is_code_fence.input no_step_in_scenario.input no_step_in_scenario_b.input no_step_or_scenario_in_feature.input vza.input
	@ - rm -f ask ask.c new_name.txt

# distclean also removes the binaries and the links
distclean: clean
	@ - rm -f bbt bbt.exe sut sut.exe gcc rpl rpl.exe
	@ $(MAKE) clean --directory=file_utilities_tests

