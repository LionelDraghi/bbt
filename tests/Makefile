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
else
	EXE_SUFFIX :=
endif

RESULTS_DIR := ../docs/tests_results/$(PLATFORM)
BADGE_URL = $(shell cat $(RESULTS_DIR)/badge.url)
# BADGE_URL_LINUX = $(shell cat ../docs/tests_results/Linux/badge.url)
# BADGE_URL_DARWIN = $(shell cat ../docs/tests_results/Darwin/badge.url)
# BADGE_URL_WINDOWS = $(shell cat ../docs/tests_results/Windows/badge.url)

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
    
	@ln -fs ../sut$(EXE_SUFFIX) sut$(EXE_SUFFIX)
	@ln -fs ../bbt$(EXE_SUFFIX) bbt$(EXE_SUFFIX)
	@ln -fs ../rpl$(EXE_SUFFIX) rpl$(EXE_SUFFIX)
# Manage differences between Windows and Unix
ifeq ($(PLATFORM), Windows)
EXCLUDE := --exclude Unix_Only

else ifeq ($(PLATFORM), Darwin)
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

features:
	@ # Then test all the features
	@ echo 1. features tests ================
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

examples:
	@ # Check examples
	@ echo 2. examples check ================
	./bbt --yes -k -c -q -hm \
	--index $(RESULTS_DIR)/examples_results.md \
	../docs/examples ../docs/examples/gcc_hello_world.md || echo KO, check the availability of gcc on your platform "********"
	@ echo

non_reg:
	@ # Then run non reg tests
	@ echo 3. non regression tests ==========
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
	
	@ - rm -f *.out *.err main cp tmp.txt output2.txt tmp.md index_*.md expected_grammar expected_keywords input.*

doc: $(RESULTS_DIR)
	@ $(MAKE) -s doc DOC_DIR=../$(RESULTS_DIR) EXE_SUFFIX=$(EXE_SUFFIX) --directory=file_utilities_tests
	
	@ rpl --quiet bbt $(PLATFORM) $(RESULTS_DIR)/badge.url
	@ echo wget badge ...
	@ wget -q "$(BADGE_URL)" -O $(RESULTS_DIR)/badge.svg


# clean removes the test run artifacts only, keeping the links
clean:
	@ - rm -f *.out *.err main cp hello optimize *.created_files tmp.txt output2.txt tmp.md index_*.md expected_grammar expected_keywords input.*
	@ - rm -f all_results.xml result2.xml time_attribute.xml result.xml windows_results.xml file1.tmp
	@ - rm -f time_attribute.md escape_test.md simple_test.md file? log.?
	@ - rm -rf dir* suite_?.md whatever.txt background.md robustness.md filtered_step.md verbose_output_*.md

# distclean also removes the binaries and the links
distclean:
	@ - rm -f *.out *.err main cp bbt bbt.exe hello optimize sut sut.exe gcc rpl *.created_files tmp.txt output2.txt tmp.md index_*.md expected_grammar expected_keywords input.*
	@ - rm -f all_results.xml result2.xml time_attribute.xml result.xml windows_results.xml file1.tmp
	@ - rm -f time_attribute.md escape_test.md simple_test.md file? log.?
	@ - rm -rf dir* suite_?.md whatever.txt background.md robustness.md filtered_step.md verbose_output_*.md
	@ $(MAKE) clean --directory=file_utilities_tests

