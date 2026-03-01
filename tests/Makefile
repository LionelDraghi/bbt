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
	EXEC_SUFFIX := .exe
else
	EXEC_SUFFIX :=
endif

RESULTS_DIR := ../docs/tests_results/$(PLATFORM)

all: check

.PHONY : clean setup check

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

setup:
	@ln -fs ../sut$(EXE_SUFFIX) sut$(EXE_SUFFIX)
	@ln -fs ../bbt$(EXE_SUFFIX) bbt$(EXE_SUFFIX)
# Manage differences between Windows and Unix
ifeq ($(PLATFORM), Windows)
EXCLUDE := --exclude Unix_Only

else ifeq ($(PLATFORM), Darwin)
	export GNAT_FILE_NAME_CASE_SENSITIVE=1
EXCLUDE := --exclude Windows_Only

else ifeq ($(PLATFORM), Linux)
	# Seems that the PATH is not passed to spawned process on Linux :
	@ln -fs /usr/bin/gcc gcc
	@ln -fs /usr/bin/rpl rpl
EXCLUDE = --exclude Windows_Only

else
	@echo "Unknown platform $(PLATFORM)"
	@exit 1

endif

check: setup bbt$(EXE_SUFFIX) sut$(EXE_SUFFIX)
	mkdir -p $(RESULTS_DIR)
    
	@ # Some examples rely on external exe : 
	@ # - rpl : on windows, you'll find one in the GnuWin32 package
	@ #         It's not the same rpl than on my Debian, but fortunately command lines are similar  
	@ # - gcc

	@ # =====================================================================
	@ # WARNING : remember that bbt output file must be output directly in
	@ # the right directory, and NOT in the current directory and then moved to 
	@ # docs/test_results, otherwise relative path in the results file to the 
	@ # scenario files will be wrong!
	@ # =====================================================================
	
	@ # Then test all the features
	@ echo 1. features tests ================
	@ ./bbt --yes --keep_going \
	--cleanup $(EXCLUDE) \
	-q -sb --human_match \
	--generate_badge $(RESULTS_DIR)/badge.url \
	--junit features.xml \
	--index $(RESULTS_DIR)/features_results.md \
	../docs/features 
	@ echo

	@ # Check examples
	@ echo 2. examples check ================
	@ ./bbt --yes -k -c -q -sb -hm \
	--index $(RESULTS_DIR)/examples_results.md \
	../docs/examples ../src/help/example.md || echo KO, check the availability of gcc and rpl on your platform "********"
	@ echo

	@ # Then run non reg tests
	@ echo 3. non regression tests ==========
	@ ./bbt --yes --keep_going --cleanup -q -sb \
	--human_match \
	--index $(RESULTS_DIR)/non_reg_tests.md \
	../tests/non_reg_tests 
	@ echo

	@ # Unit testing
	@ echo 4. unit testing ==================
	@ $(MAKE) -s check --directory=file_utilities_tests && echo OK || echo ***** KO
	@ echo

	@ - rm -f *.out main tmp.txt output2.txt tmp.md index_*.md

doc: $(RESULTS_DIR)
	@ $(MAKE) -s doc DOC_DIR=../$(RESULTS_DIR) --directory=file_utilities_tests
	
	@ rpl --quiet bbt $(PLATFORM) $(RESULTS_DIR)/badge.url
	echo wget badge ...
	@ wget -q "`cat $(RESULTS_DIR)/badge.url`" -O $(RESULTS_DIR)/badge.svg

clean:
	@ - rm -f *.out main bbt bbt.exe hello optimize sut sut.exe gcc rpl *.created_files tmp.txt output2.txt tmp.md index_*.md
	@ - rm -rf dir*
	@ $(MAKE) clean --directory=file_utilities_tests
	
