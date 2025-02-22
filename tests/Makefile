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
# Manage differences between Windows and Unix
ifeq ($(PLATFORM), Windows)
	@ln -fs ../sut sut.exe
	@ln -fs ../bbt bbt.exe

else ifeq ($(PLATFORM), Darwin)
	export GNAT_FILE_NAME_CASE_SENSITIVE=1
	@ln -fs ../sut sut
	@ln -fs ../bbt bbt

else ifeq ($(PLATFORM), Linux)
	@ln -fs ../sut sut
	@ln -fs ../bbt bbt
	# Seems that the PATH is not passed to spawned process on Linux :
	@ln -fs /usr/bin/gcc gcc
	@ln -fs /usr/bin/rpl rpl

else
	@echo "Unknown platform $(PLATFORM)"
	@exit 1

endif

check: setup ./bbt ./sut
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
	
	@ # Check examples
	@ ./bbt --yes -k -c -hm -o $(RESULTS_DIR)/examples_results.md ../docs/examples \
	  && echo 1. examples tests OK ========== || echo 1. examples tests KO, check the availability of gcc and rpl on your platform "******"
	@ echo

	@ # Then test all the features
	@ ./bbt --yes --keep_going --cleanup --human_match --generate_badge $(RESULTS_DIR)/badge.url --output $(RESULTS_DIR)/features_results.md ../docs/features && echo 2. features tests OK ========== || echo 2. features tests KO "******"
	@ echo

	@ # Then run non reg tests
	@ ./bbt --yes --keep_going --cleanup --human_match --output $(RESULTS_DIR)/non_reg_tests.md ../docs/non_reg_tests && echo 3. non reg tests OK ========== || echo 3. features reg KO "******"
	@ echo

	@ # Unit testing
	@ $(MAKE) check --directory=file_utilities_tests && echo 4. file_utilities_tests OK ========== || echo 4. file_utilities_tests KO "******"
	@ mv file_utilities_tests/test_file_utilities.md $(RESULTS_DIR)/test_file_utilities.md
	@ echo

doc: $(RESULTS_DIR)
	@ rpl --quiet bbt $(PLATFORM) $(RESULTS_DIR)/badge.url
	echo wget badge ...
	@ wget -q "`cat $(RESULTS_DIR)/badge.url`" -O $(RESULTS_DIR)/badge.svg

clean:
	@ - rm -f *.out main bbt bbt.exe sut sut.exe gcc rpl *.created_files tmp.txt
	@ - rm -rf dir*
	@ $(MAKE) clean --directory=file_utilities_tests
	
