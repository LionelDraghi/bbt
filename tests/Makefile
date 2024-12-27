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

PLATFORM := $(shell uname -s)
# should be Linux, Windows or Darwin
### ARCH := $(shell uname -m)
### PROC := $(shell uname -)

RESULTS_DIR := ../docs/tests_results/$(PLATFORM)

setup:
# Manage differences between Windows and Unix
ifeq ($(PLATFORM), Windows)
	@ln -fs ../sut sut.exe
	@ln -fs ../bbt bbt.exe
else
	@ln -fs ../sut sut
	@ln -fs ../bbt bbt
	@ln -fs /usr/bin/gcc gcc
	@ln -fs /usr/bin/rpl rpl
endif

build:
	@ $(MAKE) -s build --directory=file_utilities_tests

check: setup ./bbt ./sut
	@ mkdir -p $(RESULTS_DIR)
    
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
	@ ./bbt --yes -k -c -o $(RESULTS_DIR)/examples_results.md ../docs/examples \
	  && echo 1. examples tests OK ========== || echo 1. examples tests KO, check the availability of gcc and rpl on your platform "******"
	@ echo

	@ # Then test all the features
	@ ./bbt --yes --keep_going --cleanup --output $(RESULTS_DIR)/features_results.md ../docs/features && echo 2. features tests OK ========== || echo 2. features tests KO "******"
	@ echo

	@ # Then run non reg tests
	@ ./bbt --yes --keep_going --cleanup --output $(RESULTS_DIR)/non_reg_tests.md ../docs/non_reg_tests && echo 3. non reg tests OK ========== || echo 3. features reg KO "******"
	@ echo

	@ # Unit testing
	@ $(MAKE) check --directory=file_utilities_tests && echo 4. file_utilities_tests OK ========== || echo 4. file_utilities_tests KO "******"
	@ mv file_utilities_tests/test_file_utilities.md $(RESULTS_DIR)/test_file_utilities.md
	@ echo

doc: $(RESULTS_DIR)

clean:
	@ - rm -f *.out main bbt sut gcc rpl tmp.txt
	@ - rm -rf dir*
	@ $(MAKE) clean --directory=file_utilities_tests
	
