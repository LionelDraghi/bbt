.PHONY : clean setup

# Test results are produced locally, and moved to the doc tree when "make doc" is called, 
# in a directory matching the platform.
RESULTS_DIR := ../docs/tests_results/$(OS)

setup:
	mkdir -p $(RESULTS_DIR)
# Manage differences between Windows and Unix
ifeq ($(OS), Windows_NT)
	@ln -fs ../sut sut.exe
	@ln -fs ../bbt bbt.exe
else
	@ln -fs ../sut sut
	@ln -fs ../bbt bbt
	@ln -fs /usr/bin/gcc gcc
	@ln -fs /usr/bin/rpl rpl
endif

all: check

build:
	@ $(MAKE) -s build --directory=file_utilities_tests

check: setup ./bbt ./sut
	# mkdir -p $(RESULTS_DIR)
	echo Results in $(RESULTS_DIR)
    
	@ # Some examples rely on external exe : 
	@ # - rpl : on windows, you'll find one in the GnuWin32 package
	@ #         It's not the same rpl than on my Debian, but fortunately command lines are similar  
	@ # - gcc

	@ # Check examples
	@ ./bbt --yes -k -c -o examples_results.md ../docs/examples && echo 1. examples tests OK ========== || echo 1. examples tests KO, check the availability of gcc and rpl on your platform "******"
	@ echo

	@ # Then test all the features
	@ ./bbt --yes --keep_going --cleanup --output features_results.md ../docs/features && echo 2. features tests OK ========== || echo 2. features tests KO "******"
	@ echo

	@ # Then run non reg tests
	@ ./bbt --yes --keep_going --cleanup --output non_reg_tests.md ../docs/non_reg_tests && echo 3. non reg tests OK ========== || echo 3. features reg KO "******"
	@ echo

	@ # Unit testing
	@ $(MAKE) check --directory=file_utilities_tests && echo 4. file_utilities_tests OK ========== || echo 4. file_utilities_tests KO "******"
	@ mv file_utilities_tests/test_file_utilities.md .
	@ echo

doc: $(RESULTS_DIR)
	@ mv examples_results.md    $(RESULTS_DIR)
	@ mv features_results.md    $(RESULTS_DIR)
	@ mv non_reg_tests.md       $(RESULTS_DIR)
	@ mv test_file_utilities.md $(RESULTS_DIR)

clean:
	@ - rm -f *.out main bbt sut gcc rpl tmp.txt
	@ - rm -rf dir*
	@ $(MAKE) clean --directory=file_utilities_tests
	
