all: clean check
check: ../bbt ../sut
	@ # Some examples rely on external exe : 
	@ # - rpl : on windows, you'll find one in the GnuWin32 package
	@ #         On my Debian, it' not the same rpl, but fortunately command lines are similar  
	@ # - gcc

ifeq ($(OS), Windows_NT)
	echo $(OS)
	@ - ln -fs ../sut sut.exe
	@ - ln -fs ../bbt bbt.exe
	@ # RESULTS=../docs/tests_results/windows
else
	echo $(OS)
	@ - ln -fs ../sut
	@ - ln -fs ../bbt
	@ - ln -fs /usr/bin/gcc 
	@ - ln -fs /usr/bin/rpl
	@ # RESULTS=../docs/tests_results
endif


	# Check examples
	@ ./bbt --yes -k -c -o ../docs/tests_results/examples_results.md ../docs/examples && echo 1. examples tests OK ========== || echo 1. examples tests KO, check the availability of gcc and rpl on your platform "******"
	@ echo

	# Then test all the features
	@ ./bbt --yes --keep_going --cleanup --output ../docs/tests_results/features_results.md ../docs/features && echo 2. features tests OK ========== || echo 2. features tests KO "******"
	@ echo

	# Then run non reg tests
	@ ./bbt --yes --keep_going --cleanup --output ../docs/tests_results/non_reg_tests.md ../docs/non_reg_tests && echo 2. features tests OK ========== || echo 2. features tests KO "******"
	@ echo

	# Unit testing
	cd file_utilities_tests && alr build && alr run -a -q

doc:
	
.PHONY : clean
clean:
	@ - rm -f *.out main bbt sut gcc rpl tmp.txt
	@ - rm -rf dir*
	@ $(MAKE) clean --directory=file_utilities_tests

