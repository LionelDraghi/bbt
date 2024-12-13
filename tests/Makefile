all: clean check
check: ../bbt ../sut
	# Some examples rely on external exe : 
	# - rpl : on windows, you'll find one in the GnuWin32 package
	#         On my Debian, it' not the same rpl, but fortunately command lines are similar  
	# - gcc

ifeq ($(OS), Windows_NT)
	echo $(OS)
	@ - ln -fs ../sut sut.exe
	@ - ln -fs ../bbt bbt.exe 
else
	echo $(OS)
	@ - ln -fs ../sut
	@ - ln -fs ../bbt
	@ - ln -fs /usr/bin/gcc 
	@ - ln -fs /usr/bin/rpl
endif

	@ ./bbt --yes -k -c -o ../docs/tests/examples_results.md -ed ./ ../docs/examples && echo 1. examples tests OK ========== || echo 1. examples tests KO, check the availability of gcc and rpl on your platform "******"
	@ echo

	# Then test all the features
	@ ./bbt --yes --keep_going --cleanup --output ../docs/tests/features_results.md --exec_dir ./ ../docs/features && echo 2. features tests OK ========== || echo 2. features tests KO "******"
	@ echo

	# Unit testing
	cd file_utilities_tests && alr build && alr run -a -q

doc:
	
.PHONY : clean
clean:
	@ - rm -f *.out main bbt sut gcc rpl
	@ - rm -rf dir*
	@ $(MAKE) clean --directory=file_utilities_tests

