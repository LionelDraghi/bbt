all: clean check
check: ../bbt ../sut

	# Then we run all scenarii that should pass
	# This is also a way to test multiple test files 
	# on the command line, and in a different directory
ifeq ($(OS), Windows_NT)
	echo $(OS)

	@ - ln -fs ../sut sut.exe
	@ - ln -fs ../bbt bbt.exe 
else
	@ - ln -fs ../sut
	@ - ln -fs ../bbt
	@ - ln -fs /usr/bin/gcc 
	@ - ln -fs /usr/bin/rpl
endif

	@ ./bbt --yes -k -c -o ../docs/tests/examples_results.md -ed ./ ../docs/examples && echo 1. examples tests OK ========== || echo 1. examples tests KO "******"
	@ echo

	@ ./bbt --yes --keep_going --cleanup --output ../docs/tests/features_results.md --exec_dir ./ ../docs/features && echo 2. features tests OK ========== || echo 2. features tests KO "******"
	@ echo

doc:
	
.PHONY : clean
clean:
	@ - rm -f *.out main bbt sut gcc rpl
	@ - rm -rf dir*

