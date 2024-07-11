all: clean check

check: ../bbt ../uut
	# Then we run all scenarii that should pass
	# This is also a way to test multiple test files 
	# on the command line, and in a different directory
	@ - ln -fs ../uut .

	@ - rm examples_results.md
	@ ../bbt -c -ad -o examples_results.md examples && echo 1. examples tests OK ========== || echo 1. examples tests KO "******"
	@ echo

	@ - rm features_results.md
	@ ../bbt -c -ad -o features_results.md features && echo 2. features tests OK ========== || echo 2. features tests KO "******"
	@ echo

	# We run one by one scenario with test that should fail
	@ - $(MAKE) check --directory=error_tests && echo 3. error tests OK ============= || echo 3. error tests KO "*********"
	@ echo


.PHONY : clean
clean:
	@ $(MAKE) -s clean --directory=error_tests
	@ - rm -f features/config.ini */*.out gmon.out main main.c rules.txt */*created_files examples/main

