all: clean check

check: ../bbt ../uut
	# Then we run all scenarii that should pass
	# This is also a way to test multiple test files 
	# on the command line, and in a different directory
	@ - ln -fs ../uut .
	@ rm pass_tests/pass_tests.md
	../bbt -q -y -o pass_tests/pass_tests.md pass_tests/*.md && echo 1. features tests OK ========== || echo 1. features tests KO "******"
	@ echo

	# We run one by one scenario with test that should fail
	@ - $(MAKE) check --directory=error_tests                && echo 2. error tests OK ============= || echo 2. error tests KO "*********"
	@ echo

	@ - $(MAKE) check --directory=robustness                 && echo 3. robustness tests OK ======== || echo 3. robustness tests KO "****"
	@ echo


.PHONY : clean
clean:
	@ $(MAKE) -s clean --directory=error_tests
	@ $(MAKE) -s clean --directory=robustness
	@ - rm -f pass_tests/config.ini pass_tests/*.out gmon.out

