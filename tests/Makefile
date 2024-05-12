all: clean check

check: ../bbt ../uut
	# We run one by one scenario with test that should fail
	@ $(MAKE) check --directory=error_tests
	@ echo Error Tests OK ==================
	@ echo

	# Then we run all scenarii that should pass
	# This is also a way to test multiple test files 
	# on the command line, and in a different directory
	@ - ln -fs ../uut .
	../bbt -q -o pass_tests.md pass_tests/*.md 
	@ echo Pass Tests OK ===================
	@ echo


.PHONY : clean
clean:
	@ $(MAKE) -s clean --directory=error_tests
	@ - rm -f pass_tests/config.ini pass_tests/*.out

