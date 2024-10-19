all: clean check

check: ../bbt ../uut
	# Then we run all scenarii that should pass
	# This is also a way to test multiple test files 
	# on the command line, and in a different directory
	@ - ln -fs ../uut .

	@ - rm ../docs/tests/examples_results.md
	@ ../bbt --yes -c -o ../docs/tests/examples_results.md ../docs/tests/examples && echo 1. examples tests OK ========== || echo 1. examples tests KO "******"
	@ echo

	@ - rm ../docs/tests/features_results.md
	@ ../bbt --yes --cleanup --output ../docs/tests/features_results.md ../docs/tests/features && echo 2. features tests OK ========== || echo 2. features tests KO "******"
	@ echo

doc:
	
.PHONY : clean
clean:
	@ - rm -f *.out main
	@ - rm -rf dir*

