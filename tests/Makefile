all: clean check

check: ../bbt ../uut
	# Then we run all scenarii that should pass
	# This is also a way to test multiple test files 
	# on the command line, and in a different directory
	@ - ln -fs ../uut .
	@ - ln -fs ../bbt .

	@ ./bbt --yes -c -o ../docs/tests/examples_results.md -ed ./ ../docs/examples && echo 1. examples tests OK ========== || echo 1. examples tests KO "******"
	@ echo

	@ ./bbt --yes --cleanup --output ../docs/tests/features_results.md --exec_dir ./ ../docs/features && echo 2. features tests OK ========== || echo 2. features tests KO "******"
	@ echo

doc:
	
.PHONY : clean
clean:
	@ - rm -f *.out main bbt uut
	@ - rm -rf dir*

