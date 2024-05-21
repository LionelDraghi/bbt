all: clean check

check: ../../bbt ../../uut
	# We run one by one scenarios with tests that should fail
	@ - ln -sf ../../uut uut
	@ - ln -sf ../../bbt bbt

	../../bbt no_step.md > output.1
	@ sdiff -sbB expected.1 output.1 || meld expected.1 output.1
	# this first test check also that starting bbt from a different dir is OK

	./bbt no_feature.md > output.2
	@ sdiff -sbB expected.2 output.2 || meld expected.2 output.2

	./bbt -v no_title.md > output.3
	@ sdiff -sbB expected.3 output.3 || meld expected.3 output.3

.PHONY : clean
clean:
	@ - rm -rf dir1 output.* *.out config.ini  
	
