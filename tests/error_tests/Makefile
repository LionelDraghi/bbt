all: clean check

check: ../../bbt ../../uut
	# We run one by one scenario with test that should fail
	@ - ln -sf ../../uut uut
	@ - ln -sf ../../bbt bbt

	../../bbt file_is_string.md > output.1
	@ sdiff -sbB expected.1 output.1 || meld expected.1 output.1
	# this first test check also that starting bbt from a different dir is OK

	@ rm config.ini
	./bbt file_test_and_creation.md > output.2
	@ sdiff -sbB expected.2 output.2 || meld expected.2 output.2

	./bbt successfully.md > output.3
	@ sdiff -sbB expected.3 output.3 || meld expected.3 output.3

	./bbt contains_multiline.md > output.4
	@ sdiff -sbB expected.4 output.4 || meld expected.4 output.4

	@ rm config.ini
	./bbt background.md > output.5
	@ sdiff -sbB expected.5 output.5 || meld expected.5 output.5

	./bbt run_or.md > output.6
	@ sdiff -sbB expected.6 output.6 || meld expected.6 output.6

	./bbt is_file.md > output.7
	@ sdiff -sbB expected.7 output.7 || meld expected.7 output.7

.PHONY : clean
clean:
	@ - rm -rf dir1 output.* *.out config.ini pass_tests/*.out
	
