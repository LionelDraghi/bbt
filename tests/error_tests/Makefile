all: clean check

BBT=../../bbt

check: ${BBT} ../../uut
	# We run one by one scenarios with tests that should fail
	@ - ln -sf ../../uut uut
	@ - ln -sf ../../bbt bbt

	${BBT} file_is_string.md > output.1
	@ sdiff -sbB expected.1 output.1 || meld expected.1 output.1
	# this first test check also that starting bbt from a different dir is OK

	${BBT} successfully.md > output.3
	@ sdiff -sbB expected.3 output.3 || meld expected.3 output.3

	${BBT} contains_multiline.md > output.4
	@ sdiff -sbB expected.4 output.4 || meld expected.4 output.4

	@ rm config.ini
	${BBT} --auto_delete background.md > output.5
	@ sdiff -sbB expected.5 output.5 || meld expected.5 output.5

	# ${BBT} run_or.md > output.6
	# @ sdiff -sbB expected.6 output.6 || meld expected.6 output.6

	${BBT} is_file.md > output.7
	@ sdiff -sbB expected.7 output.7 || meld expected.7 output.7

	@rm -rf dir1
	# ${BBT} empty_dir.md > output.8
	# @ sdiff -sbB expected.8 output.8 || meld expected.8 output.8

.PHONY : clean
clean:
	@ - rm -rf dir1 output.* *.out config.ini pass_tests/*.out
	
