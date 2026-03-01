PLATFORM = $(shell uname -s)

.SILENT:
all: build check doc

bbt: build

build:
	echo
	echo === build #=# and instrument bbt
	alr --non-interactive build --development
	# Alire profiles : --release --validation --development (default)
	
	#=# alr gnatcov instrument --level=stmt --dump-trigger=atexit --projects=bbt.gpr --ignore-source-files=bbt-main*.ad? 
	#=# # can't prevent gnatcov to instrument bbt-main.adb with non legal Ada
	#=# rm obj/development/bbt-gnatcov-instr/bbt-main*ad[sb]
	#=# alr build -- --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts_full.gpr

	echo === build tools
	cd tools && alr build --release
	@ $(MAKE) -s setup --directory=tests
	@ $(MAKE) -s build --directory=tests

check:
	# --------------------------------------------------------------------
	echo === run tests
	@ $(MAKE) -s check --directory=tests

	#=# echo
	#=# echo === coverage report
	#=# alr gnatcov coverage --annotate=html --output-dir gnatcov_out --level=stmt --projects bbt.gpr *.srctrace

doc: ./bbt
	echo === doc prod
	@ $(MAKE) -s doc --directory=tests
	
	./bbt list_grammar  > docs/grammar.md
	./bbt list_keywords > docs/keywords.md

	./bbt create_template
	mv bbt_template.md docs/

	> docs/bbt_help.md
	echo "# Command line help" >> docs/bbt_help.md
	echo '```'                 >> docs/bbt_help.md
	./bbt help                 >> docs/bbt_help.md
	./bbt help filtering       >> docs/bbt_help.md
	./bbt help matching        >> docs/bbt_help.md
	./bbt help other           >> docs/bbt_help.md
	echo '```'                 >> docs/bbt_help.md
	
	echo 'Fixme in current version'	>  fixme_index.md
	echo '------------------------'	>> fixme_index.md
	echo                            >> fixme_index.md
	echo 'Location | Text'          >> fixme_index.md
	echo '---------|-----'          >> fixme_index.md
	grep -rn 'Fixme:' docs/* src/* | sort | sed -E "s/:/|/2;s/Fixme://;s/(^[^:]*)/[\1](\.\.\/\1)/" >> fixme_index.md
	mv fixme_index.md docs/fixme_index.md

	echo 'Reference to issues in current version'	>  issues_index.md
	echo '--------------------------------------'	>> issues_index.md
	echo                                			>> issues_index.md
	echo 'Location | Text'             			    >> issues_index.md
	echo '---------|-----'             			    >> issues_index.md
	grep -rn 'Issue #' docs/* src/* | sort |  sed -E "s/:/|/2;s/Issue #/#/;s/(^[^:]*)/[\1](\.\.\/\1)/" >> issues_index.md
	mv issues_index.md docs/issues_index.md

	echo Checking links in md files
ifeq ($(OS), Windows_NT)
	- mlc --ignore-path docs/tests_results/Linux --ignore-path docs/tests_results/Darwin | grep '\[Err' && echo OK 
else ifeq ($(PLATFORM), Darwin)
	- mlc --ignore-path docs/tests_results/Windows --ignore-path docs/tests_results/Linux | grep '\[Err' && echo OK 
else ifeq ($(PLATFORM), Linux)
	- mlc --ignore-path docs/tests_results/Windows --ignore-path docs/tests_results/Darwin | grep '\[Err' && echo OK 
else
	echo "Unknown platform $(PLATFORM)"
	exit 1
endif
	# mlc is much faster than markdown-link-check and gives no false positive yet
	# > apt install cargo
	# > cargo install mlc
	# > fish_add_path ~/.cargo/bin/
	echo

install: ./bbt
	echo --- install:
	cp -p bbt ~/bin
	echo OK
	echo

.PHONY : clean
clean:
	echo --- clean:
	alr clean
	cd tools && alr clean
	@ $(MAKE) -s clean --directory=tests
	@ - rm -rf config.ini *.out dir? docs/tests/*/*.out obj/* tmp.txt output2.txt
	echo OK
	echo
