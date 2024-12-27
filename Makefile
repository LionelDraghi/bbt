.SILENT:
all: build check doc

build:
	echo
	echo === build #=# and instrument bbt
	alr build --development
	# Alire profiles : --release --validation --development (default)
	
	#=# alr gnatcov instrument --level=stmt --dump-trigger=atexit --projects=bbt.gpr --ignore-source-files=bbt-main*.ad? 
	#=# # can't prevent gnatcov to instrument bbt-main.adb with non legal Ada
	#=# rm obj/development/bbt-gnatcov-instr/bbt-main*ad[sb]
	#=# alr build -- --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts_full.gpr

	echo === build tools
	cd tools && alr build --release
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
	@ $(MAKE) doc --directory=tests
	
	./bbt -lg > docs/grammar.md
	./bbt -lk > docs/keywords.md

	./bbt -ct
	mv bbt_template.md docs/

	> docs/bbt_help.md
	echo "# Command line help" >> docs/bbt_help.md
	echo '```'            >> docs/bbt_help.md
	./bbt --help          >> docs/bbt_help.md
	echo '```'            >> docs/bbt_help.md
	
	echo 'Fixme in current version'	>  /tmp/fixme_index.md
	echo '------------------------'	>> /tmp/fixme_index.md
	echo                            >> /tmp/fixme_index.md
	echo 'Location | Text'          >> /tmp/fixme_index.md
	echo '---------|-----'          >> /tmp/fixme_index.md
	grep -rn "Fixme:" src/*  docs/* | sed "s/:/|/2;s/Fixme://" >> /tmp/fixme_index.md
	mv /tmp/fixme_index.md docs/fixme_index.md

	echo 'Issue references in current version'	>  /tmp/issues_index.md
	echo '-----------------------------------'	>> /tmp/issues_index.md
	echo                                		>> /tmp/issues_index.md
	echo 'Location | Text'             		    >> /tmp/issues_index.md
	echo '---------|-----'             		    >> /tmp/issues_index.md
	grep -rn "Issue #" src/* docs/* | sed "s/:/|/2;s/Issue #/#/" >> /tmp/issues_index.md
	mv /tmp/issues_index.md docs/issues_index.md

	echo Checking links in md files
ifeq ($(OS), Windows_NT)
	- mlc --ignore-path docs/tests_results/Linux | grep '\[Err' && echo OK 
else
	- mlc --ignore-path docs/tests_results/Windows | grep '\[Err' && echo OK 
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
	@ $(MAKE) clean --directory=tests
	@ - rm -rf config.ini *.out dir1 docs/tests/*/*.out obj/*
	echo OK
	echo
