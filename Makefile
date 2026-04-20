PLATFORM = $(shell uname -s)

ifeq ($(OS), Windows_NT)
	# PLATFORM := $(shell uname -s) -> should be Linux, Windows or Darwin
	# But is MSYS_NT-10.0-26100 on Windows, and I don't want the DIR name
	# to change with the MSYS or Windows version number.
	# This is why I override it on Windows:
	PLATFORM := Windows
	EXE_SUFFIX := .exe
else
	EXE_SUFFIX :=
endif

.SILENT:

all: build sut check doc

bbt$(EXE_SUFFIX): build
sut$(EXE_SUFFIX): tools

sut:
	echo === building sut
	cd tools && alr build --release

	$(MAKE) -s setup --directory=tests
	$(MAKE) -s build --directory=tests

build:
	echo
	echo === building bbt for dev #=# and instrument bbt
	alr --non-interactive build --validation
	# Alire profiles : --release --validation --development (default)

	#=# alr gnatcov instrument --level=stmt --dump-trigger=atexit --projects=bbt.gpr --ignore-source-files=bbt-main*.ad? 
	#=# # can't prevent gnatcov to instrument bbt-main.adb with non legal Ada
	#=# rm obj/development/bbt-gnatcov-instr/bbt-main*ad[sb]
	#=# alr build -- --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts_full.gpr

release:
	echo
	echo === building bbt for release
	alr --non-interactive build --release
	# Alire profiles : --release --validation --development (default)

check: bbt$(EXE_SUFFIX) sut$(EXE_SUFFIX)
	# --------------------------------------------------------------------
	echo === running tests
	$(MAKE) -s check --directory=tests

	#=# echo
	#=# echo === coverage report
	#=# alr gnatcov coverage --annotate=html --output-dir gnatcov_out --level=stmt --projects bbt.gpr *.srctrace

doc: ./bbt
	echo === doc prod
	@ $(MAKE) -s doc --directory=tests

	./bbt list_grammar  > docs/grammar.md
	./bbt list_keywords > docs/keywords.md

	./bbt help tutorial  > docs/tutorial.md
	./bbt help example   > docs/example.md 
	./bbt help on_all    > docs/bbt_help.txt 

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
	@ - rm -rf config.ini *.out dir? docs/tests/*/*.out obj/* tmp.txt output2.txt main main.c
	echo OK
	echo
