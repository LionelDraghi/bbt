PLATFORM = $(shell uname -s)
# Not used yet:
# ARCH := $(shell uname -m)
# PROC := $(shell uname -p)

ifeq ($(OS), Windows_NT)
	# PLATFORM := $(shell uname -s) -> should be Linux, Windows or Darwin
	# But is MSYS_NT-10.0-26100 on Windows, and I don't want the DIR name
	# to change with the MSYS or Windows version number.
	# This is why I override it on Windows:
	PLATFORM := Windows
	EXE_SUFFIX := .exe
	# On Windows, ln -fs silently falls back on a timestamp-preserving
	# copy: an explicit cp is more honest, and sets a fresh mtime at
	# each refresh, so that the staleness is observable
	REFRESH := cp -f
else
	EXE_SUFFIX :=
	REFRESH := ln -fs
endif

RESULTS_DIR := ../docs/tests_results/$(PLATFORM)

# Options common to the three bbt suite invocations below
COMMON_OPTIONS := --yes --keep_going --cleanup=full --quiet \
--human_match --scenario_timeout 30

all: check

.PHONY : help clean distclean setup check features examples non_reg unit_testing doc

help:
	@ echo "Usage: make [target]"
	@ echo ""
	@ echo "Targets:"
	@ echo "  all           : check (default when no target given)"
	@ echo "  build         : build the file_utilities unit test program"
	@ echo "  setup         : create $(RESULTS_DIR) and refresh the sut, bbt"
	@ echo "                   and rpl links in tests/"
	@ echo "  refresh_bbt   : refresh the bbt link only, so that a direct"
	@ echo "                   ./bbt run in tests/ never uses a stale binary"
	@ echo "  features      : run the functional tests (docs/features)"
	@ echo "  examples      : run the sanity checks (docs/examples),"
	@ echo "                   which require gcc on the PATH"
	@ echo "  non_reg       : run the non regression tests (non_reg_tests)"
	@ echo "  unit_testing  : run the file_utilities unit tests"
	@ echo "  check         : run all the suites, which remove their own"
	@ echo "                   run artifacts (--cleanup=full)"
	@ echo "  doc           : copy the unit test results to $(RESULTS_DIR)"
	@ echo "                   and fetch the badge"
	@ echo "  clean         : fallback removing the artifacts of direct"
	@ echo "                   runs made without --cleanup=full, keeping"
	@ echo "                   the built binaries and links usable"
	@ echo "  distclean     : remove everything that can be rebuilt,"
	@ echo "                   including binaries and links"
	@ echo ""
	@ echo "To run a single test: make <test id> (e.g. make A300) from the"
	@ echo "root directory: it runs the scenario with the right options, and"
	@ echo "prints the exact command line, ready to be grabbed and tweaked"
	@ echo "(e.g. replace ./bbt with ./bbt explain, or add -d for traces)."

# --------------------------------------------------------------------------
# Replay a single functional test by id, e.g. make A300.
# The id is resolved against ../docs/features (A300 -> A300_*.md), then
# run with the same options as the features suite. The exact command
# line is displayed, so that it can be grabbed and tweaked (e.g. with
# explain or -d).
A%:
	@ f=`ls ../docs/features/$@_*.md 2>/dev/null | head -1`; \
	if [ -z "$$f" ]; then \
	    echo "no feature test file with id $@ in ../docs/features"; \
	    exit 1; \
	fi; \
	echo "=== running: (cd tests && ./bbt $(COMMON_OPTIONS) $(EXCLUDE) $(NEEDS_SHELL_EXCLUDE) $$f)"; \
	./bbt $(COMMON_OPTIONS) $(EXCLUDE) $(NEEDS_SHELL_EXCLUDE) $$f



# To keep the trace of the run on each platform, the test results
# are stored in a directory per platform (Linux, Windows, Darwin).
# Note that we can't just append the platform to the file name and put all
# the results in the same directory, because we need to separate what is 
# submitted to mlc (the tools to check the links in md files), otherwise it 
# will raise a lot of error on Windows files when run on Linux box, and vice
# versa. (and mlc command line is not flexible enough to run  
# "--ignore *_Windows.md")

build:
	@ $(MAKE) -s build --directory=file_utilities_tests

setup: ../sut$(EXE_SUFFIX) ../bbt$(EXE_SUFFIX)
	mkdir -p $(RESULTS_DIR)
    
	@$(REFRESH) ../sut$(EXE_SUFFIX) sut$(EXE_SUFFIX)
	@$(REFRESH) ../bbt$(EXE_SUFFIX) bbt$(EXE_SUFFIX)
	@$(REFRESH) ../rpl$(EXE_SUFFIX) rpl$(EXE_SUFFIX)

refresh_bbt: ../bbt$(EXE_SUFFIX)
	@$(REFRESH) ../bbt$(EXE_SUFFIX) bbt$(EXE_SUFFIX)
	# Called by the root build, so that a direct ./bbt run in tests/
	# after a rebuild never uses a stale binary
# Manage differences between Windows and Unix
ifeq ($(PLATFORM), Windows)
EXCLUDE := --exclude Unix_Only

else ifeq ($(PLATFORM), Darwin)
# Unindented on purpose: this is a make directive, exported to every recipe
# and its children (bbt and the spawned commands), not a recipe line.
# GNAT on a case-insensitive filesystem (default on macOS) otherwise
# lowercases file names at run time, breaking the self-tests.
# cf. https://forum.ada-lang.io/t/name-file-casing-error-on-darwin/1795
export GNAT_FILE_NAME_CASE_SENSITIVE=1
EXCLUDE := --exclude Windows_Only

else ifeq ($(PLATFORM), Linux)
# Seems that the PATH is not passed to spawned process on Linux :
	@ln -fs /usr/bin/gcc gcc
EXCLUDE = --exclude Windows_Only

else
	@echo "Unknown platform $(PLATFORM)"
	@exit 1

endif

# The run shell scenarios needing a shell that is not preinstalled on
# every platform are tagged needs_<shell>: they are excluded here when
# that shell is not in the PATH (cf. A310_Run_Shell.md).
NEEDS_SHELL_EXCLUDE := $(foreach s,dash zsh fish pwsh,\
  $(if $(shell command -v $(s) 2>/dev/null),,--exclude needs_$(s)))

features: setup
	@ # Then test all the features
	@ echo 1. features tests ================
	@ ./bbt --version
	./bbt $(COMMON_OPTIONS) \
	--exclude @Flaky \
	$(EXCLUDE) \
	$(NEEDS_SHELL_EXCLUDE) \
	--generate_badge $(RESULTS_DIR)/badge.url   \
	--junit $(RESULTS_DIR)/features_results.xml \
	--index $(RESULTS_DIR)/features_results.md  \
	../docs/features 
	@ echo

examples: setup
	@ # Check examples
	@ echo 2. examples check ================
	@ ./bbt --version
	./bbt $(COMMON_OPTIONS) \
	--index $(RESULTS_DIR)/examples_results.md \
	../docs/examples ../docs/examples/gcc_hello_world.md || echo KO, check the availability of gcc on your platform "********"
	@ echo

non_reg: setup
	@ # Then run non reg tests
	@ echo 3. non regression tests ==========
	@ ./bbt --version
	./bbt $(COMMON_OPTIONS) \
	--index $(RESULTS_DIR)/non_reg_tests.md \
	../tests/non_reg_tests 
	@ echo

unit_testing:
	@ # Unit testing
	@ echo 4. unit testing ==================
	$(MAKE) -s check EXE_SUFFIX=$(EXE_SUFFIX) --directory=file_utilities_tests && echo OK || echo ***** KO
	@ echo

check: setup bbt$(EXE_SUFFIX) sut$(EXE_SUFFIX) features examples non_reg unit_testing
	@ # Some examples rely on external exe : 
	@ # - gcc

	@ # =====================================================================
	@ # WARNING : remember that bbt output file must be output directly in
	@ # the right directory, and NOT in the current directory and then moved to 
	@ # docs/test_results, otherwise relative path in the results file to the 
	@ # scenario files will be wrong!
	@ # =====================================================================
	
	@ $(MAKE) -s clean --directory=file_utilities_tests

doc: $(RESULTS_DIR)
	@ $(MAKE) -s doc DOC_DIR=../$(RESULTS_DIR) EXE_SUFFIX=$(EXE_SUFFIX) --directory=file_utilities_tests
	
	@ ../rpl --quiet bbt $(PLATFORM) $(RESULTS_DIR)/badge.url
	@ echo fetching badge ...
	@ curl -s "$$(cat $(RESULTS_DIR)/badge.url)" -o $(RESULTS_DIR)/badge.svg


# clean is only a fallback: the suites run with --cleanup=full (cf.
# COMMON_OPTIONS) and bbt removes at the end of each document every
# file and dir created during the run, whatever created it. A direct
# run of a single test leaves nothing behind either when invoked as
# "./bbt --cleanup=full <test_file>". clean then only removes the
# default output files of plain direct runs, and the unit test
# results. Any other file, for instance a file of your own, is
# preserved.
clean:
	@ - rm -f *.out *.err file_utilities_tests/test_file_utilities.md

# distclean also removes the binaries and the links
distclean: clean
	@ - rm -f bbt bbt.exe sut sut.exe gcc rpl rpl.exe
	@ $(MAKE) clean --directory=file_utilities_tests

