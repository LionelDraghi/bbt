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

# Test results directories (cf. AGENTS.md, Test results, and the
# template project): the local runs of the suites write to
# docs/tests_results/<platform>/<profile>/, git-ignored; only CI runs
# are recorded, staged at the same paths by the CI workflows (with
# git add -f), so that a local run can be diffed, in the editor,
# against the last committed run of the same profile. Outputs are
# comparable; the run times are not (two machines never agree).
PROFILE ?= validation
RESULTS_ROOT := docs/tests_results/$(PLATFORM)
RESULTS_DIR = $(RESULTS_ROOT)/$(PROFILE)

.SILENT:

# The empty recipes below are required: without a recipe, make would
# pick the catch-all single-test rule at the bottom to "build" those
# targets, delegating them to tests/ and replaying the suites.
all: build check doc
	@:

bbt$(EXE_SUFFIX): build
	@:
sut$(EXE_SUFFIX): tools
	@:
rpl$(EXE_SUFFIX): tools
	@:

.PHONY : all help tools build build-release release check test-run features examples sanity backends doc install
help:
	echo "Usage: make [target]"
	echo ""
	echo "Targets:"
	echo "  all          : build, check and doc (default when no target given)"
	echo "  build        : build bbt in validation mode"
	echo "  build-release: build bbt in release mode (no test replay)"
	echo "  release      : full release chain: complete clean (distclean),"
	echo "                 release builds (bbt and tools), then a full"
	echo "                 test replay on the release executable, with the"
	echo "                 results under $(RESULTS_ROOT)/release;"
	echo "                 only a green replay unlocks the release"
	echo "  tools        : build the sut and rpl tools, and create the"
	echo "                 sut, bbt and gcc links in tests/"
	echo "  check [cli (default) | gtk4 | webserver | all ]"
	echo "               : run the tests (cli runs only the cli tests suites; gtk4 runs only the"
	echo "                 shared GUI suite of backends/scenarios)"
	echo "  features     : run the functional tests only (docs/cli_features)"
	echo "  examples     : run the sanity checks only (docs/examples)"
	echo "  sanity       : alias of examples"
	echo "  GUI=1        : modifier: run features/examples/check (and a single"
	echo "                 test) under the interactive bbt --gui debugger window"
	echo "  doc          : regenerate the generated docs (help reference files,"
	echo "                 indexes, badges) and check the links with mlc"
	echo "  install      : copy bbt to ~/bin"
	echo "  clean        : remove the test run artifacts and the backend"
	echo "                 build residues only, keeping binaries and"
	echo "                 links usable"
	echo "  distclean    : remove everything that can be rebuilt,"
	echo "                 including binaries and links"
	echo ""
	echo "Refer to AGENTS.md and docs/dev/developer_guide.md for more details."

# A full alr build is only needed when the dependencies or the profile
# changed: the stamp (obj/.alire_build_stamp) records the profile of
# the last full build, and a direct gprbuild on the generated config
# gives the same result otherwise, in seconds.
ALIRE_STAMP := obj/.alire_build_stamp

tools:
	echo === building tools
	cd tools && alr build --release

	$(MAKE) -s setup --directory=tests
	$(MAKE) -s build --directory=tests

build:
	echo
	echo === building bbt for dev
	mkdir -p obj
	# The gnoga sync runs twice (before and after the alr sync, cf.
	# tools/prepare_gnoga.sh): once before, for updates of already
	# cached crates, once after, for freshly deployed ones.
	@ ./tools/prepare_gnoga.sh
	if [ ! -f $(ALIRE_STAMP) ] \
	    || [ alire/alire.lock -nt $(ALIRE_STAMP) ] \
	    || [ src/Alire_config/bbt_config.gpr -nt $(ALIRE_STAMP) ] \
	    || [ "`cat $(ALIRE_STAMP) 2>/dev/null`" != "validation" ]; then \
	    echo === full alire sync and build; \
	    alr --non-interactive update \
	      || (echo === dirty index clone, resetting community; \
	          alr index --reset-community \
	          && alr --non-interactive update); \
	    ./tools/prepare_gnoga.sh \
	      && alr --non-interactive build --validation \
	      && echo validation > $(ALIRE_STAMP); \
	else \
	    echo === fast incremental build, dependencies unchanged; \
	    alr exec -- gprbuild -P bbt.gpr -p; \
	fi
	# Alire profiles : --release --validation --development (default)

	#=# alr gnatcov instrument --level=stmt --dump-trigger=atexit --projects=bbt.gpr --ignore-source-files=bbt-main*.ad? 
	#=# # can't prevent gnatcov to instrument bbt-main.adb with non legal Ada
	#=# rm obj/development/bbt-gnatcov-instr/bbt-main*ad[sb]
	#=# alr build -- --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts_full.gpr

	@ $(MAKE) -s refresh_bbt --directory=tests

# The standalone release build of bbt, without test replay. It runs
# after a distclean in the release chain below (the stamp is then
# gone: alr re-syncs and recompiles everything), or alone on an
# up-to-date build tree.
build-release:
	echo
	echo === building bbt for release
	mkdir -p obj
	# Same gnoga preparation as build, required before and after a
	# full sync (cf. the build target comment).
	@ ./tools/prepare_gnoga.sh
	alr --non-interactive build --release \
	  && echo release > $(ALIRE_STAMP)
	# Alire profiles : --release --validation --development (default)
	@ $(MAKE) -s refresh_bbt --directory=tests

# The full release chain (cf. the template and the shared
# release_procedure.md): a profile never reuses another profile's
# results, and only a green replay unlocks the release. The chain
# starts from a complete clean (distclean), rebuilds bbt in release
# mode and the tools (which also recreates the tests/ links and the
# unit test program), then replays the whole suite on the release
# executable, writing the results under $(RESULTS_ROOT)/release.
# bbt is rebuilt before the tools: the tools target runs the tests/
# setup, which refreshes the links from the binaries.
release:
	echo
	echo === full release chain: distclean, release builds, replay
	$(MAKE) distclean
	$(MAKE) build-release
	$(MAKE) tools
	$(MAKE) -s PROFILE=release test-run

# BACKEND selects what check runs, cli by default:
#   cli (default): the CLI suites (features, examples, non regression
#                  and unit tests)
#   gtk4         : the shared GUI suite (backends/scenarios)
#   webserver    : not implemented yet (restructuring step 3)
#   all          : cli + every implemented backend (gtk4 today)
BACKEND ?= cli

check: bbt$(EXE_SUFFIX)
	@ if [ -n "$(filter-out cli all,$(BACKEND))" ]; then \
	    $(MAKE) -s --directory=backends check-$(BACKEND); \
	  elif [ -n "$(filter gtk4 webapp adi2 webserver,$(MAKECMDGOALS))" ] \
	    && [ "$(BACKEND)" != "all" ]; then \
	    $(MAKE) -s --directory=backends \
	            check-$(filter gtk4 webapp adi2 webserver,$(MAKECMDGOALS)); \
	  elif [ "$(BACKEND)" = "all" ]; then \
	    $(MAKE) -s test-run; $(MAKE) -s --directory=backends all; \
	  else \
	    $(MAKE) -s test-run; \
	  fi

# The suite runner shared by check and release (cf. the template):
# the tests are the same, the profile only selects the executable
# under test (the tests/ link refreshed by the build targets) and the
# results directory. Delegated sequentially, not as a dependency, so
# that the release chain fully orders its own steps.
test-run: sut$(EXE_SUFFIX) rpl$(EXE_SUFFIX)
	echo '=== running the tests (profile: $(PROFILE))'
	@ $(MAKE) -s check --directory=tests RESULTS_DIR=../$(RESULTS_DIR)

features: bbt$(EXE_SUFFIX) sut$(EXE_SUFFIX)
	$(MAKE) -s features --directory=tests

examples: bbt$(EXE_SUFFIX) sut$(EXE_SUFFIX)
	$(MAKE) -s examples --directory=tests

# Alias: the sanity alias must carry an explicit (empty) recipe, else
# make would pick the catch-all single-test rule below to build it.
sanity: examples
	@:

# Per-backend thin aliases over the backends/Makefile check-<kind>
# rule, so that 'make gtk4' works beside 'make check BACKEND=gtk4'.
# One alias per implemented backend kind: build, witness and suite
# logic stay in backends/Makefile. Skipped when the run also asks for
# 'check': the check recipe then plays the requested backends itself
# (through the MAKECMDGOALS filter above), and the alias would
# otherwise replay the whole backend suite again.
gtk4 webapp adi2:
	@ if [ -z "$(filter check,$(MAKECMDGOALS))" ]; then \
	    $(MAKE) -s --directory=backends check-$@; \
	  fi

doc: ./bbt$(EXE_SUFFIX)
	echo === doc prod
	@ $(MAKE) -s doc --directory=tests
	@ $(MAKE) -s doc --directory=backends

	./bbt help grammar  > docs/grammar.md
	./bbt help keywords > docs/keywords.md

	echo 'Fixme in current version'	>  fixme_index.md
	echo '------------------------'	>> fixme_index.md
	echo                            >> fixme_index.md
	echo 'Location | Text'          >> fixme_index.md
	echo '---------|-----'          >> fixme_index.md
	grep -rni 'Fixme:' docs/* src/* | LC_ALL=C sort | sed -E "s/:/|/2;s/[Ff][Ii][Xx][Mm][Ee]://;s/(^[^:]*)/[\1](\.\.\/\.\.\/\1)/" >> fixme_index.md
	mv fixme_index.md docs/dev/fixme_index.md

	echo === metrics badges
	./tools/gen_metrics_badges.sh

	echo Checking links in md files
ifeq ($(OS), Windows_NT)
	- mlc --ignore-path alire --ignore-path docs/tests_results/Linux --ignore-path docs/tests_results/Darwin | grep '\[Err' && echo OK 
else ifeq ($(PLATFORM), Darwin)
	- mlc --ignore-path alire --ignore-path docs/tests_results/Windows --ignore-path docs/tests_results/Linux | grep '\[Err' && echo OK 
else ifeq ($(PLATFORM), Linux)
	- mlc --ignore-path alire --ignore-path docs/tests_results/Windows --ignore-path docs/tests_results/Darwin | grep '\[Err' && echo OK 
else
	echo "Unknown platform $(PLATFORM)"
	exit 1
endif
	# mlc is much faster than markdown-link-check and gives no false positive yet
	# > apt install cargo
	# > cargo install mlc
	# > fish_add_path ~/.cargo/bin/
	echo

install: ./bbt$(EXE_SUFFIX)
	echo --- install:
	cp -p bbt$(EXE_SUFFIX) ~/bin
	echo OK
	echo

.PHONY : clean distclean

# clean removes the test run artifacts only,
# keeping the built binaries and links usable
clean:
	echo --- clean:
	@ $(MAKE) -s clean --directory=tests
	@ $(MAKE) -s clean --directory=backends
	@ - rm -rf config.ini *.out dir* tmp.txt output2.txt \
	  main main.c main$(EXE_SUFFIX) tmp html js css
	# The local results (ignored files in the profile directories) go
	# as well, but the tracked CI records at the same paths stay, so
	# that a local run can still be diffed against the last committed
	# one (cf. AGENTS.md, Test results).
	@ - git clean -fqX $(RESULTS_ROOT)/validation $(RESULTS_ROOT)/release
	echo OK
	echo

# distclean removes everything that can be rebuilt,
# including the binaries and the links
distclean: clean
	echo --- distclean:
	@ alr -q clean
	cd tools && alr clean
	@ - rm -f $(ALIRE_STAMP)
	@ $(MAKE) -s distclean --directory=tests
	@ $(MAKE) -s clean --directory=backends
	echo OK
	echo
