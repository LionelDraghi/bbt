# bbt, the black box tester (https://github.com/LionelDraghi/bbt)
# SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
# SPDX-FileCopyrightText: 2026 Lionel Draghi
#
# Makefile for the GUI backend crates (cf. docs/backends.md and the
# crate restructuring proposal). Builds the backend crates and runs
# the shared GUI scenario suite (Apps/ToDoMVC/docs/features,
# linked in each witness directory) against each backend's witness
# application.
#
# The bbt core must be built first at the repository root (make
# build): the backends link its static libbbt.a, and their test runs
# go through its --gui launcher. Adding a backend is a matter of
# adding its parameter lines below (the build-%, check-% and
# witness-% pattern rules do the rest) and, once implemented, of
# adding it to IMPLEMENTED.
#
# Run from the backends directory (make -C backends or cd backends).

ROOT := $(abspath ..)

ifeq ($(OS), Windows_NT)
CORE := $(ROOT)/bbt.exe
else
CORE := $(ROOT)/bbt
endif

# Implemented backends (cf. the restructuring plan: step 2 gtk4,
# step 3 webapp, step 4 adi2)
IMPLEMENTED := gtk4 webapp adi2

# Per-backend parameters: the project file (each one withs the
# common library project transitively), the witness application
# directory, and the built executable path (Exec_Dir ./. in each
# .gpr). Witness apps live in Apps/, are CI-bound test targets, not
# products.
gtk4_GPR     := gtk4/bbt_gtk4.gpr
gtk4_WITNESS := $(ROOT)/Apps/ToDoMVC/gtk4
gtk4_EXE     := gtk4/bbt_gtk4

# The webapp backend drives the witness through a browser: it needs a
# chromedriver and a chromium/chrome binary in the PATH, and no display
# (headless by default).
webapp_GPR     := webapp/bbt_webapp.gpr
webapp_WITNESS := $(ROOT)/Apps/ToDoMVC/web
webapp_EXE     := webapp/bbt_webapp

# The adi2 backend drives its witness through Adi2's own development
# bridge (Adi.MCP, file IPC): no driver, no accessibility bus, but the
# witness itself needs a display, and a development-profile build of
# Adi2 (the bridge is compiled in that profile only).
adi2_GPR     := adi2/bbt_adi2.gpr
adi2_WITNESS := $(ROOT)/Apps/ToDoMVC/adi2
adi2_EXE     := adi2/bbt_adi2

# The per-backend targets are matched by the pattern rules below:
# they must NOT be phony, as GNU make skips the implicit rule search
# (including explicit pattern rules) for phony targets. They are
# nevertheless always run, as no file of these names is ever created.
.PHONY: all check build clean help

help:
	@echo "Usage: make [target]  (in backends/)"
	@echo ""
	@echo "Targets:"
	@echo "  check, all  : build, then check every implemented backend"
	@echo "                ($(IMPLEMENTED))"
	@echo "  build       : build every implemented backend (the common"
	@echo "                library is built transitively by each .gpr)"
	@echo "  check-<b>   : build backend <b> and run its shared GUI"
	@echo "                suite against its witness (local session"
	@echo "                with display and AT-SPI bus for gtk4)."
	@echo "                Restrict to some stories with STORY=<files>"
	@echo "  build-<b>   : build backend <b> only"
	@echo "  witness-<b> : build the witness app of backend <b>"
	@echo "  clean       : remove the backend build and test residues"
	@echo ""
	@echo "The backends take their parameter lines from IMPLEMENTED:"
	@echo "  build-<b>, check-<b>, witness-<b> for $(IMPLEMENTED)"

build: $(foreach b,$(IMPLEMENTED),build-$(b))
	@:

check: $(foreach b,$(IMPLEMENTED),check-$(b))
	@:

all: check
	@:

# Each check-<b> records the run where the CLI suites do: the witness
# Makefile writes the JUnit and Markdown reports in the platform results
# directory, named after the backend
# (docs/tests_results/<Platform>/<profile>/<backend>_gui_results.*, cf.
# docs/gui_matrix.md), and the backend CI publishes them the way the
# main workflow publishes the CLI results. This doc target keeps the doc
# chain uniform (the root make doc calls it like the tests/ one) and is
# the place holder for the future generated docs of the backend crates.
doc:
	echo === doc prod
	echo OK

build-%:
	@ if [ ! -x $(CORE) ]; then \
	    echo "Error: no bbt core build at the repository root:"; \
	    echo "run 'make build' at the repository root first"; \
	    exit 1; \
	  fi
	@ if [ -z "$($*_GPR)" ]; then \
	    echo "Error: no '$*' backend (implemented: $(IMPLEMENTED))"; \
	    echo "Planned: adi2 (restructuring step 4)"; \
	    exit 1; \
	  fi
	alr exec -- gprbuild -q -P $($*_GPR)

# The shared suite entry point of each backend: build, then run the
# witness check (Apps/ToDoMVC/<kind>/Makefile), with the freshly
# built core and backend executable first in PATH, so that the
# witness 'bbt --backend=<kind>' finds the backend built here. Extra bbt
# options (e.g. BBT_OPTIONS=--keep_going) are forwarded.
check-%: build-%
	@ if [ -z "$($*_WITNESS)" ]; then \
	    echo "Error: no witness configured for backend '$*'"; \
	    exit 1; \
	  fi
	@ echo "=== checking the $* backend"
	export PATH="$(ROOT):$(ROOT)/backends/$*:$$PATH"; \
	$(MAKE) -s check --directory=$($*_WITNESS)

witness-%:
	@ if [ -z "$($*_WITNESS)" ]; then \
	    echo "Error: no witness configured for backend '$*'"; \
	    exit 1; \
	  fi
	$(MAKE) -C $($*_WITNESS) build

clean:
	@ - rm -rf common/obj common/lib \
	  $(foreach b,$(IMPLEMENTED),$(b)/obj $($(b)_EXE))
	@ - rm -rf $(foreach b,$(IMPLEMENTED),$(b)/html $(b)/js $(b)/css)
	@echo OK
