# bbt, the black box tester (https://github.com/LionelDraghi/bbt)
# SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
# SPDX-FileCopyrightText: 2026 Lionel Draghi
#
# Local Makefile for the ToDoMVC GTK 4 witness app.
# `make help` lists the targets.

APP := todomvc

# The backend under test, and where its runs are recorded: one flat
# directory per platform and profile, the same as the CLI suites
# (tests/Makefile). The backend names its report instead of taking a
# subdirectory of its own, so that every result of a platform shows in
# one listing: <Platform>/<profile>/<backend>_gui_results.xml.
BACKEND := gtk4
PROFILE ?= validation
PLATFORM := $(shell uname -s)
ifeq ($(OS), Windows_NT)
	# cf. tests/Makefile: uname gives MSYS_NT-10.0-* here, and the
	# directory name must not follow the MSYS or Windows version.
	PLATFORM := Windows
endif
RESULTS_DIR ?= ../../../docs/tests_results/$(PLATFORM)/$(PROFILE)

# Optional STORY variable: when set, restrict the check to the given
# story files (relative to ./scenarios), e.g.
#   make check STORY=add_a_task.md
# Optional GUI=1 variable: run the scenarios under the interactive bbt
# --gui debugger window (the working combination with the backend
# selection is --gui --backend=gtk4, cf. src/bbt-driver.adb). A debug
# session is not a check: it writes no test report.
ifeq ($(OS), Windows_NT)
	# ./scenarios holds plain files carrying the link target on Windows
	# (a symlink needs a privilege): run the stories where they live
	SCENARIOS := $(if $(STORY),$(addprefix ../docs/features/,$(STORY)),../docs/features)
else
	SCENARIOS := $(if $(STORY),$(addprefix ./scenarios/,$(STORY)),./scenarios)
endif

.PHONY: all help build check clean

all: build

help:
	@echo "Targets:"
	@echo "  all   - build the app (default target)"
	@echo "  build - build ./$(APP) with Alire"
	@echo "  check - run the shared GUI scenarios (Apps/ToDoMVC/docs,"\
	        "/scenarios, linked in ./scenarios) with --backend=$(BACKEND)"
	@echo "          and record the run in $(RESULTS_DIR)/$(BACKEND)_gui_results.*"
	@echo "          restrict to some stories with STORY=<files>"
	@echo "          add GUI=1 to run under the interactive bbt --gui"
	@echo "          debugger window"
	@echo "  clean - remove everything that can be rebuilt, and run residues"

build:
	alr build

check:
	@if [ ! -x ./$(APP) ]; then \
	   echo "No $(APP) binary: run 'make build' first."; \
	   exit 1; \
	fi
	@mkdir -p $(RESULTS_DIR)
	bbt --yes $(BBT_OPTIONS) $(if $(GUI),--gui,) --backend=$(BACKEND) \
	  $(if $(GUI),,--junit $(RESULTS_DIR)/$(BACKEND)_gui_results.xml) \
	  $(if $(GUI),,--index $(RESULTS_DIR)/$(BACKEND)_gui_results.md) \
	  $(SCENARIOS)

clean:
	alr clean
	rm -f todos.state
