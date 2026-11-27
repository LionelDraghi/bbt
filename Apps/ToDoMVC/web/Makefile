# bbt, the black box tester (https://github.com/LionelDraghi/bbt)
# SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
# SPDX-FileCopyrightText: 2026 Lionel Draghi
#
# Local Makefile for the ToDoMVC web witness app.
# `make help` lists the targets.

APP := todomvc

# The port the app serves on by default (gnoga's --gnoga-port=<n>
# overrides it).
PORT := 8080

# The backend under test, and where its runs are recorded: one flat
# directory per platform and profile, the same as the CLI suites
# (tests/Makefile). The backend names its report instead of taking a
# subdirectory of its own, so that every result of a platform shows in
# one listing: <Platform>/<profile>/<backend>_gui_results.xml.
BACKEND := webapp
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
# --gui debugger window. A debug session is not a check: it writes no
# test report.
ifeq ($(OS), Windows_NT)
	# ./scenarios holds plain files carrying the link target on Windows
	# (a symlink needs a privilege): run the stories where they live
	SCENARIOS := $(if $(STORY),$(addprefix ../docs/features/,$(STORY)),../docs/features)
else
	SCENARIOS := $(if $(STORY),$(addprefix ./scenarios/,$(STORY)),./scenarios)
endif

.PHONY: all help build run check clean

all: build

help:
	@echo "Targets:"
	@echo "  all   - build the app (default target)"
	@echo "  build - build ./$(APP) with Alire"
	@echo "  run   - build then serve the app on http://127.0.0.1:$(PORT)"
	@echo "  check - run the shared GUI scenarios (Apps/ToDoMVC/docs,"
	@echo "          /scenarios, linked in ./scenarios) with --backend=$(BACKEND),"
	@echo "          and record the run in $(RESULTS_DIR)/$(BACKEND)_gui_results.*"
	@echo "          restrict to some stories with STORY=<files>"
	@echo "          add GUI=1 to run under the interactive bbt --gui"
	@echo "          debugger window"
	@echo "  clean - remove everything that can be rebuilt, and run residues"

build:
	alr build

run: build
	./$(APP)

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
