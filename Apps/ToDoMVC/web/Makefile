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

# Optional STORY variable: when set, restrict the check to the given
# story files (relative to ./scenarios), e.g.
#   make check STORY=add_a_task.md
# Optional GUI=1 variable: run the scenarios under the interactive bbt
# --gui debugger window.
SCENARIOS := $(if $(STORY),$(addprefix ./scenarios/,$(STORY)),./scenarios)

.PHONY: all help build run check clean

all: build

help:
	@echo "Targets:"
	@echo "  all   - build the app (default target)"
	@echo "  build - build ./$(APP) with Alire"
	@echo "  run   - build then serve the app on http://127.0.0.1:$(PORT)"
	@echo "  check - run the shared GUI scenarios (Apps/ToDoMVC/docs,"
	@echo "          /scenarios, linked in ./scenarios) with --backend=webapp"
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
	bbt --yes $(BBT_OPTIONS) $(if $(GUI),--gui,) --backend=webapp $(SCENARIOS)

clean:
	alr clean
	rm -f todos.state
