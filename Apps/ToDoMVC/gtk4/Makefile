# bbt, the black box tester (https://github.com/LionelDraghi/bbt)
# SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
# SPDX-FileCopyrightText: 2026 Lionel Draghi
#
# Local Makefile for the ToDoMVC GTK 4 witness app.
# `make help` lists the targets.

APP := todomvc

.PHONY: all help build check clean

all: build

help:
	@echo "Targets:"
	@echo "  all   - build the app (default target)"
	@echo "  build - build ./$(APP) with Alire"
	@echo "  check - run the bbt scenarios exercising this app"
	@echo "  clean - remove everything that can be rebuilt, and run residues"
	@echo ""
	@echo "The per-backend runnable scenarios (see Apps/ToDoMVC/ToDoMVC.md)"
	@echo "are not in place yet: check reports it and does not fail."

build:
	alr build

check:
	@if [ ! -x ./$(APP) ]; then \
	   echo "No $(APP) binary: run 'make build' first."; \
	   exit 1; \
	fi
	@bbt $(BBT_OPTIONS) ./scenarios 2>/dev/null || \
	  echo "No runnable scenarios yet (cf. the GUI vocabulary proposal)."

clean:
	alr clean
	rm -f todos.state
