.SILENT:
all: build check

build:
	alr build
	cd tools && alr build --development
	# Alire profiles : --release --validation --development (default)


check: bbt
	@ $(MAKE) check --directory=tests
	# --------------------------------------------------------------------
	echo
	echo Coverage report: 
	lcov --quiet --capture --directory obj -o obj/coverage.info
	lcov --quiet --remove obj/coverage.info -o obj/coverage.info \
		"*/adainclude/*" "*.ads" "*/obj/b__*.adb" 
	# Ignoring :
	# - spec (results are not consistent with current gcc version) 
	# - the false main
	# - libs (Standard)

	genhtml obj/coverage.info -o docs/lcov --title "bbt tests coverage" \
		--prefix "/home/lionel/prj/bbt/src" --frames | tail -n 2 > cov_sum.txt
	# --title  : Display TITLE in header of all pages
	# --prefix : Remove PREFIX from all directory names
	# --frame  : Use HTML frames for source code view
	cat cov_sum.txt
	echo

install: bbt
	cp -p bbt ~/bin

.PHONY : clean
clean:
	alr clean
	cd tools && alr clean
	@ $(MAKE) clean --directory=tests
	@ - rm -rf config.ini *.out dir1
