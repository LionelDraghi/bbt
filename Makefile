.SILENT:
all: build check doc

build:
	alr build
	cd tools && alr build --development
	# Alire profiles : --release --validation --development (default)


check: bbt
	@ $(MAKE) check --directory=tests
	# --------------------------------------------------------------------
	# echo
	# echo Coverage report: 
	# lcov --quiet --capture --directory obj -o obj/coverage.info
	# lcov --quiet --remove obj/coverage.info -o obj/coverage.info \
	# 	"*/adainclude/*" "*.ads" "*/obj/b__*.adb" 
	# # Ignoring :
	# # - spec (results are not consistent with current gcc version) 
	# # - the false main
	# # - libs (Standard)

	# genhtml obj/coverage.info -o docs/lcov --title "bbt tests coverage" \
	# 	--prefix "/home/lionel/prj/bbt/src" --frames | tail -n 2 > cov_sum.txt
	# # --title  : Display TITLE in header of all pages
	# # --prefix : Remove PREFIX from all directory names
	# # --frame  : Use HTML frames for source code view
	# cat cov_sum.txt
	# echo

doc:
	echo --- doc:
	cp tests/pass_tests/pass_tests.md docs
	cp `find tests/pass_tests/*.md` docs/pass_tests
	./bbt -lg > docs/grammar.md
	./bbt -lk > docs/keywords.md
	./bbt -ct
	mv bbt_template.md docs/
	
	>  docs/fixme.md
	rgrep -ni "Fixme" docs/*.md | sed "s/:/|/2"	>> /tmp/fixme.md

	echo 'Fixme in current version:'		>  docs/fixme.md
	echo '-------------------------'		>> docs/fixme.md
	echo                            		>> docs/fixme.md
	echo 'Location | Text'             		>> docs/fixme.md
	echo '---------|-----'             		>> docs/fixme.md
	cat /tmp/fixme.md                       >> docs/fixme.md
	rm /tmp/fixme.md
	rgrep -n "Fixme:" src/*           | sed "s/:/|/2"	>> docs/fixme.md
	rgrep -n "Fixme:" tests/*_tests/* | sed "s/:/|/2"	>> docs/fixme.md

	echo OK
	echo

install: bbt
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
	@ - rm -rf config.ini *.out dir1
	echo OK
	echo
