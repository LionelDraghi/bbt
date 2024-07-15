.SILENT:
all: build check doc

build:
	alr build --development
	cd tools && alr build --development
	# Alire profiles : --release --validation --development (default)


check: ./bbt
	@ $(MAKE) -s check --directory=tests
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

doc: ./bbt
	echo --- doc:
	@ $(MAKE) doc --directory=tests
	
	./bbt -lg > docs/grammar.md
	./bbt -lk > docs/keywords.md
	./bbt -ct
	mv bbt_template.md docs/
	
	echo 'Fixme in current version:'		    >  /tmp/fixme.md
	echo '-------------------------'	    	>> /tmp/fixme.md
	echo                                		>> /tmp/fixme.md
	echo 'Location | Text'             		    >> /tmp/fixme.md
	echo '---------|-----'             		    >> /tmp/fixme.md
	rgrep -n "Fixme:" src/*     | sed "s/:/|/2"	>> /tmp/fixme.md
	mv /tmp/fixme.md docs/fixme.md

	echo 'Issue references in current version:'	>  /tmp/issue.md
	echo '------------------------------------'	>> /tmp/issue.md
	echo                                		>> /tmp/issue.md
	echo 'Location | Text'             		    >> /tmp/issue.md
	echo '---------|-----'             		    >> /tmp/issue.md
	rgrep -n "Issue #" src/ docs/tests/ | sed "s/:/|/2"	>> /tmp/issue.md
	mv /tmp/issue.md docs/issue.md

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
	@ - rm -rf config.ini *.out dir1 docs/tests/*/*.out
	echo OK
	echo
