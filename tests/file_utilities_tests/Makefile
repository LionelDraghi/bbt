# doc target needs $(DOC_DIR)
# Should be called with 
# 	@ $(MAKE) doc DOC_DIR=$(RESULTS_DIR) --directory=file_utilities_tests 

.PHONY : clean test_file_utilities

all: check

build:
	@ alr -q build

test_file_utilities: build

check: test_file_utilities
ifeq ($(PLATFORM), Windows)
	@ ./test_file_utilities.exe > test_file_utilities.md
else
	@ ./test_file_utilities > test_file_utilities.md
endif
	@ echo
	
test_file_utilities.md: check

doc: test_file_utilities.md
	@ cp test_file_utilities.md $(DOC_DIR)

clean:
	@ - test -f test_file_utilities.md && rm test_file_utilities.md
	@ - alr clean
