.SILENT:
all: build check

build:
	alr build
	cd tools && alr build --validation

check: bbt
	@ $(MAKE) check --directory=tests

install: bbt
	cp -p bbt ~/bin

.PHONY : clean
clean:
	alr clean
	cd tools && alr clean
	@ $(MAKE) clean --directory=tests
	@ - rm -rf config.ini *.out dir1
