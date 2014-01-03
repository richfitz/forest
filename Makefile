all:
	make -C src all

install:
	R CMD INSTALL .

clean:
	make -C src clean

test:
	make -C inst/tests test

.PHONY: all install clean test
