all:
	make -C src all

install:
	R CMD INSTALL .

clean:
	make -C src clean

test:
	make -C inst/tests test

DEVTOOLS_DOCUMENT='library(methods); devtools::document()'
document: all
	@mkdir -p man
	Rscript -e ${DEVTOOLS_DOCUMENT}

check:
	R CMD build .
	R CMD check --no-manual `ls -1tr forest*gz | tail -n1`
	@rm -f `ls -1tr forest*gz | tail -n1`
	@rm -rf forest.Rcheck

.PHONY: all install clean test
