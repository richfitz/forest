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
	R CMD check forest_0.1.tar.gz

.PHONY: all install clean test
