SOURCES = $(shell ls *.cpp)

TARGETS = forest.so

# Have to set this up manually, otherwise we can't find Rcpp or BH's
# headers.
RCPP_FLAG = $(shell Rscript -e "Rcpp:::CxxFlags()")
BH_PATH = $(shell Rscript -e "library(BH); writeLines(file.path(path.package('BH'), 'include'))")
BH_FLAG = -I$(BH_PATH)
export PKG_CPPFLAGS = $(RCPP_FLAG) $(BH_FLAG)

all: $(TARGETS)

forest.so: $(SOURCES)
	R CMD SHLIB -o $@ $(SOURCES)

clean:
	rm -f *.o *.so

.PHONY: all clean
