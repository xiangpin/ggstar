PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: rd check1 clean

rd:
	Rscript -e 'library(methods);devtools::document()'

readme:
	Rscript -e 'rmarkdown::render("README.Rmd", encoding="UTF-8")'

build1:
	Rscript -e 'devtools::build()'

build2:
	Rscript -e 'devtools::build(vignettes = FALSE)'

install:
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check1: rd build1
	cd ..;\
	Rscript -e 'rcmdcheck::rcmdcheck("$(PKGNAME)_$(PKGVERS).tar.gz")'

crancheck: rd build1
	cd ..;\
	Rscript -e 'rcmdcheck::rcmdcheck("$(PKGNAME)_$(PKGVERS).tar.gz", args="--as-cran")'

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/