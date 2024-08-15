# vars
PACKAGE_NAME := eyeris
R_CMD := Rscript -e

# default target
all: install_deps build install roxygenize renv clean

# restore renv packages
install_deps:
	R -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv'); renv::restore()"

# build package
build:
	$(R_CMD) "renv::activate(project = '../${PACKAGE_NAME}')"
	$(R_CMD) "devtools::document(pkg = '../${PACKAGE_NAME}')"
	mkdir build
	$(R_CMD) "devtools::build(pkg = '../${PACKAGE_NAME}', path = 'build')"
	@echo "[ OK ] - eyeris package built successfully!"

# install package
install:
	$(R_CMD) "devtools::install_local(path = Sys.glob('build/${PACKAGE_NAME}_*.tar.gz'))"
	@echo "[ OK ] - eyeris package installed successfully!"

# check package
check:
	$(R_CMD) "devtools::check(pkg = '../${PACKAGE_NAME}')"
	@echo "[ OK ] - eyeris package check completed!"

# roxygenize
roxygenize:
	$(R_CMD) "roxygen2::roxygenize('../${PACKAGE_NAME}')"
	@echo "[ OK ] - roxygenize completed!"

# renv snapshot and status
renv:
	$(R_CMD) "renv::snapshot()"
	$(R_CMD) "renv::status()"
	@echo "[ OK ] - renv snapshot saved!"

# clean build directory
clean:
	rm -rf build
	@echo "[ OK ] - eyeris build directory cleaned!"

.PHONY: all install_deps build install check roxygenize renv clean