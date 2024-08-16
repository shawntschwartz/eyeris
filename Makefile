# vars
PACKAGE_NAME := eyeris
R_CMD := Rscript -e

# default target
all: uninstall build install roxygenize clean

# uninstal dev version of package if loaded
uninstall:
	Rscript -e "if (requireNamespace('eyeris', quietly = TRUE)) devtools::unload('${PACKAGE_NAME}')"
	Rscript -e "if (requireNamespace('eyeris', quietly = TRUE)) remove.packages('${PACKAGE_NAME}')"

# build package
build:
	$(R_CMD) "devtools::document(pkg = '.')"
	mkdir build
	$(R_CMD) "devtools::build(pkg = '.', path = 'build')"
	@echo "[ OK ] - eyeris package built successfully!"

install:
	@echo "[ INFO ] - starting eyeris package build..."
	$(R_CMD) "install.packages(Sys.glob('build/${PACKAGE_NAME}_*.tar.gz'), repos = NULL, type = 'source')"
	@echo "[ OK ] - eyeris package installed successfully!"

# check package
check:
	$(R_CMD) "devtools::check(pkg = '.')"
	@echo "[ OK ] - eyeris package check completed!"

# roxygenize
roxygenize:
	$(R_CMD) "roxygen2::roxygenize('.')"
	@echo "[ OK ] - roxygenize completed!"

# clean build directory
clean:
	rm -rf build
	@echo "[ OK ] - eyeris build directory cleaned!"

.PHONY: all uninstall build install check roxygenize clean
