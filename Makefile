# default target
all: uninstall getdeps build install roxygenize clean

# uninstall dev version of package if loaded
uninstall:
	Rscript -e "if (!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools')"
	Rscript -e "if (requireNamespace('eyeris', quietly = TRUE)) devtools::unload('eyeris')"
	Rscript -e "if (requireNamespace('eyeris', quietly = TRUE)) remove.packages('eyeris')"

# install deps
getdeps:
	Rscript -e "install.packages(c('eyelinker', 'dplyr', 'gsignal', 'tidyr', 'zoo'))"

# build package
build:
	Rscript -e "devtools::document(pkg = '.')"
	mkdir build
	Rscript -e "devtools::build(pkg = '.', path = 'build')"
	@echo "[ OK ] - eyeris package built successfully!"

install:
	@echo "[ INFO ] - starting eyeris package build..."
	Rscript -e "install.packages(Sys.glob('build/eyeris_*.tar.gz'), repos = NULL, type = 'source')"
	@echo "[ OK ] - eyeris package installed successfully!"

# check package
check:
	Rscript -e "devtools::check(pkg = '.')"
	@echo "[ OK ] - eyeris package check completed!"

# roxygenize
roxygenize:
	Rscript -e "roxygen2::roxygenize('.')"
	@echo "[ OK ] - roxygenize completed!"

# clean build directory
clean:
	rm -rf build
	@echo "[ OK ] - eyeris build directory cleaned!"
