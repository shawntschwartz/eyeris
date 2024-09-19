# default target
all: uninstall getdeps build install roxygenize readme clean

# debugging target
debug: uninstall build install clean

# uninstall dev version of package if loaded
uninstall:
	Rscript -e "if (!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools', repos = 'http://cran.us.r-project.org')"
	Rscript -e "if (requireNamespace('eyeris', quietly = TRUE)) devtools::unload('eyeris')"
	Rscript -e "if (requireNamespace('eyeris', quietly = TRUE)) remove.packages('eyeris')"

# install deps
getdeps:
	Rscript -e "install.packages(c('eyelinker', 'dplyr', 'gsignal', 'tidyr', 'zoo'), repos = 'http://cran.us.r-project.org')"

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

# readme
readme:
	Rscript -e "devtools::build_readme()"
	@echo "[ OK ] - README update completed!"

# clean build directory
clean:
	rm -rf build
	@echo "[ OK ] - eyeris build directory cleaned!"
