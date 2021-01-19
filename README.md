# gets

![R-CMD-check](https://github.com/gsucarrat/gets.plm/workflows/R-CMD-check/badge.svg)

General-to-Specific (GETS) modelling of 'plm' models (linear panel data models).

# Installation
To install the current development version available here at Github, first download the tarball (i.e. the file named gets.plm_devel.tar.gz). Next, run:

    system("R CMD INSTALL --build gets.plm")

Alternatively, you can try installing the development version directly from GitHub with the following *R* code:

    install.packages(
      "https://github.com/gsucarrat/gets.plm/raw/master/gets.plm_devel.tar.gz",
      repos = NULL, type = "source"
    )
    
# License
This package is free and open source software, licensed under GPL (>= 2)
