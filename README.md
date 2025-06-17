
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aphantasiaReasoningViie

<!-- badges: start -->

[<img alt="alt_text" src="https://img.shields.io/badge/OSF-https://osf.io/hfbcp/-337AB7?logo=osf"/>](https://osf.io/hfbcp/)
<!-- badges: end -->

aphantasiaReasoningViie is a data analysis project disguised as an R
package.

The R package structure was chosen to facilitate the sharing of the code
and data with the scientific community, and to make it easy to reproduce
the analyses. It is not intended to be a general-purpose package, but
rather a collection of functions and data specific to this study
(although many functions are reusable in their own right). The package
development workflow (see [this reference book](https://r-pkgs.org/)) is
also a good way to ensure that the code is well-documented and tested,
which is important for reproducibility in scientific research.

## Installation

The code to install the development version of aphantasiaReasoningViie
is the following:

``` r
# install.packages("pak")
pak::pak("m-delem/aphantasiaReasoningViie")
```

Note that the current WIP version of the package is private, so this
might be unstable. Cloning the repository to work on it is the way to go
for now. Once you have cloned the repository, launch the R project in
RStudio by opening the `aphantasiaReasoningViie.Rproj` file and run the
following command:

``` r
devtools::load_all()
#> ℹ Loading aphantasiaReasoningViie
#> Welcome to aphantasiaReasoningViie.
```

… Which will load the package and make all its functions and data
available in your R session.
