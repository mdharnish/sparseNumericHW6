
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sparseNumericHW6

<!-- badges: start -->

[![R-CMD-check.yaml](https://github.com/mdharnish/sparseNumericHW6/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mdharnish/sparseNumericHW6/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of sparseNumericHW6 is to provide an S4 class implementation
for sparse numeric vectors. This allows efficient storage and
manipulation of numeric datasets that contain many zeros.

By storing only the non-zero values and their positions, sparse_numeric
reduces memory usage while still supporting:

- Arithmetic (`+`, `-`, `*`)

- Coercion to and from base R numeric vectors

- Statistical summaries (`norm()`, `mean()`)

- Standardization (`standardize()`)

- Plotting of shared non-zero indices (`plot()`)

- Unit tests via **testthat** for all functionality

# Installation

You can install the development version of sparseNumericHW6 from GitHub
using:

``` r
devtools::install_github("mdharnish/sparseNumericHW6")
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo mdharnish/sparseNumericHW6@HEAD
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>       ✔  checking for file 'C:\Users\dharn\AppData\Local\Temp\RtmpATrcOY\remotes65b4526a25ad\mdharnish-sparseNumericHW6-31f6327/DESCRIPTION'
#>       ─  preparing 'sparseNumericHW6': (373ms)
#>    checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>       ─  building 'sparseNumericHW6_0.0.0.9000.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/dharn/AppData/Local/R/win-library/4.5'
#> (as 'lib' is unspecified)
```

# Usage

Here is a basic example of how to create and manipulate sparse vectors:

``` r
library(sparseNumericHW6)
#> 
#> Attaching package: 'sparseNumericHW6'
#> The following object is masked from 'package:base':
#> 
#>     norm

# Create sparse vectors from standard numeric vectors
v1 <- sn_vec(c(1, 0, 2, 0, 0))
v2 <- sn_vec(c(0, 0, 3, 0, 4))

# Inspect object (prints internal structure)
v1
#> {"class":"sparse_numeric", "len":5, "nnz":2, "pos":[1,3], "val":[1,2]}
```

# Arithmetic

``` r
v_sum  <- v1 + v2
v_prod <- v1 * v2

as(v_sum, "numeric")
#> [1] 1 0 5 0 4
as(v_prod, "numeric")
#> [1] 0 0 6 0 0
```

# Statistics

``` r
v_std <- standardize(v1)
as(v_std, "numeric")
#> [1]  0.4472136 -0.6708204  1.5652476 -0.6708204 -0.6708204
```

# Plotting

``` r
plot(v1, v2)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />
