
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sbt <img src="man/figures/logo.png" align="right" height=140/>

<!-- ![GitHub contributors](https://img.shields.io/github/contributors/quantifish/sbt) -->

<!-- ![GitHub issues](https://img.shields.io/github/issues/quantifish/sbt) -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/quantifish/sbt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/quantifish/sbt/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**Southern bluefin roam,**  
**Data rich as ocean tides—**  
**Code unlocks their truth.**

## Introduction

The R package `sbt` was developed for doing stock assessments of
southern bluefin tuna (SBT) for the Commission for the Conservation of
Southern Bluefin Tuna (CCSBT).

The `sbt` model was coded using RTMB package which provides an R
interface for Template Model Builder (TMB), avoiding the need to code in
C++ ([Kristensen et al. 2016](#ref-Kristensen2016)). The `sbt` model is
bespoke to SBT: it is age structured, with two seasons per year, and one
region. Six fisheries are defined using an area as fleets approach and
time varying selectivity is defined for some of these fisheries.
Frequentist inference can be done using the R package `nlminb` and
Bayesian inference can be done using the R package `adnuts`. The help
pages for the `sbt` package can be found on the website
(<http://www.quantifish.co.nz/sbt/>). The `Getting started` and
`Articles` tabs on the website are the best place to see what `sbt` can
do.

The `sbt` logo was painted by Joanne Webber
(<https://www.joannewebber.co.nz/>).

## Installation

There are several options for installing the `sbt` R package.

### Option 1

The `sbt` package can be installed from within R without needing to
clone the repository using:

``` r
remotes::install_github(repo = "quantifish/sbt", dependencies = TRUE, 
                        auth_token = "your_PAT")
```

### Option 2

The `sbt` package is available in a private GitHub repository. The
repository can be cloned to your computer from the command line or using
a user interface. From the command line using Linux, the repository can
be cloned using:

``` r
git clone https://github.com/quantifish/sbt
```

Because the `sbt` repository is private, you will be prompted to enter
your username and personal access token (PAT). After cloning the
repository, the `sbt` package can also be installed from within R using:

``` r
devtools::install("sbt")
```

### Option 3

The `sbt` package can be installed from the command line by cloning the
repository and then (from Linux) using:

``` r
R CMD INSTALL sbt
```

### Option 4

If you have the `sbt` package downloaded on your local machine as a
`zip` or `tar.gz` file, you can install it using the
`install.packages()` function passing in the path where the zip file is
saved, setting `repos = NULL` and `type = source`. For example:

``` r
install.packages("sbt_1.0.0_R_x86_64-pc-linux-gnu.tar.gz", repos = NULL, 
                 type = "source")
```

## Help

Help for all `sbt` functions and data sets can be found on the R help
pages associated with each function and data set. Help for a specific
function can be viewed using `?function_name`, for example:

``` r
?get_weight_at_age
?plot_cpue
```

Alternatively, to see a list of all available functions and data sets
use:

``` r
help(package = "sbt")
```

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-Kristensen2016" class="csl-entry">

Kristensen, Kasper, Anders Nielsen, Casper W. Berg, Hans Skaug, and
Bradley M. Bell. 2016. “TMB: Automatic Differentiation and Laplace
Approximation.” *Journal of Statistical Software* 70 (5): 1–21.
<https://doi.org/10.18637/jss.v070.i05>.

</div>

</div>
