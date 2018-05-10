
<!-- README.md is generated from README.Rmd. Please edit that file -->

# adrift – Modelling drift on admixture graphs

[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![lifecycle](http://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Project Status:
Active](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Last-changedate](https://img.shields.io/badge/last%20change-2018--05--10-green.svg)](/commits/master)
[![packageversion](https://img.shields.io/badge/Package%20version-0.0.0.9000-orange.svg?style=flat-square)](commits/master)
[![Travis build
status](https://travis-ci.org/mailund/adrift.svg?branch=master)](https://travis-ci.org/mailund/adrift)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/69pgmvgsmswfqw45?svg=true)](https://ci.appveyor.com/project/mailund/adrift)
[![Coverage
Status](http://img.shields.io/codecov/c/github/mailund/adrift/master.svg)](https://codecov.io/github/mailund/adrift?branch=master)
[![Coverage
Status](http://coveralls.io/repos/github/mailund/adrift/badge.svg?branch=master)](https://coveralls.io/github/mailund/adrift?branch=master)
[![CRAN
status](http://www.r-pkg.org/badges/version/adrift)](https://cran.r-project.org/package=adrift)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/adrift)](https://cran.r-project.org/package=adrift)
[![minimal R
version](https://img.shields.io/badge/R-%E2%89%A53.2-blue.svg)](https://cran.r-project.org/)

The goal of `adrift` is to have an improved `admixturegraph` package
with better visualisation of graphs and graph statistics.

## Installation

You can install the released version of adrift from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("adrift")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mailund/adrift")
```

## Example

``` r
library(magrittr)
library(adrift)
#> Loading required package: Rcpp

graph <- parse_dot(readr::read_file("data-raw/Basic_OngeEA_wArch.dot"))
graph %>% 
    make_graph_plot() %>%
    show_leaf_labels(nudge_y = -0.5) %>%
    show_inner_node_labels()
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="100%" />

``` r
library(magrittr)
library(adrift)

graph <- parse_qpgraph(readr::read_file("data-raw/Basic_OngeEA_wArch.graph"))
graph %>% 
    make_graph_plot(edge_colour = "darkblue") %>%
    show_leaf_labels(nudge_y = -0.5, angle = 15) %>%
    show_inner_node_labels()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
graph <- parse_dot(readr::read_file("data-raw/BosGraph.dot"))
graph %>% 
    make_graph_plot(edge_colour = "red") %>%
    show_leaf_labels(angle = -90, size = 2.8, nudge_y = -1, hjust = 0)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r
graph <- parse_qpgraph(readr::read_file("data-raw/Basic_OngeEA_wArch.graph"))
plt <- graph %>%
    make_graph_plot(
        ggplot2::aes(filter = !admixed),
        edge_colour = "darkblue"
    ) %>%
    show_leaf_labels(nudge_y = -0.5, angle = 15)

admixture_vars <- c(
    "P_P2" = "alpha",
    "K_K2" = "beta",
    "AfrAnc_AdmixedNonAfr" = "gamma",
    "SuperArch_DenisovaAncAnc" = "delta"
)
plt %>% add_admixture_labels(
    admixture_vars,
    linetype = "dotted",
    colour = "darkred"
)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

``` r

plt %>% add_admixture_labels(
    attr(graph, "admixture_proportions"),
    label_size = 3,
    linetype = "dashed",
    colour = "darkgray"
)
```

<img src="man/figures/README-unnamed-chunk-4-2.png" width="100%" />
