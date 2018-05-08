
<!-- README.md is generated from README.Rmd. Please edit that file -->

# adrift – Modelling drift on admixture graphs

[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![lifecycle](http://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Project Status:
Active](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Last-changedate](https://img.shields.io/badge/last%20change-2018--05--08-green.svg)](/commits/master)
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
library(ggplot2)
library(ggraph)
library(matchbox)
library(adrift)
#> Loading required package: Rcpp

ag_layout <- function(graph, circular, ...) {
    g$layout()
    cbind(g$get_node_positions() %>% dplyr::select(x, y),
          graph, circular = NA)
}

g <- parse_dot(readr::read_file("data-raw/Basic_OngeEA_wArch.dot"))
graph <- tidygraph::tbl_graph(nodes = g$get_ggraph_nodes(),
                              edges = g$get_ggraph_edges())
graph %>%
    ggraph(ag_layout) +
    geom_edge_link(edge_width = 0.8, edge_colour = "darkblue") +
    geom_node_text(aes(filter = is_leaf, label = label),
                   size = 4, nudge_y = -0.4, angle = -10) +
    geom_node_label(aes(filter = !is_leaf, label = label),
                    size = 3, nudge_y = -0.1, repel = TRUE) +
    coord_cartesian(clip = "off") +
    theme_graph()
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="100%" />
