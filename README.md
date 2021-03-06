
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ComSpat)](https://CRAN.R-project.org/package=ComSpat)
[![Last-changedate](https://img.shields.io/badge/last%20change-2021--08--19-yellowgreen.svg)](/commits/master)
<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# ComSpat

R functions for analyzing the ‘within-**Com**munity **Spat**ial
organization’ of species combinations to model plant species
co-occurrence patterns as a function of increasing sampling resolution.

## Description

We present ComSpat, a new R package that uses grid or transect data sets
to measure the number of realized (observed) species combinations (NRC)
and the Shannon diversity of realized species combinations
(compositional diversity; CD) as a function of spatial scale. NRC and CD
represent two measures from a model family developed by Pál Juhász-Nagy
based on Information Theory (see Juhász-Nagy, 1967, 1976, 1984a, 1984b,
1993; Juhász-Nagy & Podani, 1983).

To assist users in detecting and interpreting spatial associations and
infering assembly mechanisms, ComSpat offers complete spatial randomness
and random shift null models, which assists users to disentangle the
textural, intraspecific, and interspecific effects on the observed
spatial patterns. Our open-sourced package provides a vignette that
describes the method and reproduces the figures from this paper to help
users contextualize and apply functions to their data.

For any questions, comments or bug reports please submit an issue here
on GitHub. Suggestions, ideas and references of new algorithms are
always welcome.

## News

-   July-2021: Version 1.0 is available from GitHub.

<div style="display: flex;">

<div>

## Main functionalities

-   Calculates two information theory models based on species
    combinations as a function of spatial scale, specifically;
    -   the number of realized (observed) species combinations (NRC)
    -   the Shannon diversity of realized species combinations
        (Compositional Diversity; CD)
-   Allows for the application of null models:
    -   complete spatial randomness (CSR) helps to show the combined
        effects of individual species level spatial aggregations and
        interspecific associations on observed (realized) coexistence
        relationships
    -   random shift (RS) helps to show the effects of interspecific
        associations after removing the effects of intraspecific
        aggregations on observed (realized) coexistence relationships

</div>

<div>

<br /> <br /> <br />

<center>
<img src="https://github.com/jamestsakalos/ComSpat/blob/master/vignettes/Animation_3_Steps.gif?raw=true" style="width:65.0%" />
</center>

</div>

</div>

## Installation from the source

You can install the released version of ComSpat from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ComSpat")
```

And the development version from
[GitHub](https://github.com/jamestsakalos/ComSpat) with:

``` r
# install.packages("devtools")
devtools::install_github("jamestsakalos/ComSpat")
```

## Example

This is a basic example which shows you how to use the main ComSpat
function:

``` r
library(ComSpat)

data("grid.random") #input data frame
data("param.grid") #input paramater data frame
temp<-ComSpat(data = grid.random, params = param.grid[1:5,], dim_max = 64, type = "Grid")
```

The package’s vignette provides detailed explanation and demonstration
on the application of ComSpat.

## References

Juhász-Nagy, P. (1967). On association among plant populations I. *Acta
Biologica Debrecina*, 5, 43–56.

Juhász-Nagy, P. (1976). Spatial dependence of plant populations. Part 1.
Equivalence analysis (an outline for a new model). *Acta Botanica
Academiae Scientiarum Hungaricae*, 22, 61–78.

Juhász-Nagy, P. & Podani, J. (1983). Information theory methods for the
study of spatial processes and succession. *Vegetatio*, 51, 129–140.

Juhász-Nagy, P. (1984a). Notes on diversity. Part I. Introduction.
*Abstracta Botanica*, 8, 43–55.

Juhász-Nagy, P. (1984b). Spatial dependence of plant populations. Part
2. A family of new models. *Acta Botanica Hungarica*, 30, 363–402.

Juhász-Nagy, P. (1993). Notes on compositional diversity.
*Hydrobiologia*, 249, 173–182.
