
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

# ComSpat

Spatially explicit R functions for analyzing the ‘within-**Com**munity
**Spa**tial organization’ of species combinations to model plant species
co-occurrence patterns as a function of increasing sampling resolution.

## Description

The package contains diversity measures emerging from a family of models
grounded in Information Theory (see Juhász-Nagy, 1967, 1976, 1984a,
1984b, 1993; Juhász-Nagy & Podani, 1983).

The authors intend to gradually develop ComSpat to accommodate for
several additional measures of Juhász-Nagy’s model family.

For any questions, comments or bug reports please submit an issue here
on GitHub. Suggestions, ideas and references of new algorithms are
always welcome.

## News

-   July-2021: Version 1.0 is available from GitHub.

<div style="display: flex;">

<div>

## Main functionalities

-   Calculates the number of realized (observed) species combinations
    (NRC)
-   Calculates the Shannon diversity of realized species combinations
    (Compositional diversity; CD)
-   Allows for the application of null models:
    -   complete spatial randomness (CSR) where within- and
        between=species relationships are randomized
    -   random shift (RS) where only interspecific relationships are
        randomized
-   Plotting of CD and NRC

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
