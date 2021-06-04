##' Patchy associated grid data
##'
##' The package contains three grid data sets. The grid data sets
##' (64 × 64 sampling units) feature one simulation (grid.patchy.associated)
##' and two simulation derivatives (grid.random and grid.n.ISC) have been
##' produced by randomization. A spatially explicit individual-based simulation
##' model (PATPRO) was selected to produce a realistic grid community pattern
##' (\code{grid.patchy}) and features interspecific and intraspecific spatial
##' dependence due to simulated abiotic and biotic interactions. The
##' (\code{grid.random}) and (\code{grid.patchy.n.ISC}) data sets were produced
##' by CSR and RS randomizations of the original simulation (\code{grid.patchy})
##' . The (\code{grid.patchy.n.ISC}) data set only features intraspecific
##' spatial dependence; interspecific relationships were removed by
##' randomizations. The (\code{grid.random}) data set only features random
##' distributions; both intra- and interspecific relationships were removed by
##' randomizations.
##'
##' @docType data
##' @format A data frame with 1551 species observations on the following three
##' variables.
##' \describe{
##'    \item{X}{X coordinates}
##'    \item{Y}{Y coordinates}
##'    \item{Species}{Species}}
##' @details The \code{X} and \code{Y} coordinates refer to the sampling unit of
##' the species in the sampling universe.
##' @keywords datasets
##' @examples
##' data(grid.patchy.associated)
##' str(grid.patchy.associated)
##' barplot(table(grid.patchy.associated$Species),ylab = "Count (no.)",
##'  space=0.5);box()
##'
"grid.patchy.associated"





##' Random grid data
##'
##' The package contains three grid data sets. The grid data sets
##' (64 × 64 sampling units) feature one simulation (grid.patchy.associated)
##' and two simulation derivatives (grid.random and grid.n.ISC) have been
##' produced by randomization. A spatially explicit individual-based simulation
##' model (PATPRO) was selected to produce a realistic grid community pattern
##' (\code{grid.patchy}) and features interspecific and intraspecific spatial
##' dependence due to simulated abiotic and biotic interactions. The
##' (\code{grid.random}) and (\code{grid.patchy.n.ISC}) data sets were produced
##' by CSR and RS randomizations of the original simulation (\code{grid.patchy})
##' . The (\code{grid.patchy.n.ISC}) data set only features intraspecific
##' spatial dependence; interspecific relationships were removed by
##' randomizations. The (\code{grid.random}) data set only features random
##' distributions; both intra- and interspecific relationships were removed by
##' randomizations.
##'
##' @docType data
##' @format A data frame with 1551 species observations on the following three
##' variables.
##' \describe{
##'    \item{X}{X coordinates}
##'    \item{Y}{Y coordinates}
##'    \item{Species}{Species}}
##' @details The \code{X} and \code{Y} coordinates refer to the sampling unit of
##' the species in the sampling universe.
##' @keywords datasets
##' @examples
##' data(grid.random)
##' str(grid.random)
##' barplot(table(grid.random$Species),ylab = "Count (no.)", space=0.5);box()
##'
"grid.random"



##' Patchy no ISC grid data
##'
##' The package contains three grid data sets. The grid data sets
##' (64 × 64 sampling units) feature one simulation (grid.patchy.associated)
##' and two simulation derivatives (grid.random and grid.n.ISC) have been
##' produced by randomization. A spatially explicit individual-based simulation
##' model (PATPRO) was selected to produce a realistic grid community pattern
##' (\code{grid.patchy}) and features interspecific and intraspecific spatial
##' dependence due to simulated abiotic and biotic interactions. The
##' (\code{grid.random}) and (\code{grid.patchy.n.ISC}) data sets were produced
##' by CSR and RS randomizations of the original simulation (\code{grid.patchy})
##' . The (\code{grid.patchy.n.ISC}) data set only features intraspecific
##' spatial dependence; interspecific relationships were removed by
##' randomizations. The (\code{grid.random}) data set only features random
##' distributions; both intra- and interspecific relationships were removed by
##' randomizations.
##'
##' @docType data
##' @format A data frame with 1551 species observations on the following three
##' variables.
##' \describe{
##'    \item{X}{X coordinates}
##'    \item{Y}{Y coordinates}
##'    \item{Species}{Species}}
##' @details The \code{X} and \code{Y} coordinates refer to the sampling unit of
##' the species in the sampling universe.
##' @keywords datasets
##' @examples
##' data(grid.patchy.n.ISC)
##' str(grid.patchy.n.ISC)
##' barplot(table(grid.patchy.n.ISC$Species),ylab = "Count (no.)",
##' space=0.5);box()
##'
"grid.patchy.n.ISC"




##' Parameter data for the secondary sampling of a grid
##'
##' A data frame containing information on the size and shape of secondary
##' sampling units for the \code{ComSpat} function.
##'
##' The \code{ComSpat} function requires spatially explicit information. The
##' \code{params} argument stores and provides spatially explicit information.
##' The parameter data frame for the grid and transect data (i.e.,
##' \code{params.grid} and \code{params.tran}) are available in the package.
##' The data contains information on the number of scaling steps
##' (\code{Steps.of.scaling} column) and the size (the number of primary
##' sampling units to create the length and height of the secondary sampling
##' unit, captured in the \code{Length.of.plots} and \code{Height.of. plots}
##' columns) of the secondary sampling units. For example, to perform secondary
##' sampling on a grid, the first row of the parameter data set will read 1 for
##' \code{Steps.of.scaling}, \code{Length.of.plots} and \code{Height.of.plots}.
##' If the data was sampled as a \code{"Transect"} only the
##' \code{Length.of.plots} coordinate is required.  If the data was sampled as
##' a \code{"Grid"} both \code{Length.of.plots} and \code{Height.of.plots} are
##' required.
##'
##' \code{Length.of.plots} and \code{Height.of.plots} are measured in the
##' number of sampling units.
##'
##' @name param.grid
##' @docType data
##' @format A data frame with 16 observations (sampling steps) on the following
##' 3 variables.
##' \describe{
##'     \item{Steps.of.scaling}{Numeric. Sampling step number}
##'     \item{Length.of.plots}{Numeric. Length of the sampling unit.}
##'     \item{Height.of.plots}{Numeric. Height of the sampling unit.}}
##' @keywords datasets
##' @examples
##' data(param.grid)
##' str(param.grid)
"param.grid"




##' Transect grassland species data
##'
##' The package contains two transects with real data sampled in open
##' sand grassland in Hungary (Bartha et al. 2008). These real transect data
##' sets were derived from Bartha et al.’s (2008) long-term study on the effect
##' of climate change on the interannual variability of grassland communities.
##' The transect data sets represent: (1) the spatial pattern data of frequent
##' species (species with more than 25 presences along the transect)
##' selected as a threshold from standard textbooks; \code{tran.grass.s}), and
##' (2) the spatial pattern data of plant functional types (PFTs) (in this case
##' data of species which belong to the same functional groups were merged;
##' \code{tran.grass.t}). These data represent the same transect and same
##' community; however, for merging species, all data were used (i.e. the data
##' of rare species were also considered within the particular PFT). For
##' simplicity, we have only selected a 25 m example with a very typical pattern;
##' this extent (transect length) is already representative of the community
##' pattern.
##'
##' @docType data
##' @format A data frame with 713 species observations on the following two
##' variables.
##' \describe{
##'    \item{X}{X coordinates}
##'    \item{Species}{Species}}
##' @details The \code{X} refers to the sampling unit of the species in the
##' sampling universe.
##' @keywords datasets
##' @examples
##' data(tran.grass.s)
##' str(tran.grass.s)
##' barplot(table(tran.grass.s$Species),ylab = "Count (no.)",
##' space=0.5);box()
##'
"tran.grass.s"





##' Transect grassland trait data
##'
##' The package contains two real transects with data sampled in open
##' sand grassland in Hungary (Bartha et al. 2008). These real transect data
##' sets were derived from Bartha et al.’s (2008) long-term study on the effect
##' of climate change on the interannual variability of grassland communities.
##' The transect data sets represent: (1) the spatial pattern data of frequent
##' species (species with more than 25 presences along the transect)
##' selected as a threshold from standard textbooks; \code{tran.grass.s}), and
##' (2) the spatial pattern data of plant functional types (PFTs) (in this case
##' data of species which belong to the same functional groups were merged;
##' \code{tran.grass.t}). These data represent the same transect and same
##' community; however, for merging species, all data were used (i.e. the data
##' of rare species were also considered within the particular PFT). For
##' simplicity, we have only selected a 25 m example with a very typical pattern;
##' this extent (transect length) is already representative of the community
##' pattern.
##'
##' @docType data
##' @format A data frame with 713 species observations on the following two
##' variables.
##' \describe{
##'    \item{X}{X coordinates}
##'    \item{Species}{Species. Note that for the Transect grassland trait data,
##'    'Species' referes to plant functional type.}}
##' @details The \code{X} refers to the sampling unit of the species in the
##' sampling universe.
##' @keywords datasets
##' @examples
##' data(tran.grass.t)
##' str(tran.grass.t)
##' barplot(table(tran.grass.t$Species),ylab = "Count (no.)",
##' space=0.5);box()
##'
"tran.grass.t"





##' Parameter data for the secondary sampling of a transect
##'
##' A data frame containing information on the size and shape of secondary
##' sampling units for the \code{ComSpat} function.
##'
##' The \code{ComSpat} function requires spatially explicit information. The
##' \code{params} argument stores and provides spatially explicit information.
##' The parameter data frame for the grid and transect data (i.e.,
##' \code{param.grid} and \code{param.tran}) are available in the package.
##' The data contains information on the number of scaling steps
##' (\code{Steps.of.scaling} column) and the size (the number of primary
##' sampling units to create the length and height of the secondary sampling
##' unit, captured in the \code{Length.of.plots} and \code{Height.of. plots}
##' columns) of the secondary sampling units. For example, to perform secondary
##' sampling on a grid, the first row of the parameter data set will read 1 for
##' \code{Steps.of.scaling}, \code{Length.of.plots} and \code{Height.of.plots}.
##' If the data was sampled as a \code{"Transect"} only the
##' \code{Length.of.plots} coordinate is required.  If the data was sampled as
##' a \code{"Grid"} both \code{Length.of.plots} and \code{Height.of.plots} are
##' required.
##'
##' \code{Length.of.plots} and \code{Height.of.plots} are measured in the
##' number of sampling units.
##'
##' @name param.tran
##' @docType data
##' @format A data frame with 16 observations (sampling steps) on the following
##' 2 variables.
##' \describe{
##'     \item{Steps.of.scaling}{Numeric. Sampling step number}
##'     \item{Length.of.plots}{Numeric. Length of the sampling unit.}}
##' @keywords datasets
##' @examples
##' data(param.tran)
##' str(param.tran)
"param.tran"
