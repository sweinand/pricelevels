\name{pricelevels}

\alias{pricelevels}
\alias{list.indices}

\title{Spatial price indices}

\description{Calculation of multiple spatial price indices at once.}

\usage{
# list all available price indices:
list.indices()

# compute all price indices:
pricelevels(p, r, n, q=NULL, w=NULL, base=NULL, settings=list())
}

\arguments{
   \item{p}{A numeric vector of prices.}
   \item{r, n}{A character vector or factor of regional entities \code{r} and products \code{n}, respectively.}
   \item{q, w}{A numeric vector of non-negative quantities \code{q} or expenditure share weights \code{w} (see details). Either \code{q} or \code{w} must be provided for weighted indices. If both \code{q} and \code{w} are provided, \code{q} will be used.}
   \item{base}{A character specifying the base region to which all price levels are expressed. If \code{NULL}, base region is set internally.}
   \item{settings}{A list of control settings to be used. The following settings are supported:
   \itemize{
   \item \code{chatty} : A logical specifying if warnings and info messages should be printed or not. The default is \code{getOption("pricelevels.chatty")}.
   \item \code{connect} : A logical specifying if the data should be checked for connectedness or not. The default is \code{getOption("pricelevels.connect")}. If the data are not connected, price levels are computed within the biggest block of connected regions or the block of regions to which the \code{base} region belongs. See also \code{\link{connect}()}.
   \item \code{plot} : A logical specifying if the calculated price levels should be plotted or not. If \code{TRUE}, the price ratios of each region are displayed as boxplots and the price levels are added as colored points. The default is \code{getOption("pricelevels.plot")}.
   \item \code{type} : A vector specifying the index methods used to aggregate prices into price indices. See \code{list.indices()} for allowed values. The default is \code{NULL} in which case all possible price indices are computed.
   \item \code{...} : Further settings allowed for the index methods.
   }}
}

\author{Sebastian Weinand}

\details{
Before calculations start, missing values are excluded and duplicated observations for \code{r} and \code{n} are aggregated, that is, duplicated prices \code{p} and weights \code{w} are averaged and duplicated quantities \code{q} added up.

The weights \code{w} must represent expenditure shares defined as \eqn{w_i^r = p_i^r q_i^r / \sum_{j=1}^{N} p_j^r q_j^r}. They are internally (re-)normalized such that they add up to 1 for each region \code{r}.
}

\value{A matrix of price levels where the rows contain the index methods and the columns the regions.}

\examples{
\donttest{# sample complete price data:
set.seed(123)
dt1 <- rdata(R=3, B=1, N=5)

# compute unweighted indices:
dt1[, pricelevels(p=price, r=region, n=product, base="1")]

# compute all indices relying on quantities:
dt1[, pricelevels(p=price, r=region, n=product, q=quantity, base="1")]

# add price data:
dt2 <- rdata(R=4, B=1, N=4)
dt2[, "region":=factor(region, labels=4:7)]
dt2[, "product":=factor(product, labels=6:9)]
dt <- rbind(dt1, dt2)
dt[, is.connected(r=region, n=product)] # non-connected now

# compute all unweighted indices:
dt[, pricelevels(p=price, r=region, n=product, base="1")]

# change base region:
dt[, pricelevels(p=price, r=region, n=product, base="4")]}
}
