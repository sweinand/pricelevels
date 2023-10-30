\name{bilateral.index}

\alias{jevons}
\alias{carli}
\alias{dutot}
\alias{harmonic}
\alias{laspey}
\alias{paasche}
\alias{fisher}
\alias{walsh}
\alias{toernq}

\title{Bilateral price indices}

\description{Calculation of bilateral price indices. Currently, Jevons, Carli, Harmonic and Dutot index (unweighted indices), and Laspeyres, Paasche, Fisher, Toernqvist and Walsh (weighted indices) are implemented.}

\usage{
# unweighted indices:
jevons(p, r, n, base = NULL, settings = list())
carli(...)
harmonic(...)
dutot(...)

# weighted indices:
laspey(p, r, n, q, w = NULL, base = NULL, settings = list())
paasche(...)
fisher(...)
toernq(...)
walsh(...)
}

\arguments{
   \item{p}{A numeric vector of prices.}
   \item{r, n}{A character vector or factor of regional entities \code{r} and products \code{n}, respectively.}
   \item{q, w}{A numeric vector of non-negative quantities \code{q} or weights \code{w} to be used. Either \code{q} or \code{w} must be provided for weighted indices. If \code{q} and \code{w} are both given, \code{q} will be used in the calculations.}
   \item{base}{A character specifying the base region to which all price levels are expressed. If \code{NULL}, base region is set internally.}
   \item{settings}{A list of control settings to be used. The following settings are supported:
   \itemize{
   \item \code{chatty} : A logical specifying if warnings and info messages should be printed or not. The default is \code{TRUE}.
   \item \code{connect} : A logical specifying if the data should be checked for connectedness or not. The default is \code{TRUE}. If the data are not connected, price levels are computed within the biggest block of connected regions or the block of regions to which the \code{base} region belongs. See also \code{\link{connect}()}.
   }}
}

\author{Sebastian Weinand}

\references{
ILO, IMF, OECD, UNECE, Eurostat and World Bank (2020). \emph{Consumer Price Index Manual: Concepts and Methods}. Washington DC: International Monetary Fund.
}

\details{
Before calculations start, missing values are excluded and duplicated observations for \code{r} and \code{n} are aggregated, that is, duplicated prices \code{p} and weights \code{w} are averaged and duplicated quantities \code{q} added up.

The weights in \code{w} are normalized internally for each regional comparison, such that they add up to 1.
}

\value{A named vector of price levels.}

\examples{
# sample complete price data:
set.seed(123)
dt1 <- rdata(R=3, B=1, N=5)

# compute jevons and toernqvist index:
dt1[, jevons(p=price, r=region, n=product, base="1")]
dt1[, toernq(p=price, r=region, n=product, q=quantity, base="1")]

# add price data:
dt2 <- rdata(R=4, B=1, N=4)
dt2[, "region":=factor(region, labels=4:7)]
dt2[, "product":=factor(product, labels=6:9)]
dt <- rbind(dt1, dt2)
dt[, is.connected(r=region, n=product)] # non-connected now

# compute jevons and toernqvist index:
dt[, jevons(p=price, r=region, n=product, base="1")]

# change base region:
dt[, jevons(p=price, r=region, n=product, base="4")]
}
