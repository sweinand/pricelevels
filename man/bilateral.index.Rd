\name{bilateral.index}

\alias{bilateral.index}
\alias{bmw}
\alias{jevons}
\alias{carli}
\alias{dutot}
\alias{harmonic}
\alias{cswd}
\alias{laspeyres}
\alias{paasche}
\alias{fisher}
\alias{walsh}
\alias{toernqvist}
\alias{medgeworth}
\alias{theil}
\alias{svartia}
\alias{palgrave}
\alias{drobisch}
\alias{geolaspeyres}
\alias{geopaasche}
\alias{geowalsh}
\alias{lowe}
\alias{young}
\alias{uvalue}
\alias{davies}
\alias{banerjee}
\alias{lehr}

\title{Bilateral price indices}

\description{Calculation of bilateral price indices. Currently, the following ones are implemented (see below in alphabetic order).
}

\usage{
banerjee(p, r, n, q, base=NULL, settings=list())

bmw(p, r, n, base=NULL, settings=list())

carli(p, r, n, base=NULL, settings=list())

cswd(p, r, n, base=NULL, settings=list())

davies(p, r, n, q, base=NULL, settings=list())

drobisch(p, r, n, q, w=NULL, base=NULL, settings=list())

dutot(p, r, n, base=NULL, settings=list())

fisher(p, r, n, q, w=NULL, base=NULL, settings=list())

geolaspeyres(p, r, n, q, w=NULL, base=NULL, settings=list())

geopaasche(p, r, n, q, w=NULL, base=NULL, settings=list())

geowalsh(p, r, n, q, w=NULL, base=NULL, settings=list())

harmonic(p, r, n, base=NULL, settings=list())

jevons(p, r, n, base=NULL, settings=list())

laspeyres(p, r, n, q, w=NULL, base=NULL, settings=list())

lehr(p, r, n, q, base=NULL, settings=list())

lowe(p, r, n, q, base=NULL, settings=list())

medgeworth(p, r, n, q, base=NULL, settings=list())

paasche(p, r, n, q, w=NULL, base=NULL, settings=list())

palgrave(p, r, n, q, w=NULL, base=NULL, settings=list())

svartia(p, r, n, q, w=NULL, base=NULL, settings=list())

toernqvist(p, r, n, q, w=NULL, base=NULL, settings=list())

theil(p, r, n, q, w=NULL, base=NULL, settings=list())

uvalue(p, r, n, q, base=NULL, settings=list())

walsh(p, r, n, q, w=NULL, base=NULL, settings=list())

young(p, r, n, q, base=NULL, settings=list())
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
   \item \code{qbase} : A character specifying the region \eqn{b} whose quantities (and prices) should be used in \code{lowe()} and \code{young()}. If \code{NULL}, prices are averaged and quantities added up for each product, i.e. \eqn{p_i^b=\sum_{r=1}^{R} p_i^r / R} and \eqn{q_i^b=\sum_{r=1}^{R} q_i^r}.
   }}
}

\author{Sebastian Weinand}

\references{
ILO, IMF, OECD, UNECE, Eurostat and World Bank (2020). \emph{Consumer Price Index Manual: Concepts and Methods}. Washington DC: International Monetary Fund.
}

\details{
Before calculations start, missing values are excluded and duplicated observations for \code{r} and \code{n} are aggregated, that is, duplicated prices \code{p} and weights \code{w} are averaged and duplicated quantities \code{q} added up.

The weights \code{w} must represent expenditure shares defined as \eqn{w_i^r = p_i^r q_i^r / \sum_{j=1}^{N} p_j^r q_j^r}. They are internally (re-)normalized such that they add up to 1 for each region \code{r}.
}

\value{A named vector of price levels.}

\examples{
# sample complete price data:
set.seed(123)
dt1 <- rdata(R=3, B=1, N=5)

# compute jevons and toernqvist index:
dt1[, jevons(p=price, r=region, n=product, base="1")]
dt1[, toernqvist(p=price, r=region, n=product, q=quantity, base="1")]

# compute lowe index using quantities of region 2:
dt1[, lowe(p=price, r=region, n=product, q=quantity, base="1",
           settings=list(qbase="2"))]

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
