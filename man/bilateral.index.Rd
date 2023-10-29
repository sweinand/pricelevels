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
jevons(p, r, n, base = NULL)
carli(...)
harmonic(...)
dutot(...)

# weighted indices:
laspey(p, r, n, q, w = NULL, base = NULL)
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
# sample complete price data with product-specific weights:
dt <- rdata(R=7, B=1, N=15)
dt[, "weight" := rweights(r=region, b=product, type=~b)]

# jevons:
(P1.jev <- dt[, jevons(p=price, r=region, n=product, base="1")])
(P2.jev <- dt[, jevons(p=price, r=region, n=product, base="2")])
all.equal(P1.jev, P2.jev/P2.jev[1]) # true

# toernqvist:
(P1.toern <- dt[, toernq(p=price, r=region, n=product, w=weight, base="1")])
(P2.toern <- dt[, toernq(p=price, r=region, n=product, w=weight, base="2")])
all.equal(P1.toern, P2.toern/P2.toern[1]) # true

# introduce gaps:
dt.gaps <- dt[!rgaps(region, product, amount=0.2), ]

# jevons:
(P1.jev <- dt.gaps[, jevons(p=price, r=region, n=product, base="1")])
(P2.jev <- dt.gaps[, jevons(p=price, r=region, n=product, base="2")])
all.equal(P1.jev, P2.jev/P2.jev[1]) # false, transitivity gets lost

# toernqvist:
(P1.toern <- dt.gaps[, toernq(p=price, r=region, n=product, w=weight, base="1")])
(P2.toern <- dt.gaps[, toernq(p=price, r=region, n=product, w=weight, base="2")])
all.equal(P1.toern, P2.toern/P2.toern[1]) # false, transitivity gets lost
}
