\name{weighted_index}

\alias{laspey}
\alias{paasche}
\alias{fisher}
\alias{walsh}
\alias{toernq}

\title{Weighted price indices}

\description{Calculation of weighted price indices. Currently, Laspeyres, Paasche, Fisher, Toernqvist and Walsh index are implemented.}

\usage{
laspey(p, r, n, w, base = NULL)
paasche(...)
fisher(...)
toernq(...)
walsh(...)
}

\arguments{
   \item{p}{A numeric vector of prices.}
   \item{r, n}{A character vector or factor of regional entities \eqn{r} and products \eqn{n}, respectively.}
   \item{w}{A numeric vector of weights to be applied.}
   \item{base}{A character specifying the base region to which all price levels are expressed. If \code{NULL}, base region is set internally.}
}

\author{Sebastian Weinand}

\references{
ILO, IMF, OECD, UNECE, Eurostat and World Bank (2020). \emph{Consumer Price Index Manual: Concepts and Methods}. Washington DC: International Monetary Fund.
}

\value{A named vector of price levels.}

\seealso{\code{elementary_index}}

\details{
The weights in \code{w} are normalized for each regional comparison, such that they add up to 1.
}

\examples{
# sample complete price data:
data <- prices(R = 7, N = 20, weights = ~n)
(P1.toern <- data[, toernq(p=price, r=region, n=product, w=weight, base="r1")])
(P2.toern <- data[, toernq(price, region, product, weight, base="r2")])
all.equal(P1.toern, P2.toern/P2.toern[1]) # true

# introduce gaps:
data_gaps <- data[!gaps(region, product, amount=0.2), ]
(P1.toern <- data_gaps[, toernq(price, region, product, weight, base="r1")])
(P2.toern <- data_gaps[, toernq(price, region, product, weight, base="r2")])
all.equal(P1.toern, P2.toern/P2.toern[1]) # false, transitivity gets lost
}
