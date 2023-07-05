\name{elementary_index}

\alias{jevons}
\alias{carli}
\alias{dutot}
\alias{harmonic}

\title{Elementary price indices}

\description{Calculation of elementary price indices. Currently, Jevons, Carli, Harmonic and Dutot index are implemented.}

\usage{
jevons(x, r, n, base = NULL)
carli(...)
harmonic(...)
dutot(...)
}

\arguments{
   \item{x}{A numeric vector of prices.}
   \item{r, n}{A character vector or factor of regional entities \eqn{r} and products \eqn{n}, respectively.}
   \item{base}{A character specifying the base region to which all price levels are expressed. If \code{NULL}, base region is set internally.}
}

\author{Sebastian Weinand}

\references{
ILO, IMF, OECD, UNECE, Eurostat and World Bank (2020). \emph{Consumer Price Index Manual: Concepts and Methods}. Washington DC: International Monetary Fund.
}

\seealso{\code{weighted_index}}

\value{A named vector of price levels.}

\examples{
# sample complete price data:
data <- prices(R = 7, N = 20)
(P1.jev <- data[, jevons(x=price, r=region, n=product, base="r1")])
(P2.jev <- data[, jevons(price, region, product, base="r2")])
all.equal(P1.jev, P2.jev/P2.jev[1]) # true

# introduce gaps:
data_gaps <- data[!gaps(region, product, amount=0.2), ]
(P1.jev <- data_gaps[, jevons(price, region, product, base="r1")])
(P2.jev <- data_gaps[, jevons(price, region, product, base="r2")])
all.equal(P1.jev, P2.jev/P2.jev[1]) # false, transitivity gets lost
}
