\name{index.pairs}

\alias{index.pairs}

\title{Calculation of bilateral index pairs}

\description{Calculate bilateral index numbers for all pairs of regions.
}

\usage{
index.pairs(p, r, n, w = NULL, type = "jevons", all.pairs = TRUE, as.dt = FALSE)
}

\arguments{
   \item{p}{A numeric vector of prices.}
   \item{r, n}{A character vector or factor of regional entities \eqn{r} and products \eqn{n}, respectively.}
   \item{w}{A numeric vector of weights used in the computation of weighted bilateral index numbers. Can be \code{NULL}, if the index formula specified in \code{type} does not require weights.}
   \item{type}{A character specifying the index to be applied. Allowed values are \code{jevons}, \code{carli}, \code{dutot}, \code{harmonic}, \code{laspey}, \code{paasche}, \code{fisher}, \code{walsh} and \code{toernq}.}
   \item{all.pairs}{Logical indicating whether index numbers should be computed for all region pairs (\code{TRUE}) or only for non-redundant ones (\code{FALSE}), e.g. the index number of regions \code{AB} should be the same as the inverse of \code{BA}.}
   \item{as.dt}{A logical indicating whether the results should be transformed to a data.table (\code{TRUE}) or not (\code{FALSE}).}
}

\author{Sebastian Weinand}

\references{
ILO, IMF, OECD, UNECE, Eurostat and World Bank (2020). \emph{Consumer Price Index Manual: Concepts and Methods}. Washington DC: International Monetary Fund.
}

\value{Either a matrix of price levels or, in case \code{as.dt=TRUE}, a data.table with the following variables:
\tabular{lll}{
   \code{region} \tab \tab region\cr
   \code{base} \tab \tab base region\cr
   \code{price_level} \tab \tab price level of comparison between region and base\cr
}}

\examples{
### (1) equal weights

data <- prices(R = 7, N = 20)
data[, "weights" := 1]

P.je <- data[, index.pairs(p=price, r=region, n=product, type="jevons")]
P.to <- data[, index.pairs(price, region, product, w=weights, type="toernq")]
all.equal(P.je, P.to) # true

P.ca <- data[, index.pairs(price, region, product, type = "carli")]
P.la <- data[, index.pairs(price, region, product, weights, type = "laspey")]
all.equal(P.ca, P.la) # true

P.ha <- data[, index.pairs(price, region, product, type = "harmonic")]
P.pa <- data[, index.pairs(price, region, product, weights, type = "paasche")]
all.equal(P.ha, P.pa) # true

### (2) non-equal weights

data[, "weights" := weights(r=region, n=product, type=~n+r)]

P.je <- data[, index.pairs(price, region, product, type = "jevons")]
P.to <- data[, index.pairs(price, region, product, weights, type = "toernq")]
all.equal(P.je, P.to) # false
}
