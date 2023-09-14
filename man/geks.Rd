\name{geks}

\alias{geks}
\alias{index.pairs}

\title{GEKS method}

\description{Function \code{index.pairs()} computes bilateral index numbers for all pairs of regions. Based on that, function \code{geks()} derives regional price levels using the GEKS method proposed by Gini (1924, 1931), Elteto and Koves (1964), and Szulc (1964).}

\usage{
index.pairs(p, r, n, q = NULL, w = NULL, type = "jevons", all.pairs = TRUE, as.dt = FALSE)

geks(p, r, n, q = NULL, w = NULL, type = "jevons", base = NULL)
}

\arguments{
   \item{p}{A numeric vector of prices.}
   \item{r, n}{A character vector or factor of regional entities \code{r} and products \code{n}, respectively.}
   \item{w, q}{A numeric vector of non-negative quantities \code{q} or weights \code{w}, respectively, used in the computation of weighted bilateral index numbers. Can be \code{NULL}, if the index formula specified in \code{type} does not require quantities or weights. Otherwise, either \code{q} or \code{w} must be given.}
   \item{type}{A character specifying the index number method to be used. See \code{\link{bilateral.index}} for allowed values.}
   \item{base}{A character specifying the base region to which all price levels are expressed. When \code{NULL}, they refer to the (unweighted) regional average.}
   \item{all.pairs}{Logical indicating whether index numbers should be computed for all region pairs (\code{TRUE}) or only for non-redundant ones (\code{FALSE}), e.g. the index number of regions \code{AB} should be the same as the inverse of \code{BA}.}
   \item{as.dt}{A logical indicating whether the results should be transformed to a data.table (\code{TRUE}) or not (\code{FALSE}).}
}

\author{Sebastian Weinand}

\references{
Gini, C. (1924). Quelques Considerations au Sujet de la Construction des Nombres Indices des Prix et des Questions Analogues. \emph{Mentron}, 4 (1), 3-162.

Gini, C. (1931). On the Circular Test of Index Numbers. \emph{International Statistical Review}, 9 (2), 3-25.

Elteto, O. and Koves, P. (1964). On a Problem of Index Number Computation Relating to International Comparison. \emph{Statisztikai Szemle}, 42, 507-518.

Szulc, B. J. (1964). Indices for Multiregional Comparisons. \emph{Przeglad Statystyczny}, 3, 239-254.
}

\value{For \code{geks}, a named vector of price levels. For \code{index.pairs}, a matrix of price levels or, in case \code{as.dt=TRUE}, a data.table with the following variables:
\tabular{lll}{
   \code{region} \tab \tab region\cr
   \code{base} \tab \tab base region\cr
   \code{index} \tab \tab price level of comparison between region and base\cr
}}

\details{
The GEKS index is a two-step approach. First, prices are aggregated into bilateral index numbers using the index given in \code{type}. This is done for all pairs of regions via function \code{\link{index.pairs}}. Second, these bilateral index numbers are transformed into a set of multilateral, transitive index numbers.

Note that the weights \code{w} are used within the aggregation of prices into index numbers (first stage) while the subsequent transformation of these index numbers (second stage) does not rely on any weights.

Before calculations start, missing values are excluded and duplicated observations for \code{r} and \code{n} are aggregated, that is, duplicated prices \code{p} and weights \code{w} are averaged and duplicated quantities \code{q} added up.
}

\seealso{\code{\link{bilateral.index}}}

\examples{
### Index pairs

# (1) equal weights
dt <- rdata(R=7, N=15)
dt[, "weight" := 1]

P.je <- dt[, index.pairs(p=price, r=region, n=product, type="jevons")]
P.to <- dt[, index.pairs(p=price, r=region, n=product, w=weight, type="toernq")]
all.equal(P.je, P.to) # true

P.ca <- dt[, index.pairs(p=price, r=region, n=product, type="carli")]
P.la <- dt[, index.pairs(p=price, r=region, n=product, w=weight, type="laspey")]
all.equal(P.ca, P.la) # true

P.ha <- dt[, index.pairs(p=price, r=region, n=product, type="harmonic")]
P.pa <- dt[, index.pairs(p=price, r=region, n=product, w=weight, type="paasche")]
all.equal(P.ha, P.pa) # true

# (2) non-equal weights
dt[, "weight" := rweights(r=region, n=product, type=~n+r)]

P.je <- dt[, index.pairs(p=price, r=region, n=product, type="jevons")]
P.to <- dt[, index.pairs(p=price, r=region, n=product, w=weight, type="toernq")]
all.equal(P.je, P.to) # false

### GEKS method

# sample complete price data; equal weights:
dt <- rdata(R=7, N=15)
dt[, geks(p=price, r=region, n=product, type="jevons", base="1")]
dt[, jevons(p=price, r=region, n=product, base="1")]
# -> identical

# introduce gaps:
dt.gaps <- dt[!rgaps(region, product, amount=0.4), ]
dt.gaps[, geks(p=price, r=region, n=product, type="jevons", base="1")]
dt.gaps[, jevons(p=price, r=region, n=product, base="1")]
# -> now, differences
}
