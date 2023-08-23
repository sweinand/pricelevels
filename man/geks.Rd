\name{geks}

\alias{geks}

\title{GEKS method}

\description{Calculation of regional price levels using the GEKS method proposed by Gini (1924, 1931), Elteto and Koves (1964), and Szulc (1964).}

\usage{
geks(p, r, n, w = NULL, type = "jevons", base = NULL)
}

\arguments{
   \item{p}{A numeric vector of prices.}
   \item{r, n}{A character vector or factor of regional entities \eqn{r} and products \eqn{n}, respectively.}
   \item{w}{A numeric vector of weights used in the computation of weighted bilateral index numbers. Can be \code{NULL}, if the index formula specified in \code{type} does not require weights.}
   \item{type}{A character specifying the index number method to be used. See \code{\link{index.pairs}} for allowed values.}
   \item{base}{A character specifying the base region to which all price levels are expressed. When \code{NULL}, they refer to the (unweighted) regional average.}
}

\author{Sebastian Weinand}

\references{
Gini, C. (1924). Quelques Considerations au Sujet de la Construction des Nombres Indices des Prix et des Questions Analogues. \emph{Mentron}, 4 (1), 3-162.

Gini, C. (1931). On the Circular Test of Index Numbers. \emph{International Statistical Review}, 9 (2), 3-25.

Elteto, O. and Koves, P. (1964). On a Problem of Index Number Computation Relating to International Comparison. \emph{Statisztikai Szemle}, 42, 507-518.

Szulc, B. J. (1964). Indices for Multiregional Comparisons. \emph{Przeglad Statystyczny}, 3, 239-254.
}

\value{A named vector of price levels.}

\details{
The GEKS index is a two-step approach. First, prices are aggregated into bilateral index numbers using the index given in \code{type}. This is done for all pairs of regions via function \code{\link{index.pairs}}. Second, these bilateral index numbers are transformed into a set of multilateral, transitive index numbers.

Note that the weights \code{w} are used within the aggregation of prices into index numbers (first stage) while the subsequent transformation of these index numbers (second stage) does not rely on any weights.
}

\seealso{\code{\link{index.pairs}}}

\examples{
# sample complete price data; equal weights:
data <- prices(R = 7, N = 20)
data[, geks(p = price, r = region, n = product, type = "jevons", base = "r1")]
data[, jevons(p = price, r = region, n = product, base = "r1")]
# -> identical

# introduce gaps:
data_gaps <- data[!gaps(region, product, amount=0.4), ]
data_gaps[, geks(p = price, r = region, n = product, type = "jevons", base = "r1")]
data_gaps[, jevons(p = price, r = region, n = product, base = "r1")]
# -> now, differences
}
