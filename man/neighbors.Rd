\name{price_matrix}

\alias{neighbors}
\alias{connect}
\alias{comparisons}
\alias{is.connected}
\alias{sparsity}

\title{Price matrix characteristics}

\description{A price matrix or price tableau typically consists of prices for multiple products and regions.

Function \code{is.connected()} checks if all regions in the price matrix are connected either directly or indirectly by some bridging region. \code{neighbors()} divides the regions into groups of connected regions. \code{connect()} is a simple wrapper of \code{neighbors()}, connecting some price data by relying on the group with the maximum number of observations. \code{comparisons()} derives the amount of bilateral (or pairwise) comparisons that could be computed for each of those groups of regions. \code{sparsity()} indicates the sparsity of the price matrix.}

\usage{
is.connected(r, n)

neighbors(r, n, simplify = FALSE)

connect(r, n)

comparisons(r, n, ngbs = NULL)

sparsity(r, n, useable = FALSE)
}

\arguments{
   \item{r, n}{A character vector or factor of regional entities \eqn{r} and products \eqn{n}, respectively.}
   \item{simplify}{A logical indicating whether the results should be simplified to a factor of group identifiers (\code{TRUE}) or not (\code{FALSE}). In the latter case the output will be a list of connected regions.}
   \item{ngbs}{Either \code{NULL} or a list of connected regions derived from \code{neighbors()}. The latter case will allow for small performance gains in terms of processing times.}
   \item{useable}{A logical indicating whether only observations should be taken into account that could be used for interregional comparisons (\code{TRUE}) or not (\code{FALSE}).}
}

\author{Sebastian Weinand}

\value{Function \code{is.connected()} prints a single logical indicating the connectedness while \code{connect()} returns a logical vector of the same length as the input vectors. \code{neighbors()} gives a list or vector of connected regions. \code{sparsity()} returns a single numeric showing the sparsity of the price matrix. \code{comparisons()} outputs a data.table with the following variables:
\tabular{lll}{
   \code{group_id} \tab \tab group identifier\cr
   \code{group_members} \tab \tab regions belonging to that group\cr
   \code{group_size} \tab \tab number of regions belonging to that group\cr
   \code{total} \tab \tab number of (non-redundant) regional pairs that could be computed, following the formula \eqn{R*(R-1)/2}\cr
   \code{direct} \tab \tab number of regional pairs that traces back to direct connections, e.g. when two regions have priced the same product \cr
   \code{indirect} \tab \tab number of regional pairs that traces back to indirect connections, e.g. when two regions are connected via a third bridging region\cr
   \code{n_obs} \tab \tab number of observations containing interregional information\cr
}
}

\details{
Following World Bank (2013, p. 98), a "price tableau is said to be connected if the price data are such that it is not possible to place the countries in two groups in which no item priced by any country in one group is priced by any other country in the second group".
}

\references{
World Bank (2013). \emph{Measuring the Real Size of the World Economy: The Framework, Methodology, and Results of the International Comparison Program}. Washington, D.C.: World Bank.
}

\examples{
### connected price data:
data1 <- prices(R = 4, N = 3)

data1[, sparsity(r = region, n = product)]
data1[, is.connected(r = region, n = product)] # true
data1[, neighbors(r = region, n = product, simplify = TRUE)]
data1[, comparisons(r = region, n = product)]

### non-connected price data:
data2 <- data.table::data.table(
          "region" = c("a","a","h","b","a","a","c","c","d","e","e","f",NA),
          "product" = c(1,1,"bla",1,2,3,3,4,4,5,6,6,7),
          "price" = runif(13,5,6),
          stringsAsFactors = TRUE)

data2[, is.connected(r = region, n = product)] # false
with(data2, neighbors(r=region, n=product))
data2[, comparisons(region, product)]
# note that the region-product-combination [NA,7] is dropped
# from the output, while [a,2] and [e,5] are not included in
# the calculation of 'n_obs' as both are not useable in terms
# of regional price comparisons. also sparsity() takes this
# into account, if wanted:
data2[, sparsity(region, product, useable=TRUE)]
data2[, sparsity(region, product)]

# connect the price data:
data2[connect(r=region, n=product),]
}
