\name{pricedata}

\alias{neighbors}
\alias{pairs}
\alias{connect}
\alias{properties}
\alias{is.connected}
\alias{gaps}

\title{Price data characteristics}

\description{
Price data typically consist of prices (and purchased quantities) for multiple products and regions.
Since not all products are usually available in all regions, the data exhibit gaps.
In some situations, the gaps can lead to non-connected data, which prevents a price comparison between all regions.

The following functions are available to derive the characteristics of a data set:
\itemize{
  \item \code{is.connected()} checks if all regions in the data are connected either directly or indirectly by some bridging region
  \item \code{neighbors()} divides the regions into groups of connected regions
  \item \code{connect()} is a simple wrapper of \code{neighbors()}, connecting the data using the group of regions with the maximum number of observations
  \item \code{gaps()} computes the (percentage) number of gaps in the data
  \item \code{pairs()} derives the number of available bilateral index pairs
  \item \code{properties()} derives data characteristics for each group of connected regions
}
}

\usage{
is.connected(r, n)

neighbors(r, n, simplify=FALSE)

connect(r, n)

gaps(r, n, relative=TRUE)

pairs(r, n)

properties(r, n)
}

\arguments{
   \item{r, n}{A character vector or factor of regional entities \code{r} and products \code{n}, respectively.}
   \item{simplify}{A logical indicating whether the results should be simplified to a vector of group identifiers (\code{TRUE}) or not (\code{FALSE}). In the latter case the output will be a list of connected regions.}
   \item{relative}{A logical indicating whether the absolute (\code{FALSE}) or relative (\code{TRUE}) number of data gaps should be computed.}
}

\author{Sebastian Weinand}

\value{
The function
\itemize{
  \item \code{is.connected()} prints a single logical indicating if the data is connected or not
  \item \code{connect()} returns a logical vector of the same length as \code{r} and \code{n}
  \item \code{neighbors()} gives a list or vector of connected regions
  \item \code{pairs()} returns a single numeric for the number of bilateral pairs
  \item \code{gaps()} returns a single numeric for the percentage of data gaps
}

The function \code{properties()} provides a data.table with the following variables:
\tabular{lllll}{
   \code{group} \tab \tab \emph{integer} \tab \tab group identifier\cr
   \code{size} \tab \tab \emph{integer} \tab \tab number of regions belonging to that group\cr
   \code{regions} \tab \tab \emph{list} \tab \tab regions belonging to that group\cr
   \code{pairs} \tab \tab \emph{integer} \tab \tab number of available non-redundant region pairs, e.g., \code{(AB,AC,BC)=3}\cr
   \code{nprods} \tab \tab \emph{integer} \tab \tab number of unique products\cr
   \code{nobs} \tab \tab \emph{integer} \tab \tab number of observations\cr
   \code{gaps} \tab \tab \emph{numeric} \tab \tab percentage of data gaps\cr
}
}

\details{
Before calculations start, missing values are removed from the data.
Duplicated observations for \code{r} and \code{n} are counted as one observation.
Products with prices in only one region \code{r} do not provide meaningful information for interregional comparisons.
Such products are therefore not considered by \code{gaps()}, \code{pairs()} and \code{properties()}.
This approach follows the default treatment of all index functions in this package.

Following World Bank (2013, p. 98), a "price tableau is said to be connected if the price data are such that it is not possible to place the countries in two groups in which no item priced by any country in one group is priced by any other country in the second group".
}

\references{
World Bank (2013). \emph{Measuring the Real Size of the World Economy: The Framework, Methodology, and Results of the International Comparison Program}. Washington, D.C.: World Bank.
}

\examples{
### connected price data:
set.seed(123)
dt1 <- rdata(R=4, B=1, N=3)

dt1[, is.connected(r=region, n=product)] # true
dt1[, neighbors(r=region, n=product, simplify=TRUE)]
dt1[, gaps(r=region, n=product)]
dt1[, pairs(r=region, n=product)]
dt1[, properties(r=region, n=product)]

### non-connected price data:
dt2 <- data.table::data.table(
          "region"=c("a","a","h","b","a","a","c","c","d","e","e","f",NA),
          "product"=c(1,1,"bla",1,2,3,3,4,4,5,6,6,7),
          "price"=runif(13,5,6),
          stringsAsFactors=TRUE)

dt2[, is.connected(r=region, n=product)] # false
with(dt2, neighbors(r=region, n=product))
dt2[, properties(region, product)]
# note that the first two observations are treated as one
# while the observation [NA,7] is dropped. Observation [a,2]
# is still included even though it does not provide valueable
# information for interregional comparisons (the product is
# observed in only one region)

# connect the price data:
dt2[connect(r=region, n=product),]
}
