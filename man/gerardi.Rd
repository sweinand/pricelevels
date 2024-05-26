\name{gerardi}

\alias{gerardi}

\title{Gerardi index}

\description{
Calculation of regional price levels using the multilateral Gerardi index (Eurostat, 1978).
}

\usage{
gerardi(p, r, n, q, w=NULL, base=NULL, simplify=TRUE, settings=list())
}

\arguments{
   \item{p}{A numeric vector of prices.}
   \item{r, n}{A character vector or factor of regional entities \code{r} and products \code{n}, respectively.}
   \item{q, w}{A numeric vector of non-negative quantities \code{q} or expenditure share weights \code{w} (see details). If both \code{q} and \code{w} are provided, \code{q} will be used.}
   \item{base}{A character specifying the base region to which all price levels are expressed. When \code{NULL}, they refer to the (unweighted) regional average.}
   \item{simplify}{A logical indicating whether a named vector of estimated regional price levels (\code{TRUE}) should be returned, or also the average product prices.}
   \item{settings}{A list of control settings to be used. The following settings are supported:
   \itemize{
   \item \code{chatty} : A logical specifying if warnings and info messages should be printed or not. The default is \code{getOption("pricelevels.chatty")}.
   \item \code{connect} : A logical specifying if the data should be checked for connectedness or not. The default is \code{getOption("pricelevels.connect")}. If the data are not connected, price levels are computed within the biggest block of connected regions or the block of regions to which the \code{base} region belongs. See also \code{\link{connect}()}.
   \item \code{plot} : A logical specifying if the calculated price levels should be plotted or not. If \code{TRUE}, the price ratios of each region are displayed as boxplots and the price levels are added as colored points. The default is \code{getOption("pricelevels.plot")}.
   \item \code{variant} : for \code{original}, the international prices are calculated as unweighted geometric means. This is the original approach. With \code{adjusted}, the international prices are calculated as weighted geometric means.
   }}
}

\details{
Before calculations start, missing values are excluded and duplicated observations for \code{r} and \code{n} are aggregated, that is, duplicated prices \code{p} and weights \code{w} are averaged and duplicated quantities \code{q} added up.

The weights \code{w} must represent expenditure shares defined as \eqn{w_i^r = p_i^r q_i^r / \sum_{j=1}^{N} p_j^r q_j^r}. They are internally (re-)normalized such that they add up to 1 for each region \code{r}.
}

\author{Sebastian Weinand}

\references{
Balk, B. M. (1996). A comparison of ten methods for multilateral international
price and volume comparisons. \emph{Journal of Official Statistics}, 12 (1), 199-222.

Eurostat (1978), \emph{Comparison in real values of the aggregates of ESA 1975}, Publications Office, Luxembourg.
}

\value{For \code{simplify=TRUE}, a named vector of regional price levels. Otherwise, for \code{simplify=FALSE}, a list containing the named vector of international product prices and regional price levels.
}

\examples{
require(data.table)

# example data:
set.seed(123)
dt1 <- rdata(R=3, B=1, N=5)

# Gerardi price index:
dt1[, gerardi(p=price, q=quantity, r=region, n=product)]

# add price data:
dt2 <- rdata(R=4, B=1, N=4)
dt2[, "region":=factor(region, labels=4:7)]
dt2[, "product":=factor(product, labels=6:9)]
dt <- rbind(dt1, dt2)
dt[, is.connected(r=region, n=product)] # non-connected now

# compute expenditure share weights:
dt[, "share" := price*quantity/sum(price*quantity), by="region"]

# Gerardi index with quantites or expenditure share weights:
dt[, gerardi(p=price, q=quantity, r=region, n=product)]
dt[, gerardi(p=price, w=share, r=region, n=product)]
}
