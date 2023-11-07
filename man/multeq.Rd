\name{multeq}

\alias{gk}
\alias{geradi}
\alias{idb}
\alias{rao}

\title{Multilateral systems of equations}

\description{
Calculation of regional price levels using the
\itemize{
  \item Geary-Khamis method \code{gk()} proposed by Geary (1958) and Khamis (1972);
  \item Geradi method \code{geradi()} used by Eurostat (1978);
  \item Rao method \code{rao()} proposed by Rao (1990);
  \item Ikle-Dikhanov-Balk method \code{idb()} developed by Ikle (1972), Dikhanov (1997), and Balk (1996).
}
All methods have in common that they set up a system of interrelated equations of international product prices and price levels, which must be solved iteratively. It is only the definition of the international product prices and price levels that differ between the methods (see package vignette).
}

\usage{
# geary-khamis index requires quantities:
gk(p, r, n, q, base = NULL, simplify = TRUE, settings = list())

# ikle-dikhanov-balk, rao, and geradi indices:
idb(p, r, n, q, w=NULL, base=NULL, simplify=TRUE, settings=list())
rao(...)
geradi(...)
}

\arguments{
   \item{p}{A numeric vector of prices.}
   \item{r, n}{A character vector or factor of regional entities \code{r} and products \code{n}, respectively.}
   \item{q, w}{A numeric vector of non-negative quantities \code{q} or expenditure share weights \code{w}, respectively. If both \code{q} and \code{w} are provided, \code{q} will be used. Note that \code{gk()} requires quantities \code{q}.}
   \item{base}{A character specifying the base region to which all price levels are expressed. When \code{NULL}, they refer to the (unweighted) regional average.}
   \item{simplify}{A logical indicating whether a named vector of estimated regional price levels (\code{TRUE}) should be returned, or also the average product prices.}
   \item{settings}{A list of control settings to be used. The following settings are supported:
   \itemize{
   \item \code{chatty} : A logical specifying if warnings and info messages should be printed or not. The default is \code{TRUE}.
   \item \code{connect} : A logical specifying if the data should be checked for connectedness or not. The default is \code{TRUE}. If the data are not connected, price levels are computed within the biggest block of connected regions or the block of regions to which the \code{base} region belongs. See also \code{\link{connect}()}.
   \item \code{solve} : the method used for solving the system of equations. The default for all indices is \code{iterative} for iterative solving until convergence. For \code{gk()}, the analytical solution proposed by Diewert (1999) is also allowed by setting to \code{matrix}.
   \item \code{tol} : the tolerance level when convergence is achieved if \code{type="iterative"}. The default is \code{1e-9}.
   \item \code{max.iter} : the maximum number of iterations if \code{type="iterative"}. The default is \code{99}.
   }}
}

\details{
Before calculations start, missing values are excluded and duplicated observations for \code{r} and \code{n} are aggregated, that is, duplicated prices \code{p} and weights \code{w} are averaged and duplicated quantities \code{q} added up.

The weights \code{w} are (re-)normalized internally such that they add up to 1 for each region \code{r}.
}

\author{Sebastian Weinand}

\references{
Balk, B. M. (1996). A comparison of ten methods for multilateral international
price and volume comparisons. \emph{Journal of Official Statistics}, 12 (1), 199-222.

Diewert, W. E. (1999). Axiomatic and Economic Approaches to International Comparisons. In: \emph{International and Interarea Comparisons of Income, Output and Prices}, edited by A. Heston and R. E Lipsey. Chicago: The University of Chicago Press.

Dikhanov, Y. (1994). Sensitivity of PPP-based income estimates to the choice of aggregation procedures. The World Bank, Washington D.C., June 10, paper presented at 23rd General Conference of the International Association for Research in Income and Wealth, St. Andrews, Canada.

Eurostat (1978), \emph{Comparison in real values of the aggregates of ESA 1975}, Publications Office, Luxembourg.

Geary, R. C. (1958). A Note on the Comparison of Exchange Rates and Purchasing
Power Between Countries. \emph{Journal of the Royal Statistical Society. Series A
(General)}, 121 (1), 97–99.

Ikle, D. M. (1972). A new approach to the index number problem. \emph{The Quarterly Journal of Economics}, 86 (2), 188-211.

Khamis, S. H. (1972). A New System of Index Numbers for National and International
Purposes. \emph{Journal of the Royal Statistical Society. Series A (General)},
135 (1), 96–121.

Rao, D. S. P. (1990). A system of log-change index numbers for multilateral comparisons. In: \emph{Comparisons of prices and real products in Latin America. Contributions to Economic Analysis Series}, edited by Salazar-Carrillo and Rao. Amsterdam: North-Holland Publishing Company.
}

\value{For \code{simplify=TRUE}, a named vector of regional price levels. Otherwise, for \code{simplify=FALSE}, a list containing the named vector of international product prices and regional price levels, the number of iterations until convergence, and the achieved difference at convergence.
}

\examples{
require(data.table)

# example data:
set.seed(123)
dt1 <- rdata(R=3, B=1, N=5)

# Gery-Khamis price index can be obtained in two ways:
dt1[, gk(p=price, q=quantity, r=region, n=product, settings=list(solve="iterative"))]
dt1[, gk(p=price, q=quantity, r=region, n=product, settings=list(solve="matrix"))]

# gk(), idb() and geradi() yield same results if quantites the same:
dt1[, "quantity2" := 1000*rleidv(product)]
dt1[, gk(p=price, r=region, n=product, q=quantity2)]
dt1[, geradi(p=price, r=region, n=product, q=quantity2)]
dt1[, idb(p=price, r=region, n=product, q=quantity2)]
dt1[, "quantity2":=NULL]

# add price data:
dt2 <- rdata(R=4, B=1, N=4)
dt2[, "region":=factor(region, labels=4:7)]
dt2[, "product":=factor(product, labels=6:9)]
dt <- rbind(dt1, dt2)
dt[, is.connected(r=region, n=product)] # non-connected now

# compute expenditure share weights:
dt[, "share" := price*quantity/sum(price*quantity), by="region"]

# Geradi index with quantites or expenditure share weights:
dt[, geradi(p=price, q=quantity, r=region, n=product)]
dt[, geradi(p=price, w=share, r=region, n=product)]
}
