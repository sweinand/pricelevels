\name{qauv}

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
  \item Ikle-Dikhaov-Balk method \code{idb()} developed by Ikle (1972), Dikhanov (1994), and Balk (1996).
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
   \item \code{method} : the method used for solving the system of equations. The default for all indices is \code{iterative} for iterative solving until convergence. For \code{gk()}, the analyitical solution proposed by Diewert (1999) is also allowed.
   \item \code{tol} : the tolerance level when convergence is achieved if \code{type="iterative"}. The default is \code{1e-9}.
   \item \code{max.iter} : the maximum number of iterations if \code{type="iterative"}. The default is \code{99}.
   }}
}

\details{
Before calculations start, missing values are excluded and duplicated observations for \code{r} and \code{n} are aggregated, that is, duplicated prices \code{p} and weights \code{w} are averaged and duplicated quantities \code{q} added up.
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

\value{For \code{simplify=TRUE}, a named vector of regional price levels. Otherwise, for \code{simplify=FALSE}, a named vector of average product prices and regional price levels.
}

\examples{
require(data.table)

# example data:
set.seed(123)
dt <- rdata(R=3, B=1, N=4, gaps=0)
dt[, "share" := price*quantity/sum(price*quantity), by="region"]

# Gery-Khamis price index:
dt[, gk(p=price, q=quantity, r=region, n=product, settings=list(method="iterative"))]
dt[, gk(p=price, q=quantity, r=region, n=product, settings=list(method="solve"))]

# Geradi index with quantites or expenditure share weights:
dt[, geradi(p=price, q=quantity, r=region, n=product)]
dt[, geradi(p=price, w=share, r=region, n=product)]

# gk(), idb() and geradi() yield same results if quantites the same:
dt[, "quantity2" := 1000*rleidv(product)]
dt[, gk(p=price, r=region, n=product, q=quantity2)]
dt[, geradi(p=price, r=region, n=product, q=quantity2)]
dt[, idb(p=price, r=region, n=product, q=quantity2)]
}
