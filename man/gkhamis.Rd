\name{gkhamis}

\alias{mcarli}
\alias{mjevons}
\alias{mharmonic}
\alias{mdutot}
\alias{gkhamis}
\alias{ikle}
\alias{rao}
\alias{rhajargasht}

\title{Multilateral systems of equations}

\description{
Calculation of regional price levels using the
\itemize{
  \item Geary-Khamis method (Geary, 1958; Khamis, 1972): \code{gkhamis()}
  \item Iklé method (Ikle, 1972; Dikhanov, 1997; Balk, 1996): \code{ikle()}
  \item Rao system (Rao, 1990): \code{rao()}
  \item Rao-Hajargasht method (Rao and Hajargasht, 2016): \code{rhajargasht()}
  \item Multilateral variants of Carli, Dutot, Jevons, and Harmonic mean (Rao and Hajargasht, 2016): \code{mcarli(), mdutot(), mjevons(), mharmonic()}
}
All methods have in common that they set up a system of interrelated equations of international product prices and price levels, which must be solved iteratively. It is only the definition of the international product prices and price levels that differ between the methods (see package vignette).
}

\usage{
gkhamis(p, r, n, q, base=NULL, simplify=TRUE, settings=list())

ikle(p, r, n, q, w=NULL, base=NULL, simplify=TRUE, settings=list())

mcarli(p, r, n, base=NULL, simplify=TRUE, settings=list())

mdutot(p, r, n, base=NULL, simplify=TRUE, settings=list())

mharmonic(p, r, n, base=NULL, simplify=TRUE, settings=list())

mjevons(p, r, n, base=NULL, simplify=TRUE, settings=list())

rao(p, r, n, q, w=NULL, base=NULL, simplify=TRUE, settings=list())

rhajargasht(p, r, n, q, w=NULL, base=NULL, simplify=TRUE, settings=list())
}

\arguments{
   \item{p}{A numeric vector of prices.}
   \item{r, n}{A character vector or factor of regional entities \code{r} and products \code{n}, respectively.}
   \item{q, w}{A numeric vector of non-negative quantities \code{q} or expenditure share weights \code{w} (see details). If both \code{q} and \code{w} are provided, \code{q} will be used. Note that \code{gkhamis()} does not use weights \code{w}.}
   \item{base}{A character specifying the base region to which all price levels are expressed. When \code{NULL}, they refer to the (unweighted) regional average.}
   \item{simplify}{A logical indicating whether a named vector of estimated regional price levels (\code{TRUE}) should be returned, or also the average product prices.}
   \item{settings}{A list of control settings to be used. The following settings are supported:
   \itemize{
   \item \code{chatty} : A logical specifying if warnings and info messages should be printed or not. The default is \code{TRUE}.
   \item \code{connect} : A logical specifying if the data should be checked for connectedness or not. The default is \code{TRUE}. If the data are not connected, price levels are computed within the biggest block of connected regions or the block of regions to which the \code{base} region belongs. See also \code{\link{connect}()}.
   \item \code{solve} : the method used for solving the system of equations. The default for all indices is \code{iterative} for iterative solving until convergence. For \code{gkhamis()}, the analytical solution proposed by Diewert (1999) is also allowed by setting to \code{matrix}.
   \item \code{tol} : the tolerance level when convergence is achieved if \code{type="iterative"}. The default is \code{1e-9}.
   \item \code{max.iter} : the maximum number of iterations if \code{type="iterative"}. The default is \code{99}.
   }}
}

\details{
In their original form, the index methods use quantities and/or weights. However, Rao and Hajargasht (2016, p. 417) have shown that similar solutions exist for the unweighted definitions of international product prices and price levels. This is implemented in functions \code{mcarli(), mdutot(), mjevons(), mharmonic()}, which do not require quantities or weights.

Before calculations start, missing values are excluded and duplicated observations for \code{r} and \code{n} are aggregated, that is, duplicated prices \code{p} and weights \code{w} are averaged and duplicated quantities \code{q} added up.

The weights \code{w} must represent expenditure shares defined as \eqn{w_i^r = p_i^r q_i^r / \sum_{j=1}^{N} p_j^r q_j^r}. They are internally (re-)normalized such that they add up to 1 for each region \code{r}.
}

\author{Sebastian Weinand}

\references{
Balk, B. M. (1996). A comparison of ten methods for multilateral international
price and volume comparisons. \emph{Journal of Official Statistics}, 12 (1), 199-222.

Diewert, W. E. (1999). Axiomatic and Economic Approaches to International Comparisons. In: \emph{International and Interarea Comparisons of Income, Output and Prices}, edited by A. Heston and R. E Lipsey. Chicago: The University of Chicago Press.

Dikhanov, Y. (1994). Sensitivity of PPP-based income estimates to the choice of aggregation procedures. The World Bank, Washington D.C., June 10, paper presented at 23rd General Conference of the International Association for Research in Income and Wealth, St. Andrews, Canada.

Geary, R. C. (1958). A Note on the Comparison of Exchange Rates and Purchasing
Power Between Countries. \emph{Journal of the Royal Statistical Society. Series A
(General)}, 121 (1), 97–99.

Ikle, D. M. (1972). A new approach to the index number problem. \emph{The Quarterly Journal of Economics}, 86 (2), 188-211.

Khamis, S. H. (1972). A New System of Index Numbers for National and International
Purposes. \emph{Journal of the Royal Statistical Society. Series A (General)},
135 (1), 96–121.

Rao, D. S. P. (1990). A system of log-change index numbers for multilateral comparisons. In: \emph{Comparisons of prices and real products in Latin America. Contributions to Economic Analysis Series}, edited by Salazar-Carrillo and Rao. Amsterdam: North-Holland Publishing Company.

Rao, D. S. P. and G. Hajargasht (2016). Stochastic approach to computation of purchasing power parities in the International Comparison Program. \emph{Journal of Econometrics}, 191 (2016), 414-425.
}

\value{For \code{simplify=TRUE}, a named vector of regional price levels. Otherwise, for \code{simplify=FALSE}, a list containing the named vector of international product prices and regional price levels, the number of iterations until convergence, and the achieved difference at convergence.
}

\examples{
require(data.table)

# example data:
set.seed(123)
dt1 <- rdata(R=3, B=1, N=5)

# Gery-Khamis price index can be obtained in two ways:
dt1[, gkhamis(p=price, q=quantity, r=region, n=product, settings=list(solve="iterative"))]
dt1[, gkhamis(p=price, q=quantity, r=region, n=product, settings=list(solve="matrix"))]

# gkhamis(), ikle() and gerardi() yield same results if quantites the same:
dt1[, "quantity2" := 1000*rleidv(product)]
dt1[, gkhamis(p=price, r=region, n=product, q=quantity2)]
dt1[, gerardi(p=price, r=region, n=product, q=quantity2)]
dt1[, ikle(p=price, r=region, n=product, q=quantity2)]
dt1[, "quantity2":=NULL]

# add price data:
dt2 <- rdata(R=4, B=1, N=4)
dt2[, "region":=factor(region, labels=4:7)]
dt2[, "product":=factor(product, labels=6:9)]
dt <- rbind(dt1, dt2)
dt[, is.connected(r=region, n=product)] # non-connected now

# compute expenditure share weights:
dt[, "share" := price*quantity/sum(price*quantity), by="region"]

# Ikle index with quantites or expenditure share weights:
dt[, ikle(p=price, q=quantity, r=region, n=product)]
dt[, ikle(p=price, w=share, r=region, n=product)]
}
