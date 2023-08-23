\name{gk}

\alias{gk}

\title{Geary-Khamis method}

\description{
Calculation of regional price levels using the Geary-Khamis method proposed by Geary (1958) and Khamis (1972).
}

\usage{
gk(p, q, r, n, base = NULL, simplify = TRUE, settings = list())
}

\arguments{
   \item{p, q}{A numeric vector of prices and quantities, respectively.}
   \item{r, n}{A character vector or factor of regional entities \eqn{r} and products \eqn{n}, respectively.}
   \item{base}{A character specifying the base region to which all price levels are expressed. When \code{NULL}, they refer to the (unweighted) regional average.}
   \item{simplify}{A logical indicating whether a named vector of estimated regional price levels (\code{TRUE}) should be returned, or also the average product prices.}
   \item{settings}{A list of control settings to be used. The following settings are supported:
   \itemize{
   \item \code{method} : the method used for solving the system of equations. Allowed values are \code{solve} for the (more efficient) solution proposed by Diewert (1999) or \code{iterative} for iterative searching until convergence. The default is \code{solve}.
   \item \code{tol} : the tolerance level when convergence is achieved if \code{type="iterative"}. The default is \code{1e-6}.
   \item \code{max.iter} : the maximum number of iterations if \code{type="iterative"}. The default is \code{99}.
   }}
}

\author{Sebastian Weinand}

\references{
Diewert, W. E. (1999). Axiomatic and Economic Approaches to International Comparisons. In: \emph{International and Interarea Comparisons of Income, Output and Prices}, edited by A. Heston and R. E Lipsey. Chicago: The University of Chicago Press.

Geary, R. C. (1958). A Note on the Comparison of Exchange Rates and Purchasing
Power Between Countries. \emph{Journal of the Royal Statistical Society. Series A
(General)}, 121 (1), 97–99.

Khamis, S. H. (1972). A New System of Index Numbers for National and International
Purposes. \emph{Journal of the Royal Statistical Society. Series A (General)},
135 (1), 96–121.
}

\value{For \code{simplify=TRUE}, a named vector of regional price levels. Otherwise, for \code{simplify=FALSE}, a named vector of average product prices and regional price levels.
}

\examples{
require(data.table)

set.seed(123)
R <- 3 # number of regions
N <- 4 # number of products
data <- prices(R = R, N = N)
data[, "quantity" := 1]

# complete data:
dt[, gk(p=price, q=quantity, t=time, n=item, settings=list(method="iterative"))]
dt[, gk(p=price, q=quantity, t=time, n=item, settings=list(method="solve"))]

# incomplete data:
dt[-c(1,16), gk(p=price, q=quantity, t=time, n=item, settings=list(method="iterative"))]
dt[-c(1,16), gk(p=price, q=quantity, t=time, n=item, settings=list(method="solve"))]
}
