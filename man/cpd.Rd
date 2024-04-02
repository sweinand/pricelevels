\name{cpd}

\alias{cpd}
\alias{nlcpd}

\title{CPD and NLCPD methods}

\description{
Function \code{cpd()} estimates regional price levels by the Country-Product-Dummy (CPD) method, originally developed by Summers (1973). Auer and Weinand (2022) recently proposed a generalization of the CPD method. This nonlinear CPD method (NLCPD method) is implemented in function \code{nlcpd()}.
}

\usage{
cpd(p, r, n, q=NULL, w=NULL, base=NULL, simplify=TRUE, settings=list())

nlcpd(p, r, n, q=NULL, w=NULL, base=NULL, simplify=TRUE, settings=list(), ...)
}

\arguments{
   \item{p}{A numeric vector of prices.}
   \item{r, n}{A character vector or factor of regional entities \code{r} and products \code{n}, respectively.}
   \item{q, w}{A numeric vector of non-negative quantities \code{q} or weights \code{w}. By default, no weights are used in the regression (\code{q=NULL} and \code{w=NULL}). While \code{w} can be any weights considered as appropriate for weighted regression, \code{q} will result in an expenditure share weighted regression (see details). If both \code{q} and \code{w} are provided, \code{q} will be used.}
   \item{base}{A character specifying the base to which the estimated logarithmic regional price levels are expressed. When \code{NULL}, they refer to the (unweighted) regional average, similar to \code{\link{contr.sum}}.}
   \item{simplify}{A logical indicating whether the full regression-object should be provided (\code{FALSE}) or a named vector of estimated regional price levels (\code{TRUE}).}
   \item{settings}{A list of control settings to be used. The following settings are supported:
   \itemize{
   \item \code{chatty} : A logical specifying if warnings and info messages should be printed or not. The default is \code{TRUE}.
   \item \code{connect} : A logical specifying if the data should be checked for connectedness or not. The default is \code{TRUE}. If the data are not connected, price levels are computed within the biggest block of connected regions or the block of regions to which the \code{base} region belongs. See also \code{\link{connect}()}.
   \item \code{self.start} : Only if \code{par=NULL}, the strategy how parameter start values are internally derived by \code{nlcpd()}. Currently, values \code{s1}, \code{s2} and \code{s3} are allowed. For \code{s1}, simple price averages across products and regions are used as start values, while these are derived by the CPD method for strategies \code{s2} and \code{s3}. Start values for \code{delta} are either set to 1 or derived by their first-order condition if \code{s3}. By default, \code{self.start='s1'}.
   %\item \code{use.bounds} : A logical indicating if lower and upper bounds should be used for the parameters \eqn{\ln P^r} and \eqn{\ln \pi_i}. If \code{TRUE}, these are derived from the values in \code{par.start}. If \code{TRUE} and \code{upper} and/or \code{lower} is given as well (see below), the latter are used.
   \item \code{use.jac} : A logical indicating if the jacobian matrix should be used by \code{nlcpd()} for the nonlinear optimization or not. The default is \code{FALSE}.
   \item \code{w.delta} : A named vector of weights for the \code{delta}-parameter (see Details). Vector length must be equal to the number of products, while names must match product names. If not supplied, \eqn{\delta_i} weights are derived internally by \code{nlcpd()} from the weights \code{w}.
   }}
   \item{...}{Further arguments passed to \code{\link{nls.lm}}, typically arguments \code{control}, \code{par}, \code{upper}, and \code{lower}. For \code{par}, \code{upper}, and \code{lower}, vectors must have names for each parameter separated by a dot, e.g., \code{lnP.1}, \code{pi.2}, or \code{delta.3}.}
}

\details{
The CPD method is a linear regression model that explains the logarithmic price of product \eqn{i} in region \eqn{r}, \eqn{\ln p_i^r}, by the general product price, \eqn{\ln \pi_i}, and the overall price level, \eqn{\ln P^r}:

\deqn{\ln p_i^r = \ln \pi_i + \ln P^r + u_i^r}

The NLCPD method inflates the CPD model by product-specific elasticities \eqn{\delta_i}:

\deqn{\ln p_i^r = \ln \pi_i + \delta_i \ln P^r + u_i^r}

Note that both the CPD and the NLCPD method require a normalization of the estimated price levels \eqn{\widehat{\ln P^r}} to avoid multicollinearity. If \code{base=NULL}, normalization \eqn{\sum_{r=1}^{R} \widehat{\ln P^r}=0} is used in both functions; otherwise, one price level is set to 0. The NLCPD method additionally imposes the restriction \eqn{\sum_{i=1}^{N} w_i \widehat{\delta_i}=1}, where the weights \eqn{w_i} can be defined by \code{settings$w.delta}. In \code{nlcpd()}, it is always the parameter \eqn{\widehat{\delta_1}} that is derived residually from this restriction.

Before calculations start, missing values are excluded and duplicated observations for \code{r} and \code{n} are aggregated, that is, duplicated prices \code{p} and weights \code{w} are averaged and duplicated quantities \code{q} added up.

If \code{q} is provided, expenditure shares are derived as \eqn{w_i^r = p_i^r q_i^r / \sum_{j=1}^{N} p_j^r q_j^r} and used as weights in the regression. If only \code{w} is provided, the weights \code{w} are used in the regression without any further adjustment. This is different to the behavior of the other price index functions in the \code{pricelevels}-package where the weights \code{w} are (re-)normalized for each region \code{r}.
}

\author{Sebastian Weinand}

\references{
Auer, L. v. and Weinand, S. (2022). \emph{A Nonlinear Generalization of the Country-Product-
Dummy Method.} Discussion Paper 2022/45, Deutsche Bundesbank.

Summers, R. (1973). International Price Comparisons based upon Incomplete Data. \emph{Review of Income and Wealth}, 19 (1), 1-16.
}

\value{For \code{simplify=TRUE}, a named vector of (unlogged) regional price levels. Otherwise, for \code{cpd()}, a \code{lm}-object containing the full regression output, and for \code{nlcpd()} the full output of \code{nls.lm()} plus element \code{w.delta}.
}

\seealso{\code{\link{lm}}, \code{\link{dummy.coef}}, \code{\link{nls.lm}}}

\examples{
# sample complete price data:
set.seed(123)
R <- 3 # number of regions
B <- 1 # number of product groups
N <- 5 # number of products
dt1 <- rdata(R=R, B=B, N=N)

# compute expenditure share weighted cpd and nlcpd index:
dt1[, cpd(p=price, r=region, n=product, q=quantity)]
dt1[, nlcpd(p=price, r=region, n=product, q=quantity)]

# set individual start values in nlcpd():
par.init <- list("lnP"=setNames(rep(0, R), 1:R),
                 "pi"=setNames(rep(2, N), 1:N),
                 "delta"=setNames(rep(1, N), 1:N))
dt1[, nlcpd(p=price, r=region, n=product, q=quantity, par=par.init)]

# use lower and upper bounds on parameters:
dt1[, nlcpd(p=price, r=region, n=product, q=quantity,
            lower=unlist(par.init)-0.1, upper=unlist(par.init)+0.1)]

# change internal calculation of start values:
dt1[, nlcpd(p=price, r=region, n=product, q=quantity, settings=list(self.start="s2"))]

# add price data:
dt2 <- rdata(R=4, B=1, N=4)
dt2[, "region":=factor(region, labels=4:7)]
dt2[, "product":=factor(product, labels=6:9)]
dt <- rbind(dt1, dt2)
dt[, is.connected(r=region, n=product)] # non-connected now

# compute expenditure share weighted cpd and nlcpd index:
dt[, cpd(p=price, r=region, n=product, q=quantity, base="1")]
dt[, nlcpd(p=price, r=region, n=product, q=quantity, base="1")]

# compare with toernqvist index:
dt[, toernqvist(p=price, r=region, n=product, q=quantity, base="1")]

\donttest{
# computational speed in nlcpd() usually increases if use.jac=TRUE:
set.seed(123)
dt3 <- rdata(R=20, B=1, N=30)
system.time(m1 <- dt3[, nlcpd(p=price, r=region, n=product, q=quantity,
                              settings=list(use.jac=FALSE), simplify=FALSE,
                              control=minpack.lm::nls.lm.control("maxiter"=200))])
system.time(m2 <- dt3[, nlcpd(p=price, r=region, n=product, q=quantity,
                              settings=list(use.jac=TRUE), simplify=FALSE,
                              control=minpack.lm::nls.lm.control("maxiter"=200))])
all.equal(m1$par, m2$par, tol=1e-05)
}
}
