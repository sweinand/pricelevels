\name{stochastic_index}

\alias{cpd}
\alias{nlcpd}

\title{CPD and NLCPD methods}

\description{
Function \code{cpd()} estimates regional price levels by the Country-Product-Dummy (CPD) method, originally developed by Summers (1973). Auer and Weinand (2022) recently proposed a generalization of the CPD method. This nonlinear CPD method (NLCPD method) is implemented in \code{nlcpd()}.

The CPD method is a linear regression model that explains the logarithmic price of product \eqn{i} in region \eqn{r}, \eqn{\ln p_i^r}, by the general product price, \eqn{\ln \pi_i}, and the overall price level, \eqn{\ln P^r}:

\deqn{\ln p_i^r = \ln \pi_i + \ln P^r + u_i^r}

The NLCPD method inflates the CPD model by product-specific elasticities \eqn{\delta_i}:

\deqn{\ln p_i^r = \ln \pi_i + \delta_i \ln P^r + u_i^r}

Note that both the CPD and the NLCPD method require a normalization of the estimated price levels \eqn{\widehat{\ln P^r}} to avoid multicollinearity. If \code{base=NULL}, normalization \eqn{\sum_{r=1}^{R} \widehat{\ln P^r}=0} is used in both functions; otherwise, one price level is set to 0. The NLCPD method additionaly imposes the restriction \eqn{\sum_{i=1}^{N} w_i \widehat{\delta_i}=1}, which is satisfied by the CPD method by definiton.
}

\usage{
cpd(x, r, n, w = NULL, base = NULL, simplify = TRUE)

nlcpd(x, r, n, w = NULL, base = NULL, simplify = TRUE, settings = list(), ...)
}

\arguments{
   \item{x}{A numeric vector of prices.}
   \item{r, n}{A character vector or factor of regional entities \eqn{r} and products \eqn{n}, respectively.}
   \item{w}{A numeric vector of weights \eqn{w} to be applied. By default (\code{w=NULL}), no weights are used in the regression.}
   \item{base}{A character specifying the base to which the estimated logarithmic regional price levels are expressed. When \code{NULL}, they refer to the (unweighted) regional average, similar to \code{\link{contr.sum}}.}
   \item{simplify}{A logical indicating whether the full regression-object should be provided (\code{FALSE}) or a named vector of estimated regional price levels (\code{TRUE}).}
   \item{settings}{A list of control settings to be used. The following settings are supported:
   \itemize{
   \item \code{par.start} : A named list of start values for the NLCPD coefficients: \eqn{\ln P^r} (\eqn{R-1} region price levels), \eqn{\ln \pi_i} (\eqn{N} general product prices), and \eqn{\delta_i} (\eqn{N-1} product-specific elasticities). Must have names \code{lnP}, \code{pi}, and \code{delta}. By default, \code{par.start=NULL}, meaning that start values are internally derived by \code{self.start}.
   \item \code{self.start} : Only if \code{par.start=NULL}, the strategy how parameter start values are internally derived. Currently, values \code{s1}, \code{s2} and \code{s3} are allowed. For \code{s1}, simple price averages across products and regions are used as start values, while these are derived by the CPD method for strategies \code{s2} and \code{s3}. Start values for \code{delta} are either set to 1 or derived by their first-order condition if \code{s3}. By default, \code{self.start='s1'}.
   \item \code{use.jac} : A logical indicating if the jacobian should be used or not.
   \item \code{w.delta} : A named vector of weights for the \code{delta}-parameter. Vector length must be equal to the number of products, while names must match product names. If not supplied, \eqn{\delta_i} weights are derived internally from weights \code{w}.
   }}
   \item{...}{Further arguments passed to \code{\link{nls.lm}}, typically arguments \code{control}, \code{upper} and \code{lower}.}
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
### (1) connected price data:
set.seed(123)
R <- 3 # number of regions
N <- 4 # number of products
data1 <- prices(R = R, N = N, weights = ~n)

data1[, cpd(price, region, product, base = "r1")] # switch to specific base region
data1[, cpd(price, region, product)]
data1[, nlcpd(price, region, product)]

# set individual start values in nlcpd():
par.init <- list("lnP"=rep(0, R-1), "pi"=rep(2, N), "delta"=rep(1, N-1))
data1[, nlcpd(price, region, product, settings=list(par.start=par.init))]
# -> no impact on solution

# change internal calculation of start values:
data1[, nlcpd(price, region, product, settings=list(self.start="s2"))]

### (2) working with non-connected price data:
data2 <- data.table::data.table(
   "region" = c("a","a","h","b","a","a","c","c","d","e","e","f",NA),
   "product" = c(1,1,"bla",1,2,3,3,4,4,5,6,6,7),
   "price" = runif(13,5,6))

is.connected(data2$region, data2$product) # false
data2$groups <- neighbors(data2$region, data2$product, TRUE)
lapply(X = split(data2, data2$groups),
       FUN = function(z) cpd(z$price, z$region, z$product))

data2[, "group_id" := neighbors(region, product, TRUE)]
data2[!is.na(group_id),
      data.table::as.data.table(cpd(price, region, product), keep.rownames = TRUE),
      by = "group_id"]
data2[!is.na(group_id),
      .("output" = list(cpd(price, region, product, simplify = FALSE))),
      by = "group_id"]

### (3) computational speed in nlcpd() usually increases if use.jac=TRUE
set.seed(123)
R <- 20 # number of regions
N <- 30 # number of products
data3 <- prices(R = R, N = N)
system.time(m1 <- data3[, nlcpd(x=price, r=region, n=product,
                                settings=list(use.jac=FALSE), simplify=FALSE,
                                control=minpack.lm::nls.lm.control("maxiter"=200))])
system.time(m2 <- data3[, nlcpd(x=price, r=region, n=product,
                                settings=list(use.jac=TRUE), simplify=FALSE,
                                control=minpack.lm::nls.lm.control("maxiter"=200))])
all.equal(m1$par, m2$par, tol=1e-05)
}
