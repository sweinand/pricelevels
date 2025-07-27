\name{ratios}

\alias{ratios}

\title{Calculation of price ratios}

\description{
Calculation of regional price ratios per product with flexible setting of base prices.
}

\usage{
ratios(p, r, n, q=NULL, w=NULL, base=NULL, settings=list())
}

\arguments{
   \item{p}{A numeric vector of positive prices.}
   \item{r, n}{A character vector or factor of regional entities \code{r} and products \code{n}, respectively.}
   \item{q, w}{A numeric vector of non-negative quantities \code{q} or expenditure share weights \code{w}. If both \code{q} and \code{w} are provided, \code{q} will be used. This is only relevant for the averaging of duplicated prices (see Section 'Details').}
   \item{base}{A character specifying the base region to be used for the calculation of price ratios. If \code{NULL}, price ratios are calculated with reference to the regional average price of a product (see Section 'Details')}
   \item{settings}{A list of control settings to be used. The following settings are supported:
   \itemize{
   \item \code{chatty} : A logical specifying if warnings and info messages should be printed or not. The default is \code{getOption("pricelevels.chatty")}.
   \item \code{static} : A logical indicating whether the \code{base} region is static (\code{TRUE}), that is, always the same, or if another region than \code{base} is allowed to be used when prices for \code{base} are not available or \code{NA} for specific products. Only relevant if \code{base} is not \code{NULL}. The default is \code{TRUE}.
   }}
}

\author{Sebastian Weinand}

\value{
A numeric vector of the same length as \code{p}.
If \code{base} is not \code{NULL} and \code{static=FALSE}, the attribute \code{attr(x, "base")} is added to the output, providing the respective base region for each product.}

\details{
If there are \eqn{k=1,\ldots,K^{r}_{n}} duplicated prices for product \eqn{n} in region \eqn{r}, these are averaged using the quantities \code{q} (or similarly as a weighted arithmetic mean using the weights \code{w}) if provided:

\deqn{\bar{p}^{r}_n = \sum_{k=1}^{K^r_n} p^r_{nk} q^r_{nk} \Big/ \sum_{k=1}^{K^r_n} q^r_{nk}}

Price ratios are then derived for each product \eqn{n} by \eqn{p_n^r \Big/ \frac{1}{R} \sum_{s=1}^{R} \bar{p}_{n}^s} if \code{base=NULL} and by \eqn{p_n^r \big/ \bar{p}^{base}_n} otherwise.
}

\examples{
### (1) unique price observations

set.seed(123)
dt1 <- rdata(R=3, B=1, N=4)
levels(dt1$region) <- c("a","b","c")

# calculate price ratios by product:
dt1[, ratios(p=price, r=region, n=product, base="b")]


### (2) unique price observations and missing base region


# drop two observations:
dt2 <- dt1[-c(5,10), ]

# now, region 'a' is base for product 2:
dt2[, "pr" := ratios(p=price, r=region, n=product, base="b",
                     settings=list(static=FALSE))]

# base regions are stored in attributes:
attr(dt2$pr, "base")

# with static base, NAs are produced:
dt2[, "pr_static" := ratios(p=price, r=region, n=product, base="b")]


### (3) duplicated prices

# insert duplicates and missings:
dt3 <- rbind(dt1[c(2,3),], dt1[-c(11),])
dt3[1:2, c("price","quantity") := list(price*1.1, quantity*0.95)]
anyDuplicated(dt3, by=c("region","product"))

# duplicated prices are divided by the weighted average base prices:
dt3[, ratios(p=price, r=region, n=product, q=quantity, base="b",
             settings=list(static=FALSE))]
}
