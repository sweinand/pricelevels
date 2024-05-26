\name{ratios}

\alias{ratios}

\title{Calculation of price ratios}

\description{
Calculation of regional price ratios per product with flexible setting of base prices.
}

\usage{
ratios(p, r, n, base=NULL, static=FALSE, settings=list())
}

\arguments{
   \item{p}{A numeric vector of prices.}
   \item{r, n}{A character vector or factor of regional entities \code{r} and products \code{n}, respectively.}
   \item{base}{A character specifying the base region to be used for the calculation of price ratios, i.e., \eqn{p_n^r / p_n^{base}}. If \code{NULL}, price ratios are calculated with reference to the average price of a product, i.e., \eqn{p_n^r / \bar{p}_n}, with \eqn{\bar{p}_n = (1/R) \sum_{s=1}^{R} p_n^s}.}
   \item{static}{A logical indicating whether the \code{base} region is static (\code{TRUE}), i.e. always the same as \code{base}, or if another region than \code{base} is allowed to be used when prices for \code{base} are not available or missing (=\code{NA}). Only relevant if \code{base} is not \code{NULL}.}
   \item{settings}{A list of control settings to be used. The following settings are supported:
   \itemize{
   \item \code{chatty} : A logical specifying if warnings and info messages should be printed or not. The default is \code{getOption("pricelevels.chatty")}.
   }}
}

\author{Sebastian Weinand}

\value{A numeric vector of the same length as \code{p}. If \code{base} has been adjusted for some products, the attribute \code{attr("base")} is added to the output, providing the respective base region.}

\details{
If \code{base} is not available for a specific product, \eqn{n}, and \code{static=FALSE}, another base region is used instead. This is particularly important in cases of missing prices. Otherwise, for \code{static=TRUE}, computation is not possible and gives \code{NA}.

If there are duplicated observations, only one of these duplicates is used as the base price. For example, if two prices are available for product \eqn{n} in base region \eqn{r}, \code{ratios()} divides both prices by the first one.
}

\examples{
### (1) unique price observations; no missings

set.seed(123)
dt1 <- rdata(R=3, B=1, N=4)
levels(dt1$region) <- c("a","b","c")

# calculate price ratios by product:
dt1[, ratios(p=price, r=region, n=product, base="b")]


### (2) unique price observations; missings


# drop two observations:
dt2 <- dt1[-c(5,10), ]

# now, region 'a' is base for product 2:
(pr <- dt2[, ratios(p=price, r=region, n=product, base="b")])

# base region prices are stored in attributes:
attr(pr, "base")

# with static base, NAs are produced:
dt2[, ratios(p=price, r=region, n=product, base="b", static=TRUE)]


### (3) treatment of duplicates and missing prices (not NAs):

# insert duplicates and missings:
dt3 <- rbind(dt1[2,], dt1[-c(1,10),])
dt3[1, "price" := dt1[2,price]+abs(rnorm(1))]
anyDuplicated(dt3, by=c("region","product"))

# duplicated prices are divided by the first base price:
dt3[, ratios(p=price, r=region, n=product, base="b")]
}
