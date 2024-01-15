\name{ratios}

\alias{ratios}

\title{Calculation of price ratios}

\description{
Groupwise calculation of price ratios with flexible setting of base prices by product.
}

\usage{
ratios(p, r, n, base=NULL, static=FALSE, drop=FALSE)
}

\arguments{
   \item{p}{A numeric vector of prices.}
   \item{r, n}{A character vector or factor of regional entities \code{r} and products \code{n}, respectively.}
   \item{base}{A character specifying the base region to be used for the calculation of price ratios. If \code{NULL}, \code{base} is set internally.}
   \item{static}{A logical indicating whether the \code{base} region is static (\code{TRUE}), i.e. always the same as \code{base}, or if another region than \code{base} is allowed to be used when prices for \code{base} are not available or missing (=\code{NA}).}
   \item{drop}{A logical indicating whether observations of \code{base} should be dropped (\code{TRUE}) from the output or not.}
}

\author{Sebastian Weinand}

\value{A named vector. The names indicate the base region chosen.}

\details{
If \code{base} is not available for a specific product, \eqn{n}, and \code{static=FALSE}, another base region is used instead. This is particularly important in cases of missing prices. Othwerise, for \code{static=TRUE}, computation is not possible and gives \code{NA}.

If there are duplicated observations, only one of these duplicates is used as the base price. For example, if two prices are available for product \eqn{n} in base region \eqn{r}, \code{ratios()} divides both prices by the first one.
}

\examples{
### (1) unique price observations; no missings

set.seed(123)
dt1 <- rdata(R=3, B=1, N=4)
levels(dt1$region) <- c("a","b","c")

# calculate price ratios by product groups:
(pr <- dt1[, ratios(p = price, r = region, n = product, base = "b")])

# add to data:
dt1[, c("base", "ratio") := list(names(pr), as.vector(pr))]

### (2) unique price observations; missings

# drop two observations:
dt2 <- dt1[-c(5,10), ]
dt2[, ratios(price, region, product, base = "b")]
# now, region 'a' is base for product 'n2'

### (3) treatment of duplicates and missing prices (not NAs):

# insert duplicates and missings:
dt3 <- rbind(dt1[2,], dt1[-c(1,10),])
dt3[1, "price" := dt1[2,price]+abs(rnorm(1))]
anyDuplicated(dt3, by=c("region","product"))

dt3[, c("base","ratio") := data.table::as.data.table(
                              ratios(price, region, product),
                              keep.rownames = TRUE)]

# duplicated prices are divided by the base price and
# kept when base prices used are dropped:
dt3[, ratios(price, region, product, drop = TRUE)]
}
