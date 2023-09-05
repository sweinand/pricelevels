\name{ratios}

\alias{ratios}

\title{Calculation of price ratios}

\description{
Groupwise calculation of price ratios with flexible setting of base prices by product.
}

\usage{
ratios(p, r, n, base = NULL, static = FALSE, drop = FALSE)
}

\arguments{
   \item{p}{A numeric vector of prices.}
   \item{r, n}{A character vector or factor of regional entities \code{r} and products \code{n}, respectively.}
   \item{base}{A character specifying the base region to be used for the calculation of price ratios. If \code{NULL}, \code{base} is set internally.}
   \item{static}{A logical indicating whether the \code{base} region is static (\code{TRUE}), i.e. always the same as \code{base}, or if another region than \code{base} is allowed to be used when prices for \code{base} are not available.}
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
data1 <- prices(R = 3, N = 4)
levels(data1$region) <- c("a","b","c")

# calculate price ratios by product groups:
(pr <- data1[, ratios(p = price, r = region, n = product, base = "b")])

# add to data:
data1[, c("base", "ratio") := list(names(pr), as.vector(pr))]

### (2) unique price observations; missings

# drop two observations:
data2 <- data1[-c(5,10), ]
data2[, ratios(price, region, product, base = "b")]
# now, region 'a' is base for product 'n2'

### (3) treatment of duplicates and missing prices (not NAs):

# insert duplicates and missings:
data3 <- rbind(data1[2,], data1[-c(1,10),])
data3[1, "price" := data1[2,price]+abs(rnorm(1))]
anyDuplicated(data3, by=c("region","product"))

data3[, c("base","ratio") := data.table::as.data.table(
                              ratios(price, region, product),
                              keep.rownames = TRUE)]

# duplicated prices are divided by the base price and
# kept when base prices used are dropped:
data3[, ratios(price, region, product, drop = TRUE)]
}
