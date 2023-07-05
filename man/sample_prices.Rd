\name{sample_prices}

\alias{prices}
\alias{gaps}
\alias{weights}

\title{Sampling of price data}

\description{Sample price data and introduce random amounts of gaps into these price data while ensuring interregional connectivity. Price data can include sampled product weights if wanted. The underlying data generating process relies on the NLCPD model (see \code{\link{nlcpd}}).}

\usage{
gaps(r, n, amount = 0, prob = NULL, pairs = FALSE, exclude = NULL)

weights(r, n, type = ~1)

prices(R, N, weights = NULL, gaps = 0, settings = list())
}

\arguments{
   \item{r, n}{A character vector or factor of regional entities \eqn{r} and products \eqn{n}, respectively.}
   \item{R, N}{A single integer specifying the number of regions and products, respectively.}
   \item{weights, type}{Either \code{NULL} (only in \code{prices()}) or a formula specifiyng the sampling of weights. If \code{weights=~1}, products receive identical weights, while weights are product-specific for \code{weights=~n}. If weights should vary among products and regions, use \code{weights=~n+r}. As long as there are no data gaps, the weights add up to 1 for each region.}
   \item{gaps, amount}{Percentage amount of gaps (between 0 and 1) to be introduced in the data.}
   \item{prob}{A vector of probability weights, see also \code{\link{sample}}. Either \code{NULL} or the same length as \code{r} and \code{n}. Larger values make gaps occur more likely at this position.}
   \item{pairs}{A logical indicating if gaps should be introduced such that there are still at least two observations per product available (\code{pairs=TRUE}). Only in this case, all products provide valuable information for a spatial price comparison. Otherwise, if \code{pairs=FALSE}, there can be products with only one observation. See also the Details section.}
   \item{exclude}{Data.frame of two (character) variables \code{r} and \code{n}, specifying regions and products to be excluded from introducing gaps. Default is \code{NULL}, meaning that gaps are allowed to occur in all regions and products present in the data. Missing values (\code{NA}) are translated into no gaps for the corresponding product or region, e.g. \code{data.frame(r="r1", n=NA)} means that there will be no gaps in region \code{r1}.}
   \item{settings}{A list of control settings to be used. The following settings are supported:
   \itemize{
   \item \code{gaps.prob} : See argument \code{prob}.
   \item \code{gaps.pairs} : See argument \code{pairs}.
   \item \code{gaps.exclude} : See argument \code{exclude}.
   \item \code{par.sd} : named vector specifying the standard deviations used for sampling true parameters and errors. Default is \code{c(lnP=0.1, pi=exp(1), delta=0.5, error=0.01)}. \strong{Important note:} Setting the standard deviation of delta is currently not implemented for \code{weights=~n+r}.
   \item \code{par.add} : logical, specifying if the parameters underlying the data generating process should be added the function output. This is particularly useful if \code{prices()} is applied in simulations. Default is \code{FALSE}.
   }
   }
}

\author{Sebastian Weinand}

\value{For \code{gaps()}, a vector of \code{TRUE} and \code{FALSE}; for \code{weights()} a numeric vector of weights. \code{prices()} gives a data.table with the following variables:
\tabular{lll}{
   \code{region} \tab \tab region identifier (factor)\cr
   \code{product} \tab \tab product identifier (factor)\cr
   \code{weight} \tab \tab weights (numeric) if not \code{NULL}\cr
   \code{price} \tab \tab sampled price (numeric)\cr
}
or a list with the sampled data and its underlying parameter values, if \code{settings=list(par.add=TRUE)}.
}

\details{
Function \code{gaps()} ensures that gaps do not lead to non-connected price data (see \code{\link{is_connected}}). Therefore, it could happen that the amount of gaps specified in \code{gaps()} is only approximate, in particluar, in cases where certain regions and/or products should additionally be exluded from exhibiting gaps by \code{exclude}.

If \code{gaps(pairs=FALSE)}, the minimum number of observations for a connected data set is \eqn{R+N-1}. Otherwise, for \code{gaps(pairs=TRUE)}, this number is defined by \eqn{2N+\text{max}(0, R-N-1)}.
}

\examples{
# sample complete price data:
data <- prices(R = 4, N = 3)

# add weights:
data[, "w1" := weights(r=region, n=product, type=~1)] # constant
data[, "w2" := weights(r=region, n=product, type=~n)] # product-specific
data[, "w3" := weights(r=region, n=product, type=~n+r)] # product-region-specific

# weights add up to 1:
data[, sum(w1), by = "region"]
data[, sum(w2), by = "region"]
data[, sum(w3), by = "region"]

# or, directly, in one step:
data <- prices(R = 4, N = 3, weights = ~n)

# introduce 25\% random gaps:
data.gaps <- data[!gaps(region, product, amount = 0.25), ]

# weights no longer add up to 1 in each region:
data.gaps[, sum(weight), by = "region"]

# approx. 25\% random gaps, but keep observation for product "n2"
# in region "r1" and all observations in region "r2":
no_gaps <- data.frame(r=c("r1","r2"), n=c("n2",NA))

# apply to data:
data[!gaps(region, product, amount = 0.25, exclude = no_gaps), ]

# or, directly, in one step:
data <- prices(R=4, N=3, gaps=0.25, settings=list("gaps.exclude"=no_gaps))

# introduce systematic gaps:
data <- prices(R=15, N=10)
data[, "prob" := data.table::rleidv(product)] # probability for gaps increases per product
data.gaps <- data[!gaps(r=region, n=product, amount=0.25, prob=prob), ]
plot(table(data.gaps$product), type="l")
}
