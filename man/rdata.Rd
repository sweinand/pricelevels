\name{rdata}

\alias{rdata}
\alias{rgaps}
\alias{rweights}
\alias{rsales}

\title{Simulate random price and quantity data}

\description{Simulate price data and introduce gaps into these price data while ensuring interregional connectivity. Price data include quantities and expenditure share weights. The underlying sampling of prices uses the NLCPD model (see \code{\link{nlcpd}}).}

\usage{
rgaps(r, n, amount = 0, prob = NULL, pairs = FALSE, exclude = NULL)

rweights(r, n, type = ~1)

rsales(p, q, amount = 0, max.rebate = 1/4, max.qi = 2)

rdata(R, N, gaps = 0, sales = 0, settings = list())
}

\arguments{
   \item{r, n}{A character vector or factor of regional entities \code{r} and products \code{n}, respectively.}
   \item{p, q}{A numeric vector of prices \code{p} and quantities \code{q}, respectively.}
   \item{R, N}{A single integer specifying the number of regions and products, respectively.}
   \item{type}{A formula specifiyng the sampling of expenditure share weights. If \code{type=~1}, products receive identical weights, while weights are product-specific for \code{type=~n}. If weights should vary among products and regions, use \code{type=~n+r}. As long as there are no data gaps, the weights add up to 1 for each region.}
   \item{gaps, sales, amount}{Percentage amount of gaps and sales (between 0 and 1), respecitvely, to be introduced in the data.}
   \item{prob}{A vector of probability weights, see also \code{\link{sample}}. Either \code{NULL} or the same length as \code{r} and \code{n}. Larger values make gaps occur more likely at this position.}
   \item{pairs}{A logical indicating if gaps should be introduced such that there are always at least two observations per product available (\code{pairs=TRUE}). Only in this case, all products provide valuable information for a spatial price comparison. Otherwise, if \code{pairs=FALSE}, there can be products with only one observation. See also the Details section.}
   \item{exclude}{Data.frame of two (character) variables \code{r} and \code{n}, specifying regions and products to be excluded from introducing gaps. Default is \code{NULL}, meaning that gaps are allowed to occur in all regions and products present in the data. Missing values (\code{NA}) are translated into no gaps for the corresponding product or region, e.g. \code{data.frame(r="r1", n=NA)} means that there will be no gaps in region \code{r1}.}
   \item{max.rebate, max.qi}{Maximum allowed percentage price rebate and quantity decrease (between 0 and 1), respectively.}
   \item{settings}{A list of control settings to be used. The following settings are supported:
   \itemize{
   \item \code{gaps.prob} : See argument \code{prob}.
   \item \code{gaps.pairs} : See argument \code{pairs}.
   \item \code{gaps.exclude} : See argument \code{exclude}.
   \item \code{sales.max.rebate} : See argument \code{max.rebate}.
   \item \code{sales.max.qi} : See argument \code{max.qi}.
   \item \code{par.sd} : named vector specifying the standard deviations used for sampling true parameters and errors. Default is \code{c(lnP=0.1, pi=exp(1), delta=0.5, error=0.01)}.
   \item \code{par.add} : logical, specifying if the parameters underlying the data generating process should be added the function output. This is particularly useful if \code{rdata()} is applied in simulations. Default is \code{FALSE}.
   }
   }
}

\author{Sebastian Weinand}

\value{For \code{rgaps()}, a logical vector and for \code{rweights()} a numeric vector of expenditure share weights. Function \code{rsales()} returns a list of prices and quantities with a logical flag indicating sales. Function \code{rdata()} gives a data.table with the following variables:
\tabular{lll}{
   \code{region} \tab \tab region identifier (factor)\cr
   \code{product} \tab \tab product identifier (factor)\cr
   \code{is_sale} \tab \tab are prices and quantities affected by sales (logical)\cr
   \code{price} \tab \tab sampled price (numeric)\cr
   \code{quantity} \tab \tab consumed quantity (numeric)\cr
   \code{share} \tab \tab expenditure share weights (numeric)\cr

}
or a list with the sampled data and its underlying parameter values, if \code{settings=list(par.add=TRUE)}.
}

\details{
Function \code{rgaps()} ensures that gaps do not lead to non-connected price data (see \code{\link{is.connected}}). Therefore, it could happen that the amount of gaps specified in \code{rgaps()} is only approximate, in particluar, in cases where certain regions and/or products should additionally be exluded from exhibiting gaps by \code{exclude}.

If \code{rgaps(pairs=FALSE)}, the minimum number of observations for a connected data set is \eqn{R+N-1}. Otherwise, for \code{rgaps(pairs=TRUE)}, this number is defined by \eqn{2N+\text{max}(0, R-N-1)}.
}

\examples{
# sample complete price data:
dt <- rdata(R = 4, N = 3)

# add expenditure share weights:
dt[, "w1" := rweights(r=region, n=product, type=~1)] # constant
dt[, "w2" := rweights(r=region, n=product, type=~n)] # product-specific
dt[, "w3" := rweights(r=region, n=product, type=~n+r)] # product-region-specific

# weights add up to 1:
dt[, list("w1"=sum(w1),"w2"=sum(w2),"w3"=sum(w3)), by = "region"]

# introduce 25\% random gaps:
dt.gaps <- dt[!rgaps(region, product, amount = 0.25), ]

# weights no longer add up to 1 in each region:
dt.gaps[, list("w1"=sum(w1),"w2"=sum(w2),"w3"=sum(w3)), by = "region"]

# approx. 25\% random gaps, but keep observation for product "n2"
# in region "r1" and all observations in region "r2":
no_gaps <- data.frame(r=c("r1","r2"), n=c("n2",NA))

# apply to data:
dt[!rgaps(region, product, amount = 0.25, exclude = no_gaps), ]

# or, directly, in one step:
dt <- rdata(R=4, N=3, gaps=0.25, settings=list("gaps.exclude"=no_gaps))

# introduce systematic gaps:
dt <- rdata(R=15, N=10)
dt[, "prob" := data.table::rleidv(product)] # probability for gaps increases per product
dt.gaps <- dt[!rgaps(r=region, n=product, amount=0.25, prob=prob), ]
plot(table(dt.gaps$product), type="l")
}
