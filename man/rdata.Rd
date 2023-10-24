\name{rdata}

\alias{rdata}
\alias{rgaps}
\alias{rweights}

\title{Simulate random price and quantity data}

\description{Simulate random price and quantity data for a specified number of regions \eqn{(r=1,\ldots,R)}, product groups \eqn{(b=1,\ldots,B)}, and individual products \eqn{(n=1,\ldots,N_{b})} using function \code{rdata()}.

The sampling of prices relies on the NLCPD model (see \code{\link{nlcpd}()}), while expenditure weights for product groups are sampled using function \code{rweights()}. Purchased quantities are assigned to individual products. Moreover, random sales and gaps (using function \code{rgaps()}) can be introduced in the sampled data.}

\usage{
rgaps(r, n, amount = 0, prob = NULL, pairs = FALSE, exclude = NULL)

rweights(r, b, type = ~1)

rdata(R, B, N, gaps = 0, weights = ~b+r, sales = 0, settings = list())
}

\arguments{
   \item{r, n, b}{A character vector or factor of regional entities \code{r}, individual products \code{n}, and product groups (or basic headings) \code{b}, respectively.}
   \item{p, q}{A numeric vector of prices \code{p} and quantities \code{q}, respectively.}
   \item{R, B, N}{A single integer specifying the number of regions \code{R} and product groups \code{B}, respectively, and a vector of length \code{B} specifying the number of individual products \code{N} in each product group.}
   \item{weights, type}{A formula specifiyng the sampling of expenditure weights for product groups. If \code{type=~1}, product groups receive identical weights, while weights are product group specific for \code{type=~b}. If weights should vary among product groups and regions, use \code{type=~b+r}. As long as there are no data gaps, the weights add up to 1 for each region.}
   \item{gaps, sales, amount}{Percentage amount of gaps and sales (between 0 and 1), respecitvely, to be introduced in the data.}
   \item{prob}{A vector of probability weights, see also \code{\link{sample}}. Either \code{NULL} or the same length as \code{r} and \code{n}. Larger values make gaps occur more likely at this position.}
   \item{pairs}{A logical indicating if gaps should be introduced such that there are always at least two observations per product available (\code{pairs=TRUE}). Only in this case, all products provide valuable information for a spatial price comparison. Otherwise, if \code{pairs=FALSE}, there can be products with only one observation. See also the Details section.}
   \item{exclude}{Data.frame of two (character) variables \code{r} and \code{n}, specifying regions and products to be excluded from introducing gaps. Default is \code{NULL}, meaning that gaps are allowed to occur in all regions and products present in the data. Missing values (\code{NA}) are translated into no gaps for the corresponding product or region, e.g. \code{data.frame(r="r1", n=NA)} means that there will be no gaps in region \code{r1}.}
   \item{settings}{A list of control settings to be used. The following settings are supported:
   \itemize{
   \item \code{gaps.prob} : See argument \code{prob}.
   \item \code{gaps.pairs} : See argument \code{pairs}.
   \item \code{gaps.exclude} : See argument \code{exclude}.
   \item \code{sales.max.rebate} : Maximum allowed percentage price rebate for a sale (between 0 and 1). Default is \code{1/4}.
   \item \code{sales.max.qi} : Maximum allowed percentage quantity decrease for a sale (between 0 and 1). Default is \code{2}.
   \item \code{par.sd} : named vector specifying the standard deviations used for sampling true parameters and errors. Default is \code{c(lnP=0.1, pi=exp(1), delta=0.5, error=0.01)}.
   \item \code{par.add} : logical, specifying if the parameters underlying the data generating process should be added the function output. This is particularly useful if \code{rdata()} is applied in simulations. Default is \code{FALSE}.
   \item \code{round} : logical, specifying if prices should be rounded to two decimals or not. While prices usually have two decimal places in reality, this rounding can cause small differences between estimated and true parameter values. For simulation purposes, it is therefore recommended to use unrounded prices by setting \code{round=FALSE}.
   }
   }
}

\author{Sebastian Weinand}

\value{
Function \code{rgaps()} returns a logical vector of the same length as \code{r} where \code{TRUE}s indicate gaps and \code{FALSE}s no gaps.

Function \code{rweights()} returns a numeric vector of (non-negative) expenditure share weights of the same length as \code{r}.

Function \code{rdata()} returns a data.table with the following variables:
\tabular{lll}{
   \code{group} \tab \tab product group identifier (factor)\cr
   \code{weight} \tab \tab expenditure weight of product groups (numeric)\cr
   \code{region} \tab \tab region identifier (factor)\cr
   \code{product} \tab \tab product identifier (factor)\cr
   \code{sale} \tab \tab are prices and quantities affected by sales (logical)\cr
   \code{price} \tab \tab sampled price (numeric)\cr
   \code{quantity} \tab \tab consumed quantity (numeric)\cr
   \code{share} \tab \tab expenditure share weights (numeric)\cr
}
or a list with the sampled data and its underlying parameter values, if \code{settings=list(par.add=TRUE)}.
}

\details{
Function \code{rgaps()} ensures that gaps do not lead to non-connected price data (see \code{\link{is.connected}}). Therefore, it could happen that the amount of gaps specified in \code{rgaps()} is only approximate, in particluar, in cases where certain regions and/or products should additionally be exluded from exhibiting gaps by \code{exclude}.

If \code{rgaps(pairs=FALSE)}, the minimum number of observations for a connected data set is \eqn{R+N-1}. Otherwise, for \code{rgaps(pairs=TRUE)}, this number is defined by \eqn{2N+\text{max}(0, R-N-1)}.

Note that setting \code{sales>0} in function \code{rdata()} distorts the initial price generating process. Consequently, parameter estimates may deviate stronger from their true values. Note also that the sampled expenditure weights \code{weight} represent the relevance of product groups as (often) derived from national accounts and other data sources. Therefore, they cannot be derived from the sampled prices and quantities in the data, which would represent the expenditure shares of available products.}

\examples{
# sample price data for ten regions and five product groups
# containing three individual products each:
set.seed(1)
dt <- rdata(R=10, B=5, N=3)
boxplot(price~paste(group, product, sep=":"), data=dt)

# sample price data for ten regions and five product groups
# containing one to five individual products:
set.seed(1)
dt <- rdata(R=10, B=5, N=c(1,2,3,4,5))
boxplot(price~paste(group, product, sep=":"), data=dt)

# sample price data for three product groups (with one product each) in four regions:
dt <- rdata(R=4, B=3, N=1)

# add expenditure share weights:
dt[, "w1" := rweights(r=region, b=group, type=~1)] # constant
dt[, "w2" := rweights(r=region, b=group, type=~b)] # product-specific
dt[, "w3" := rweights(r=region, b=group, type=~b+r)] # product-region-specific

# weights add up to 1:
dt[, list("w1"=sum(w1),"w2"=sum(w2),"w3"=sum(w3)), by="region"]

# introduce 25\% random gaps:
dt.gaps <- dt[!rgaps(r=region, n=product, amount=0.25), ]

# weights no longer add up to 1 in each region:
dt.gaps[, list("w1"=sum(w1),"w2"=sum(w2),"w3"=sum(w3)), by="region"]

# approx. 25\% random gaps, but keep observation for product "n2"
# in region "r1" and all observations in region "r2":
no_gaps <- data.frame(r=c("r1","r2"), n=c("n2",NA))

# apply to data:
dt[!rgaps(r=region, n=product, amount=0.25, exclude=no_gaps), ]

# or, directly, in one step:
dt <- rdata(R=4, B=3, N=1, gaps=0.25, settings=list("gaps.exclude"=no_gaps))

# introduce systematic gaps:
dt <- rdata(R=15, B=1, N=10)
dt[, "prob" := data.table::rleidv(product)] # probability for gaps increases per product
dt.gaps <- dt[!rgaps(r=region, n=product, amount=0.25, prob=prob), ]
plot(table(dt.gaps$product), type="l")
}
