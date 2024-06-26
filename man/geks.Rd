\name{geks}

\alias{geks}
\alias{index.pairs}

\title{GEKS method}

\description{Function \code{index.pairs()} computes bilateral index numbers for all pairs of regions. Based on that, function \code{geks()} derives regional price levels using the GEKS method proposed by Gini (1924, 1931), Elteto and Koves (1964), and Szulc (1964).}

\usage{
index.pairs(p, r, n, q=NULL, w=NULL, settings=list())

geks(p, r, n, q=NULL, w=NULL, base=NULL, simplify=TRUE, settings=list())
}

\arguments{
   \item{p}{A numeric vector of prices.}
   \item{r, n}{A character vector or factor of regional entities \code{r} and products \code{n}, respectively.}
   \item{q, w}{A numeric vector of non-negative quantities \code{q} or expenditure share weights \code{w} (see details) to be used in the computation of weighted bilateral index numbers. Can be \code{NULL}, if the index formula specified in \code{type} does not require quantities or weights. If both \code{q} and \code{w} are provided, \code{q} will be used.}
   \item{base}{A character specifying the base region to which all price levels are expressed. When \code{NULL}, they refer to the (unweighted) regional average.}
   \item{simplify}{A logical indicating whether the full regression-object should be provided (\code{FALSE}) or a named vector of estimated regional price levels (\code{TRUE}).}
   \item{settings}{A list of control settings to be used. The following settings are supported:
   \itemize{
   \item \code{chatty} : A logical specifying if warnings and info messages should be printed or not. The default is \code{getOption("pricelevels.chatty")}.
   \item \code{connect} : A logical specifying if the data should be checked for connectedness or not. The default is \code{getOption("pricelevels.connect")} for \code{geks()} and \code{FALSE} for \code{index.pairs()}. If the data are not connected, price levels are computed within the biggest block of connected regions or the block of regions to which the \code{base} region belongs. See also \code{\link{connect}()}.
   \item \code{plot} : A logical specifying if the calculated price levels should be plotted or not. If \code{TRUE}, the price ratios of each region are displayed as boxplots and the price levels are added as colored points. The default is \code{getOption("pricelevels.plot")}. Used only by \code{geks()}.
   \item \code{all.pairs} : Logical indicating whether index numbers should be computed for all region pairs (\code{TRUE}) or only for non-redundant ones (\code{FALSE}), e.g. the index number of regions \code{AB} should be the same as the inverse of \code{BA}. The default is \code{TRUE}.
   \item \code{type} : A character specifying the index method(s) used to aggregate prices into bilateral price indices for each pair of regions (first step of GEKS). See \code{\link{bilateral.index}} for allowed values. Multiple choices allowed. The default is \code{jevons}.
   \item \code{wmethod} : the weighting method (second step of GEKS). Allowed values are \code{none} for equal weighting of all bilateral price indices, \code{obs} for weighting the bilateral price indices according to the underlying number of intersecting observations, or \code{shares} for weighting according to the intersecting expenditure shares. The default is \code{none}. Used only by \code{geks()}.
   \item \code{qbase} : relevant only for \code{type='lowe'} and \code{type='young'}, see \code{\link{bilateral.index}}.
   }}
}

\author{Sebastian Weinand}

\references{
Gini, C. (1924). Quelques Considerations au Sujet de la Construction des Nombres Indices des Prix et des Questions Analogues. \emph{Mentron}, 4 (1), 3-162.

Gini, C. (1931). On the Circular Test of Index Numbers. \emph{International Statistical Review}, 9 (2), 3-25.

Elteto, O. and Koves, P. (1964). On a Problem of Index Number Computation Relating to International Comparison. \emph{Statisztikai Szemle}, 42, 507-518.

Szulc, B. J. (1964). Indices for Multiregional Comparisons. \emph{Przeglad Statystyczny}, 3, 239-254.
}

\value{
For \code{index.pairs()}, a data.table with variables \code{base} (the base region), \code{region} (the comparison region), and \code{eval(settings$type)} (the price level between the two regions).

For \code{geks()}, a named vector or matrix of (unlogged) regional price levels if \code{simplify=TRUE}. Otherwise, for \code{simplify=FALSE}, a \code{lm}-object containing the full regression output.
}

\details{
The GEKS index is a two-step approach. First, prices are aggregated into bilateral index numbers using the index given in \code{type}. This is done for all pairs of regions via function \code{index.pairs()}. Second, these bilateral index numbers are transformed into a set of multilateral, transitive index numbers.

Note that the quantities \code{q} or weights \code{w} are used within the aggregation of prices into index numbers (first stage) while the subsequent transformation of these index numbers (second stage) usually does not rely on any weights (but can if specified in \code{settings$wmethod}).

Before calculations start, missing values are excluded and duplicated observations for \code{r} and \code{n} are aggregated, that is, duplicated prices \code{p} and weights \code{w} are averaged and duplicated quantities \code{q} added up.

The weights \code{w} must represent expenditure shares defined as \eqn{w_i^r = p_i^r q_i^r / \sum_{j=1}^{N} p_j^r q_j^r}. They are internally (re-)normalized such that they add up to 1 for each region \code{r}.
}

\seealso{\code{\link{bilateral.index}}}

\examples{
# example data:
set.seed(123)
dt1 <- rdata(R=3, B=1, N=5)

### Index pairs

# matrix of bilateral index numbers:
Pje <- dt1[, index.pairs(p=price, r=region, n=product, settings=list(type="jevons"))]
# if the underlying index satisfies the country-reversal
# test (like the Jevons index), the price index numbers of
# the upper-right triangle are the same as the inverse of
# the price index numbers of the lower-left triangle.
all.equal(Pje$jevons[3], 1/Pje$jevons[7]) # true
# hence, one could set all.pairs=FALSE without loosing any
# information. however, this is no longer true for indices
# that do not satisfy this test (like the Carli index):
Pca <- dt1[, index.pairs(p=price, r=region, n=product, settings=list(type="carli"))]
all.equal(Pca$carli[3], 1/Pca$carli[7]) # false

### GEKS method

# for complete price data (no gaps), the jevons index is transitive.
# hence, no adjustment is needed by the geks approach, which is
# why the index numbers are the same:
all.equal(
  dt1[, geks(p=price, r=region, n=product, base="1", settings=list(type="jevons"))],
  dt1[, jevons(p=price, r=region, n=product, base="1")]
) # true

# this is no longer true when there are gaps in the data:
dt1.gaps <- dt1[!rgaps(region, product, amount=0.25), ]
all.equal(
  dt1.gaps[, geks(p=price, r=region, n=product, base="1", settings=list(type="jevons"))],
  dt1.gaps[, jevons(p=price, r=region, n=product, base="1")]
) # now, differences

# weighting at the second step of GEKS can be done with respect
# to the intersection of products for each pair of region:
dt1.gaps[, geks(p=price, r=region, n=product, base="1",
                settings=list(type="jevons", wmethod="obs"))]

# add price data:
dt2 <- rdata(R=4, B=1, N=4)
dt2[, "region":=factor(region, labels=4:7)]
dt2[, "product":=factor(product, labels=6:9)]
dt <- rbind(dt1, dt2)
dt[, is.connected(r=region, n=product)] # non-connected now

# compute all index pairs and geks:
require(data.table)
as.matrix(dcast(
  data=dt[, index.pairs(p=price, r=region, n=product)],
  formula=base~region,
  value.var="jevons"), rownames="base")
dt[, geks(p=price, r=region, n=product, base="1", settings=list(type="jevons"))]
}
