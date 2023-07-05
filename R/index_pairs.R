# START

# Title:  Compute index pairs
# Author: Sebastian Weinand
# Date:   2020-05-18

# compute bilateral index pairs:
index.pairs <- function(x, r, n, w=NULL, type="jevons", all.pairs=TRUE, as.dt=FALSE){

  # allowed values for index type:
  index_vals <- c("jevons", "carli", "dutot", "harmonic", "toernq", "laspey", "paasche", "walsh", "fisher")

  # weights required for following index types:
  index_weights <- c("toernq", "laspey", "paasche", "walsh", "fisher")

  # input checks:
  .check.num(x=x, int=c(0, Inf))
  .check.char(x=r)
  .check.char(x=n)
  .check.char(x=type, min.len=1, max.len=1, na.ok=FALSE)
  .check.log(x=all.pairs, min.len=1, max.len=1, miss.ok=TRUE, na.ok=FALSE)
  .check.log(x=as.dt, min.len=1, max.len=1, miss.ok=TRUE, na.ok=FALSE)
  .check.lengths(x=r, y=n)
  .check.lengths(x=r, y=x)

  # set index function based on type:
  type <- match.arg(arg = type, choices = index_vals)
  if(type == "jevons"){index_func <- .jevons}
  if(type == "carli"){index_func <- .carli}
  if(type == "dutot"){index_func <- .dutot}
  if(type == "harmonic"){index_func <- .harmonic}
  if(type == "toernq"){index_func <- .toernq}
  if(type == "laspey"){index_func <- .laspey}
  if(type == "paasche"){index_func <- .paasche}
  if(type == "walsh"){index_func <- .walsh}
  if(type == "fisher"){index_func <- .fisher}

  # more sophisticated error handling for weights:
  if(type%in%index_weights){
    
    if(is.null(w) || missing(w)){
      stop(paste0("Non-valid input for type -> weights required for type='", type, "'"))
    }else{
      .check.num(x=w, miss.ok=FALSE, null.ok=FALSE, int=c(0, Inf))
      .check.lengths(x=r, y=w)
    }
    
  }else{
    
    if(!is.null(w)){
      warning("Weights supplied, but will be ignored for type='", type, "'")
    }
    w <- NULL # set to NULL such that weights are ignored in the following
    
  }

  # coerce to character:
  r <- factor(r)
  n <- factor(n)

  # convert prices and weights into matrix:
  prices <- tapply(X = x, INDEX = list(n, r), FUN = mean)
  if(!is.null(w)){weights <- tapply(X = w, INDEX = list(n, r), FUN = mean)}

  # number of regions or time periods:
  n_cols <- ncol(prices)

  # container to store price levels:
  out <- matrix(data = NA_real_,
                nrow = n_cols,
                ncol = n_cols,
                dimnames = list(colnames(prices), colnames(prices)))

  # slow, but complete or quick, but only non-redundant:
  if(!all.pairs){

    # loop over all columns:
    for(i in 1:n_cols){

      # tighten column selection in each iteration:
      idx <- i:n_cols

      # aggregate prices into price levels:
      out[i, idx] <- index_func(prices = prices[, idx, drop = FALSE], weights = weights[, idx, drop = FALSE], base = 1L)

    }

  }else{

    # loop over all columns:
    for(i in 1:n_cols){

      # aggregate prices into price levels:
      out[i, ] <- index_func(prices = prices, weights = weights, base = i)

    }

  }

  # convert to dataframe:
  if(as.dt){

    # convert into dataframe:
    out <- as.data.table(as.table(t(out)))

    # drop missing values:
    out <- out[!is.na(N),]

    # set column names:
    setnames(x = out, c("region", "base", "price_level"))

    # set key:
    setkeyv(x = out, cols = c("region", "base"))

  }

  # print output to console:
  out[]

}

# END
