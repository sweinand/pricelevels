# START


### NOT ACTIVE


# Title:  CD regression
# Author: Sebastian Weinand
# Date:   2020-10-18

# helper function to create block (covariance) matrix:
.block_mat <- function(x, blocks = c(1, 0.5)){

  # # check input for "x":
  # if(missing(x)){stop("'x' is missing.")}
  # if(!(is.vector(x = x, mode = "character") | is.factor(x = x))){stop("'x' must be a character vector or a factor.")}
  # if(length(x) <= 0){stop("'x' must be of length greater than 0.")}
  # if(all(is.na(x))){stop("All elements in 'x' are NA.")}
  #
  # # check input for "blocks":
  # if(missing(blocks)){blocks <- c(1, 0)} # set to default
  # if(!(is.vector(x = blocks, mode = "numeric") | is.vector(x = blocks, mode = "integer"))){stop("'blocks' must be a numeric vector.")}
  # if(length(blocks) <= 0 | length(blocks) > 2){stop("'blocks' must be of length two.")}

  # coerce to character:
  x <- as.character(x)

  # create block matrix:
  mat <- matrix(data = sapply(x, function(z) ifelse(z == x, blocks[2], 0)),
                nrow = length(x),
                ncol = length(x),
                dimnames = list(x, x))

  # replace diagonal values by one:
  diag(mat) <- blocks[1]

  # print output to console:
  mat

}

# helper function to create dummy matrix:
.dummy_mat <- function(x, lvls, base){

  # coerce variable to factor:
  x <- factor(x = x, levels = lvls)

  # set contrasts:
  if(is.null(base)){

    contrasts(x = x) <- contr.sum(lvls)
    colnames(contrasts(x = x)) <- lvls[-length(lvls)]

  }else{

    contrasts(x = x) <- contr.treatment(lvls)

  }

  # create dummy matrix:
  out <- model.matrix(~ x)
  # keep intercept in model.matrix because only than "contr.sum"
  # and "contr.treatment" differ

  # adjust column names:
  colnames(out) <- sub(pattern = "^x", replacement = "", x = colnames(out), ignore.case = TRUE)

  # print output to console:
  out

}

# estimate price levels by CD method:
cd <- function(x, r, n, w = NULL, base = NULL, details = FALSE, agg_weights_by = function(x, x0){(x+x0)/2}){

  # input checks:
  .check_dim(x = r)
  .check_dim(x = n)
  .check_price(x = x)
  .check_weights(x = w, allow_null = TRUE)
  .check_base(x = base, allow_null = TRUE)
  .check_logical(x = details)
  .check_lengths(x = r, y = n)
  .check_lengths(x = r, y = x)
  .check_lengths(x = r, y = w)

  # set default function for weight aggregation:
  if(missing(agg_weights_by)){agg_weights_by <- function(x, x0){(x+x0)/2}} # set to default value
  if(!is.function(agg_weights_by)){stop("'agg_weights_by' must be a function for aggregating the weights, e.g. function(x, x0){(x+x0)/2}.")}
  if(!all(c("x", "x0") %in% names(as.list(args(agg_weights_by))))){stop("'agg_weights_by' must have arguments x and x0.")}

  # complete cases:
  full_row <- complete.cases(r, n, x, w)

  # stop if no observations left:
  if(all(!full_row)){stop("No complete cases available. All data pairs contain at least one NA.")}

  # keep only complete cases:
  region <- r[full_row]
  product <- n[full_row]
  price <- x[full_row]
  w <- w[full_row]

  # coerce to factor:
  region <- factor(region)
  # do not use "as.factor()" because this does not drop unused factor levels

  # relevel if necessary:
  if(base %in% levels(region) && !is.null(base)){region <- relevel(x = region, ref = base)}

  # coerce to factor:
  product <- factor(product)
  # do not use "as.factor()" because this does not drop unused factor levels

  # CASE: one region, one or multiple products:
  if(nlevels(region) <= 1){

    # no interregional price comparsion possible:
    out <- NA_real_

    # set names only if exactly one level:
    if(nlevels(region) == 1L){names(out) <- levels(region)}

  }else{

    # calculate regional price ratios per product:
    ratios_df <- ratios(r = region,
                        n = product,
                        x = price,
                        base = NULL,
                        as_dt = TRUE,
                        drop_base = TRUE)

    # add weights:
    if(!is.null(w)){

      # weights must be transformed/aggregated into weights for
      # price ratios, e.g. weight of product 1 in region A
      # differs to that of product 1 in region B -> what is
      # the weight of this price ratio?

      # weights are calculated like in Toernqvist index, e.g.
      # (w1A + w1B)/2:
      ratios_df$weights <- .apply_per_group(x = w,
                                            r = region,
                                            n = product,
                                            base = NULL,
                                            as_dt = FALSE,
                                            drop_base = TRUE,
                                            FUN = agg_weights_by)

    }

    # create dummy matrices:
    X_base <- .dummy_mat(x = ratios_df$base, lvls = levels(region), base = base)
    X_comp <- .dummy_mat(x = ratios_df$region, lvls = levels(region), base = base)

    # build variance-covariance-matrix:
    Omega <- .block_mat(x = ratios_df$product, blocks = c(1, 0.5))
    # include "vcov"-argument?

    # decompose matrix:
    P <- chol(solve(Omega))
    # see Auer (2016), p. 485

    # transform variables:
    X <- (P%*%(X_comp - X_base))[, -1, drop = FALSE] # drop intercept to prevent multicollinearity
    y <- P%*%log(ratios_df$ratio) # logarithmic price ratios

    # # if we do not want to transform the model, we could use:
    # XO <- crossprod(x = X, y = solve(Omega))
    # beta <- solve(XO%*%X)%*%XO%*%y
    # # -> we need Cholesky decomposition (see above) for
    # # calculating of R-squared

    # weights matrix:
    if(is.null(w)){W <- diag(nrow(ratios_df))}else{W <- diag(ratios_df$weights)}

    # t(X)%*%X:
    XW <- crossprod(x = X, y = W)

    # estimation:
    beta <- solve(XW%*%X)%*%XW%*%y

    # coerce to vector and add dropped (base) coefficient:
    out <- as.numeric(c(ifelse(test = is.null(base), yes = 0-sum(as.vector(beta)), no = 0), as.vector(beta)))

    # set coefficient names:
    names(out) <- as.character(c(ifelse(test = is.null(base), yes = levels(region)[nlevels(region)], no = levels(region)[1]), rownames(beta)))

    # reorder:
    out <- out[match(x = levels(region), table = names(out))]

  }

  # add regression summary statistics:
  if(details){

    # transform to dataframe:
    out <- data.table("region" = names(out),
                      "log_price" = as.vector(out))

    if(nlevels(region) <= 1){

      # add additional variables:
      out <- data.table(out, NA_real_, NA_real_, NA_real_, as.numeric(0), as.numeric(1))

      # # set default values for objects:
      # mod_sum <- list("r.squared" = NA_real_, "sigma" = NA_real_)
      # freq_tab <- matrix(data = 1, nrow = 1, ncol = 1)

    }else{

      # compute statistics on regression coefficients:
      deg_free <- length(y)-length(beta) # degrees of freedom
      u <- y - X%*%beta # residuals
      sigma2 <- (crossprod(x = u, y = W)%*%u)/deg_free # estimated variance
      # sigma2 <- (t(u)%*%solve(Omega)%*%u)/deg_free # if no model transformation
      se <- sqrt(diag(as.vector(sigma2)*solve(XW%*%X))) # coefficient standard errors
      t_value <- as.vector(beta)/se # t-values
      p_value <- 2*pt(q = -abs(t_value), df = deg_free) # p-values
      # r_squared <- 1 - crossprod(u) / sum((y - mean(y))^2) # r-squared

      # coerce to dataframe:
      mod_sum_sub <- data.table("region" = as.character(names(se)),
                                "std_error" = as.numeric(se),
                                "t_val" = as.numeric(t_value),
                                "p_val" = as.numeric(p_value))

      # merge with present data:
      out <- merge(x = out,
                   y = mod_sum_sub,
                   by = "region",
                   all.x = TRUE)

      # number of regions by product:
      freq_tab <- as.matrix(table(as.character(product), as.character(region), useNA = "no"), drop = FALSE)

      # subset to products which are priced in multiple regions:
      freq_tab <- freq_tab[rowSums(freq_tab) > 1, , drop = FALSE]

      # number of observations by region relevant for CPD regression:
      n_obs <- colSums(x = freq_tab)

      # regional coverage:
      coverage <- colSums(x = freq_tab > 0)/nrow(freq_tab)

      # combine into dataframe:
      obs_df <- data.table("region" = names(n_obs),
                           "n_obs" = as.vector(n_obs),
                           "coverage" = as.vector(coverage))

      # merge with other data:
      out <- merge(x = out, y = obs_df, by = "region", all.x = TRUE)

    }

    # set names:
    setnames(x = out, c("region", "log_coef", "std_err", "t_val", "p_val", "n_obs", "coverage"))

    # set key:
    setkeyv(x = out, cols = "region")

    # # add further global statistics:
    # attr(x = out, which = "r_squared") <- r_squared
    # attr(x = out, which = "sigma") <- sqrt(sigma2)
    # attr(x = out, which = "sparsity") <- 1 - sum(freq_tab > 0)/prod(dim(freq_tab))

  }

  # print output to console:
  out[]

}

# EXAMPLES

# # connected price data:
# pdata1 <- sample_prices(R = 3, N = 4)
# levels(pdata1$region) <- c("a","b","c")
# pdata1[, cd(x = price, r = region, n = product)]
# pdata1[, cd(price, region, product, base = "a")]
# pdata1[, cd(price, region, product, details = TRUE)]
#
# # non-connected price data:
# pdata2 <- data.frame("region" = c("a","a","h","b","a","a","c","c","d","e","e","f",NA),
#                   "product" = as.character(c(1,1,"bla",1,2,3,3,4,4,5,6,6,7)),
#                   "price" = runif(13,5,6))
#
# is_connected(pdata2$region, pdata2$product)
# pdata2$groups <- neighbors(pdata2$region, pdata2$product, TRUE)
# lapply(X = split(pdata2, pdata2$groups),
#        FUN = function(z) cd(z$price, z$region, z$product))

# END
