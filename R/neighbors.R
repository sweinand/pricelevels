# START

# Title:  Interregional connections
# Author: Sebastian Weinand
# Date:   2023-07-05

# divide into connected regions:
neighbors <- function(r, n, simplify = FALSE){

  # input checks:
  .check.char(x=r)
  .check.char(x=n)
  .check.log(x=simplify, min.len=1, max.len=1, miss.ok=TRUE, na.ok=FALSE)
  .check.lengths(x=r, y=n)

  # coerce input vectors to character:
  region <- as.character(r)
  product <- as.character(n)

  # create copy of initial region vector:
  region0 <- region

  # drop missing values:
  missings <- is.na(region) | is.na(product)
  region <- region[!missings]
  product <- product[!missings]

  # output in case there are only NAs:
  if(all(missings)){

    # set output to NA:
    out <- list(NA_character_)

  }else{

    # explanation:
    # a) one region and one or multiple product(s)
    #    -> no regional connections possible -> one connected group of regions
    # b) multiple regions and one product
    #    -> all regions connected -> one connected group of regions
    if(length(unique(region)) <= 1 | length(unique(product)) <= 1){

      # set output to unique regions:
      out <- list(unique(region))

    }else{

      # set starting parameters:
      j <- 1 # iteration of loop
      R <- NULL # regions in j-th group
      out <- list() # container to store regional groups

      # loop as long as regions are present in the vector:
      while(length(region) > 0){

        # regions in which "first" product is present:
        r_fst <- unique(region[product == product[1]])

        # set start parameter:
        i <- 1 # iteration of loop

        # loop maximally over all (remaining) unique regions:
        while(i < length(unique(region))){

          # number of regions at i-th iteration:
          n0 <- length(r_fst)

          # regions in which "first" and following products are present:
          r_fst <- unique(region[product %in% product[region %in% r_fst]])

          # break loop when no changes occur, i.e. no regions are added:
          if(n0 >= length(r_fst)){break()}

          # increment by one:
          i <- i+1

        }

        # store neighboring regions:
        out[[j]] <- r_fst

        # regions which are processed, i.e. already assigned to a group and
        # therefore not available for further processing:
        R <- c(R, r_fst)

        # index of (remaining) regions:
        idx_match <- region %in% R
        # -> all regions when starting

        # subset regions to remaining regions:
        region <- region[!idx_match]

        # subset products to remianing regions:
        product <- product[!idx_match]

        # increment by one:
        j <- j+1

      }

    }

  }

  # divide into groups of connected regions:
  if(simplify){

    # store neighbors as vector:
    ngbs_all <- unlist(x = out, use.names = FALSE)

    # add names:
    names(ngbs_all) <- rep(x = 1:length(out), times = lengths(out))

    # remove NAs in order to prevent matching between NAs:
    ngbs_all <- ngbs_all[!is.na(ngbs_all)]

    # assign regions to corresponding groups:
    as.factor(names(ngbs_all)[match(x = region0, table = ngbs_all)])

  }else{

    # unique list elements form regional connections:
    out

  }

}

# connect price data:
connect <- function(r, n){
  
  # @description:
  # simple wrapper of neighbors() for connecting 
  # price data by keeping the only the connected
  # observations with maximum number of observations
  
  # @value:
  # logical indicating the observations to be kept
  
  # divide into connected region groups:
  ngbs <- neighbors(r = r, n = n, simplify = TRUE)
  
  # frequency count of observations by group:
  ngbs.tab <- table(ngbs, useNA = "no")
  
  # logical indicating the observations to be kept:
  ngbs %in% names(which.max(ngbs.tab))
  
}

# flag if region-product-sample is connected:
is.connected <- function(r, n){

  # input checks:
  .check.char(x=r)
  .check.char(x=n)
  .check.lengths(x=r, y=n)

  # coerce input vectors to character:
  region <- as.character(r)
  product <- as.character(n)

  # drop missing values:
  missings <- is.na(region) | is.na(product)
  region <- region[!missings]
  product <- product[!missings]

  # output in case there are only NAs:
  if(all(missings)){

    # set output to NA:
    out <- as.logical(NA)

  }else{

    # explanation:
    # a) one region and one or multiple product(s)
    #    -> no regional connections possible -> one connected group of regions
    # b) multiple regions and one product
    #    -> all regions connected -> one connected group of regions
    if(length(unique(region)) <= 1 | length(unique(product)) <= 1){

      # set output to TRUE:
      out <- TRUE

    }else{

      # regions in which "first" product is present:
      r_fst <- unique(region[product == product[1]])

      # set start parameter:
      i <- 1 # iteration of loop

      # loop maximally over all (remaining) unique regions:
      while(i < length(unique(region))){

        # number of regions at i-th iteration:
        n0 <- length(r_fst)

        # regions in which "first" and following products are present:
        r_fst <- unique(region[product %in% product[region %in% r_fst]])

        # break loop when no changes occur, i.e. no regions are added:
        if(n0 >= length(r_fst)){break()}

      }

      # check if all regions are present in this group:
      out <- all(unique(region) %in% r_fst)
      # -> if this is not the case, then there are multiple,
      #    separated groups

    }

  }

  # print output to console:
  out

}

# get amount of computable pairwise comparisons:
comparisons <- function(r, n, ngbs=NULL){

  # input checks:
  .check.char(x=r)
  .check.char(x=n)
  .check.lengths(x=r, y=n)
  if(!is.null(ngbs)){if(!is.list(ngbs)){stop("'Wrong input for 'ngbs': list or NULL expected.")}}

  # coerce input vectors to character:
  region <- as.character(r)
  product <- as.character(n)

  # create copy of initial region vector:
  region0 <- region

  # drop missing values:
  missings <- is.na(region) | is.na(product)
  region <- region[!missings]
  product <- product[!missings]

  # output in case there are only NAs:
  if(all(missings)){

    # store all numbers:
    out <- data.table("group_id" = 1L,
                      "group_members" = NA_character_,
                      "group_size" = NA_integer_,
                      "total" = NA_integer_,
                      "direct" = NA_integer_,
                      "indirect" = NA_character_,
                      "n_obs" = NA_integer_)

  }else{

    # regional occurences by product:
    X0 <- as.matrix(x = table(product, region, useNA = "no"))

    # number of observations that contain interregional information:
    n0 <- colSums(X0[rowSums(X0) > 1, , drop = FALSE])

    # remove duplicated pairs, e.g. in case one product
    # is observed multiple times in multiple regions:
    X0[X0 > 1] <- 1

    # matrix of 1st/direct neighbors:
    X1 <- crossprod(x = X0)
    # -> this is the time consuming part when there are
    #    many regions and/or products!

    # remove redundant pairs, e.g. comparison of A with B
    # is the same as B with A:
    X1[lower.tri(X1, diag = FALSE)] <- 0

    # transform to data.table:
    ngbs1 <- as.data.table(as.data.frame(as.table(X1), stringsAsFactors = FALSE))

    # set names:
    setnames(x = ngbs1, c("var1", "var2", "freq"))

    # remove further redundant pairs, e.g. comparison of A with A,
    # and pairs which are not present in the data:
    ngbs1 <- ngbs1[(var1 != var2) & freq > 0, list(var1, var2)]

    # divide into groups of connected regions:
    if(is.null(ngbs)){ngbs <- neighbors(r = region, n = product, simplify = FALSE)}

    # region groups:
    groups <- as.vector(unlist(x = lapply(X = ngbs, FUN = function(i) if(length(i) > 5){paste(paste(i[1:5], collapse = ";"), "...", sep = ";")}else{paste(i, collapse = ";")}), use.names = FALSE))

    # group size:
    group_size <- as.vector(unlist(x = lapply(X = ngbs, FUN = length), use.names = FALSE))

    # number of directly connected regional pairs:
    n_dir <- as.vector(unlist(x = lapply(X = ngbs, FUN = function(i) sum(ngbs1$var1 %in% i)), use.names = FALSE))

    # number of direct and indirect ('total') regional pairs:
    n_total <- as.vector(unlist(x = lapply(X = ngbs, FUN = function(i) length(i)*(length(i)-1)/2), use.names = FALSE))

    # number of indirectly connected regional pairs:
    n_ind <- n_total - n_dir

    # number observations that provide interregional information:
    n_obs <- unlist(x = lapply(X = ngbs, FUN = function(i) sum(n0[match(i, names(n0))])), use.names = FALSE)

    # bundle summary measures:
    out <- data.table("group_id" = as.integer(1:length(ngbs)),
                      "group_members" = as.character(groups),
                      "group_size" = as.integer(group_size),
                      "total" = as.integer(n_total),
                      "direct" = as.integer(n_dir),
                      "indirect" = as.integer(n_ind),
                      "n_obs" = as.integer(n_obs))

  }

  # set key:
  setkeyv(x = out, cols = "group_id")

  # print output to console:
  out[]

}

# function to compute sparsity of price data:
sparsity <- function(r, n, useable=FALSE){

  # input checks:
  .check.char(x=r)
  .check.char(x=n)
  .check.log(x=useable, min.len=1, max.len=1, miss.ok=TRUE, na.ok=FALSE)
  .check.lengths(x=r, y=n)

  # number of regions by product:
  freq_tab <- as.matrix(table(n, r, useNA = "no"), drop = FALSE)

  # subset to products which are priced in multiple regions:
  if(useable){freq_tab <- freq_tab[rowSums(freq_tab) > 1, , drop = FALSE]}

  # compute sparsity of price data:
  as.vector(x = 1 - sum(freq_tab > 0)/prod(dim(freq_tab)), mode = "numeric")

}

# END
