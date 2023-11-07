# START

# Title:  General helper functions
# Author: Sebastian Weinand
# Date:   6 November 2023

# set base region:
set.base <- function(r, base, null.ok, settings=list()){

  # @ allowed settings:
  # - chatty

  # @args:
  # r         factor, vector of regions
  # base      character, one base region
  # null.ok   logical, base=NULL allowed or not
  # chatty    logical, print warning or not

  # derive base region if required and base=NULL:
  if(is.null(base) && !null.ok){
    base <- names(which.max(table(r)))[1]
    if(settings$chatty){
      warning(paste0("Base region set to base='", base, "'"), call.=FALSE)
    }
  }

  # derive base region if not found in regions and base not NULL:
  if(!base%in%levels(r) && !is.null(base)){
    base <- names(which.max(table(r)))[1]
    if(settings$chatty){
      warning(paste0("Base region not found -> reset to base='", base, "'"), call.=FALSE)
    }
  }

  # return output:
  return(base)

}

# initialize data:
arrange <- function(p, r, n, q=NULL, w=NULL, base, settings=list()){

  # @ allowed settings:
  # - missings : check and remove NAs
  # - connect : check and remove non-connected regions
  # - duplicates : check and aggregate duplicated values
  # - chatty : print warnings

  # @output:
  # if 'q' and/or 'w' is provided, the output data will contain 'z=q'
  # and expenditure share weights that may differ to 'w' (if provided)
  # if only 'w' is provided, the output data will contain 'z=w'
  # if neither 'q' nor 'w' is provided, the output data will
  # contain 'z=1'

  # set quantities or weights if available:
  if(is.null(q) && is.null(w)){
    z <- rep(1, length(p))
  }else{
    if(is.null(q)) z <- w else z <- q
  }

  # gather in data.table:
  dt <- data.table("r"=as.character(r), "n"=as.character(n), "p"=as.numeric(p), "z"=as.numeric(z))

  # check and remove NAs:
  if(settings$missings){

    # if both q and w are provided, q will be checked:
    nas <- dt[, complete.cases(r, n, p, z)]
    dt <- dt[nas,]
    if(settings$chatty & sum(!nas)>0L){
      warning(paste(sum(!nas), "incomplete case(s) found and removed"), call.=FALSE)
    }

    # stop if no observations left:
    if(nrow(dt)<=0L){
      stop("No complete cases available -> all data pairs contain at least one NA", call.=FALSE)
    }

  }

  # subset to connected data:
  if(settings$connect){

    if(dt[, !spin::is.connected(r=r, n=n)]){

      # subset based on input:
      if(!base%in%levels(factor(dt$r)) || is.null(base)){
        dt <- dt[spin::connect(r=r, n=n), ]
      }else{
        dt[, "ng" := spin::neighbors(r=r, n=n, simplify=TRUE)]
        dt <- dt[ng%in%dt[r%in%base, unique(ng)], ]
      }

      # warning message:
      if(settings$chatty){
        warning("Non-connected regions -> computations with subset of data", call.=FALSE)
      }

    }

  }

  # check for duplicated entries:
  if(settings$duplicates && anyDuplicated(x=dt, by=c("r","n"))>0L){

    # average duplicated prices and weights, sum duplicated quantities:
    if(is.null(q)){
      dt <- dt[, list("p"=mean(p), "z"=mean(z)), by=c("r","n")]
    }else{
      dt <- dt[, list("p"=mean(p), "z"=sum(z)), by=c("r","n")]
    }

    # print warning:
    if(settings$chatty){
      warning("Duplicated observations found and aggregated", call.=FALSE)
    }

  }


  if(!is.null(q)){
    # compute expenditure share weights for each region:
    dt[, "w" := (p*z)/sum(p*z, na.rm=TRUE), by="r"]
  }else{
    if(!is.null(w)){
      # normalize given weights:
      dt[, "w" := z/sum(z), by="r"]
    }else{
      dt[, "w" := z]
    }
  }

  # coerce regions and products to factor:
  dt[, c("r","n") := list(factor(r), factor(n))]
  # do not use "as.factor()" because this does not drop unused factor levels

  # return data:
  return(dt)

}

# END