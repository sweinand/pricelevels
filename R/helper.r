# START

# Title:  General helper functions
# Author: Sebastian Weinand
# Date:   6 November 2023

# set base region:
set.base <- function(r, base, null.ok, chatty){

  # @args:
  # r         factor, vector of regions
  # base      character, one base region
  # null.ok   logical, base=NULL allowed or not
  # chatty    logical, print warning or not

  # derive base region if required and base=NULL:
  if(is.null(base) && !null.ok){
    base <- names(which.max(table(r)))[1]
    if(chatty){
      warning(paste0("Base region set to base='", base, "'"), call.=FALSE)
    }
  }

  # derive base region if not found in regions and base not NULL:
  if(!base%in%levels(r) && !is.null(base)){
    base <- names(which.max(table(r)))[1]
    if(chatty){
      warning(paste0("Base region not found -> reset to base='", base, "'"), call.=FALSE)
    }
  }

  # return output:
  return(base)

}

# END
