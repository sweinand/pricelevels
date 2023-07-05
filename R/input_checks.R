# START

# Title:  Input checks
# Author: Sebastian Weinand
# Date:   2023-07-04

# check character and factor inputs:
.check.char <- function(x, min.len=1, max.len=Inf, miss.ok=FALSE, null.ok=FALSE, na.ok=TRUE){
  
  input <- deparse(substitute(x))
  msg_prefix <- paste("Non-valid input for", input, "->")
  if(abs(max.len-min.len)<1e-6) one.len <- TRUE else one.len <- FALSE
  
  if(missing(x) & !miss.ok) stop(paste(msg_prefix, "missing input"), call. = FALSE)
  if(missing(x) & miss.ok) return(invisible())
  
  if(!(null.ok && is.null(x))){
    if(!(is.vector(x=x, mode="character") | is.factor(x=x))) stop(paste(msg_prefix, "only character vectors or factors allowed"), call. = FALSE)
    if(one.len & length(x)!=max.len) stop(paste(msg_prefix, "must contain exactly one element"), call. = FALSE)
    if(length(x)<min.len) stop(paste(msg_prefix, "length must be greater than", min.len, "element(s)"), call. = FALSE)
    if(length(x)>max.len) stop(paste(msg_prefix, "length must be smaller than", max.len, "element(s)"), call. = FALSE)
    if(any(is.na(x)) & !na.ok) stop(paste(msg_prefix, "NAs not allowed"), call.=FALSE)
  }
  
}

# check logical inputs:
.check.log <- function(x, min.len=1, max.len=Inf, miss.ok=FALSE, null.ok=FALSE, na.ok=TRUE){
  
  input <- deparse(substitute(x))
  msg_prefix <- paste("Non-valid input for", input, "->")
  if(abs(max.len-min.len)<1e-6) one.len <- TRUE else one.len <- FALSE
  
  if(missing(x) & !miss.ok) stop(paste(msg_prefix, "missing input"), call. = FALSE)
  if(missing(x) & miss.ok) return(invisible())
  
  if(!(null.ok && is.null(x))){
    if(!is.vector(x=x, mode = "logical")) stop(paste(msg_prefix, "only logical allowed"), call. = FALSE)
    if(one.len & length(x)!=max.len) stop(paste(msg_prefix, "must contain exactly one element"), call. = FALSE)
    if(length(x)<min.len) stop(paste(msg_prefix, "length must be greater than", min.len, "element(s)"), call. = FALSE)
    if(length(x)>max.len) stop(paste(msg_prefix, "length must be smaller than", max.len, "element(s)"), call. = FALSE)
    if(any(is.na(x)) & !na.ok) stop(paste(msg_prefix, "NAs not allowed"), call.=FALSE)
    if(!all(x%in%c(NA, TRUE, FALSE))) stop(paste(msg_prefix, "only TRUE and FALSE allowed"), call. = FALSE)
  }
  
}

# check numeric inputs:
.check.num <- function(x, min.len=1, max.len=Inf, miss.ok=FALSE, null.ok=FALSE, na.ok=TRUE, int=c(-Inf, Inf)){
  
  input <- deparse(substitute(x))
  msg_prefix <- paste("Non-valid input for", input, "->")
  if(abs(max.len-min.len)<1e-6) one.len <- TRUE else one.len <- FALSE
  
  if(missing(x) & !miss.ok) stop(paste(msg_prefix, "missing input"), call. = FALSE)
  if(missing(x) & miss.ok) return(invisible())
  
  if(!(null.ok && is.null(x))){
    if(!is.vector(x=x, mode="numeric")) stop(paste(msg_prefix, "only numeric vectors allowed"), call. = FALSE)
    if(one.len & length(x)!=max.len) stop(paste(msg_prefix, "must contain exactly one element"), call. = FALSE)
    if(length(x)<min.len) stop(paste(msg_prefix, "length must be greater than", min.len, "element(s)"), call. = FALSE)
    if(length(x)>max.len) stop(paste(msg_prefix, "length must be smaller than", max.len, "element(s)"), call. = FALSE)
    if(any(is.na(x)) & !na.ok) stop(paste(msg_prefix, "NAs not allowed"), call.=FALSE)
    if(any(x <= int[1] | x >= int[2], na.rm = TRUE)) stop(paste(msg_prefix, "only values between", paste(int, collapse = " and "), "allowed"), call. = FALSE)
  }
  
}

# # check input for base level:
# .check_base <- function(x, allow_null = TRUE){
# 
#   input <- deparse(substitute(x))
#   msg_prefix <- paste("Non-valid input for", input, "->")
#   if(!(allow_null && is.null(x))){
#     if(!is.vector(x = x, mode = "character")){stop(paste(msg_prefix, "only characters allowed"), call. = FALSE)}
#     if(length(x) != 1L){stop(paste(msg_prefix, "must contain exactly one element"), call. = FALSE)}
#     if(is.na(x)){stop(paste(msg_prefix, "NA not allowed"), call. = FALSE)}
#   }
# 
# }
# 
# # check type:
# .check_type <- function(x){
# 
#   input <- deparse(substitute(x))
#   msg_prefix <- paste("Non-valid input for", input, "->")
#   if(missing(x)) stop(paste(msg_prefix, "missing input"), call. = FALSE)
#   if(!is.vector(x = x, mode = "character")) stop(paste(msg_prefix, "only characters allowed"), call. = FALSE)
#   if(length(x) != 1L) stop(paste(msg_prefix, "must contain exactly one element"), call. = FALSE)
# 
# }

# check identical length of input vectors:
.check.lengths <- function(x, y){

  input_x <- deparse(substitute(x))
  input_y <- deparse(substitute(y))
  msg_prefix <- paste("Non-valid input for", input_x, "or", input_y, "->")
  
  if(!is.null(x) && !is.null(y)){
    if(length(x) != length(y)) stop(paste(msg_prefix, "vectors must be of equal length"), call. = FALSE)
  }

}

# check start values in nlcpd():
.check.nlcpd.start <- function(x, len=NULL){
  
  input <- deparse(substitute(x))
  msg_prefix <- paste("Non-valid input for", input, "->")
  
  if(!is.list(x)) stop(paste(msg_prefix, "must be a list"), call. = FALSE)
  if(length(x) != 3L) stop(paste(msg_prefix, "must be of length 3L"), call. = FALSE)
  if(!all(c("lnP","pi","delta") %in% names(x))) stop(paste(msg_prefix, "names must be 'lnP', 'pi', 'delta'"), call. = FALSE)
  
  if(length(x$lnP)>0) .check.num(x=x$lnP, min.len=0, max.len=Inf, miss.ok=FALSE, null.ok=FALSE, na.ok=FALSE, int=c(-Inf,Inf))
  if(length(x$pi)>0) .check.num(x=x$pi, min.len=0, max.len=Inf, miss.ok=FALSE, null.ok=FALSE, na.ok=FALSE, int=c(-Inf,Inf))
  if(length(x$delta)>0) .check.num(x=x$delta, min.len=0, max.len=Inf, miss.ok=FALSE, null.ok=FALSE, na.ok=FALSE, int=c(-Inf,Inf))
  
  if(length(x$lnP) != len[1]) stop(paste(msg_prefix, "'lnP' must be of length", len[1]), call. = FALSE)
  if(length(x$pi) != len[2]) stop(paste(msg_prefix, "'pi' must be of length", len[2]), call. = FALSE)
  if(length(x$delta) != len[3]) stop(paste(msg_prefix, "'delta' must be of length", len[3]), call. = FALSE)
  
}


# END
