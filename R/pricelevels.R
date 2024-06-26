# START

# Title:  Spatial price indices
# Author: Sebastian Weinand
# Date:   18 May 2024

# list available price indices:
list.indices <- function(){

  return(sort(pindices$name))

}

# spatial price indices:
pricelevels <- function(p, r, n, q=NULL, w=NULL, base=NULL, settings=list()){

  # set default if missing:
  if(missing(q)) q <- NULL
  if(missing(w)) w <- NULL

  # set default settings if missing:
  if(is.null(settings$chatty)) settings$chatty <- getOption("pricelevels.chatty")
  if(is.null(settings$connect)) settings$connect <- getOption("pricelevels.connect")
  if(is.null(settings$plot)) settings$plot <- getOption("pricelevels.plot")

  # store and then drop 'settings$type' if provided:
  if(!is.null(settings$type)){
    type <- tolower(settings$type)
    settings$type <- NULL
  }else{
    type <- NULL
  }

  # overwrite non-exported settings:
  if(is.null(settings$check.inputs)) settings$check.inputs <- getOption("pricelevels.check.inputs")
  if(is.null(settings$missings)) settings$missings <- getOption("pricelevels.missings")
  if(is.null(settings$duplicates)) settings$duplicates <- getOption("pricelevels.duplicates")
  if(is.null(settings$norm.weights)) settings$norm.weights <- TRUE # also for cpd() and nlcpd()

  # input checks:
  if(settings$check.inputs){

    # main input checks:
    check.num(x=p, int=c(0, Inf))
    check.char(x=r)
    check.char(x=n)
    check.num(x=w, null.ok=TRUE, int=c(0, Inf))
    check.num(x=q, null.ok=TRUE, int=c(0, Inf))
    check.char(x=base, miss.ok=TRUE, min.len=1, max.len=1, null.ok=TRUE, na.ok=FALSE)
    check.lengths(x=r, y=n)
    check.lengths(x=r, y=p)
    check.lengths(x=r, y=w)
    check.lengths(x=r, y=q)

    # check settings:
    check.log(x=settings$connect, min.len=1, max.len=1, na.ok=FALSE)
    check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
    check.char(x=type, min.len=1, max.len=Inf, null.ok=TRUE, na.ok=FALSE)

  }

  # allowed index types:
  if(is.null(q) && is.null(w)){
    type.vals <- pindices$name[pindices$uses_none==TRUE]
  }else{
    if(is.null(q)){
      type.vals <- pindices$name[(pindices$uses_none==TRUE & pindices$uses_q==FALSE) | pindices$uses_w==TRUE]
    }else{
      type.vals <- pindices$name
    }
  }

  # check against allowed index types:
  if(is.null(type)){
    type <- type.vals
  }else{
    type <- match.arg(arg=type, choices=type.vals, several.ok=TRUE)
  }

  # initialize data:
  pdata <- arrange(p=p, r=r, n=n, q=q, w=w, base=base, settings=settings)

  # set base region:
  base <- set.base(r=pdata$r, base=base, null.ok=FALSE, settings=settings)

  # store settings for plotting:
  makeplot <- settings$plot

  # overwrite settings to avoid double checking:
  settings$missings <- settings$duplicates <- settings$connect <- settings$check.inputs <- settings$plot <- FALSE

  # class of bilateral indices and geks indices:
  type.bil <- type[type%in%pindices$name[pindices$type=="bilateral"]]
  type.geks <- gsub(pattern="^geks-", replacement="", x=grep(pattern="^geks-", x=type, value=TRUE))

  # unweighted indices:
  if(is.null(q) && is.null(w)){

    Pout <- list(

      if(length(type.bil)>0){
       pdata[, bilateral.index(p=p, r=r, n=n, base=base, type=type.bil, settings=settings)]
      },

      "cpd"=if("cpd"%in%type){
        pdata[, cpd(p=p, r=r, n=n, base=base, settings=settings)]
      },

      "nlcpd"=if("nlcpd"%in%type){
        pdata[, nlcpd(p=p, r=r, n=n, base=base, settings=settings)]
      },

      if(length(type.geks)>0){
        pdata[, geks.main(p=p, r=r, n=n, base=base, settings=c(list("type"=type.geks), settings))]
      },

      "rao"=if("rao"%in%type){
        pdata[, rao(p=p, r=r, n=n, base=base, settings=settings)]
      },

      "rhajargasht"=if("rhajargasht"%in%type){
        pdata[, rhajargasht(p=p, r=r, n=n, base=base, settings=settings)]
      },

      "ikle"=if("ikle"%in%type){
        pdata[, ikle(p=p, r=r, n=n, base=base, settings=settings)]
      },

      "gkhamis"=if("gkhamis"%in%type){
        pdata[, gkhamis(p=p, r=r, n=n, base=base, settings=settings)]
      }

    )

  }else{

    # expenditure share weighted indices:
    if(is.null(q)){

      Pout <- list(

        if(length(type.bil)>0){
          pdata[, bilateral.index(p=p, r=r, n=n, w=w, base=base, type=type.bil, settings=settings)]
        },

        "cpd"=if("cpd"%in%type){
          pdata[, cpd(p=p, r=r, n=n, w=w, base=base, settings=settings)]
        },

        "nlcpd"=if("nlcpd"%in%type){
          pdata[, nlcpd(p=p, r=r, n=n, w=w, base=base, settings=settings)]
        },

        if(length(type.geks)>0){
          pdata[, geks.main(p=p, r=r, n=n, w=w, base=base, settings=c(list("type"=type.geks), settings))]
        },

        "rao"=if("rao"%in%type){
          pdata[, rao(p=p, r=r, n=n, w=w, base=base, settings=settings)]
        },

        "rhajargasht"=if("rhajargasht"%in%type){
          pdata[, rhajargasht(p=p, r=r, n=n, w=w, base=base, settings=settings)]
        },

        "ikle"=if("ikle"%in%type){
          pdata[, ikle(p=p, r=r, n=n, w=w, base=base, settings=settings)]
        },

        "gerardi"=if("gerardi"%in%type){
          pdata[, gerardi(p=p, r=r, n=n, w=w, base=base, settings=settings)]
        }

      )

    # quantity weighted indices:
    }else{

      Pout <- list(

        if(length(type.bil)>0){
          pdata[, bilateral.index(p=p, r=r, n=n, q=q, base=base, type=type.bil, settings=settings)]
        },

        "cpd"=if("cpd"%in%type){
          pdata[, cpd(p=p, r=r, n=n, q=q, base=base, settings=settings)]
        },

        "nlcpd"=if("nlcpd"%in%type){
          pdata[, nlcpd(p=p, r=r, n=n, q=q, base=base, settings=settings)]
        },

        if(length(type.geks)>0){
          pdata[, geks.main(p=p, r=r, n=n, q=q, base=base, settings=c(list("type"=type.geks), settings))]
        },

        "rao"=if("rao"%in%type){
          pdata[, rao(p=p, r=r, n=n, q=q, base=base, settings=settings)]
        },

        "rhajargasht"=if("rhajargasht"%in%type){
          pdata[, rhajargasht(p=p, r=r, n=n, q=q, base=base, settings=settings)]
        },

        "ikle"=if("ikle"%in%type){
          pdata[, ikle(p=p, r=r, n=n, q=q, base=base, settings=settings)]
        },

        "gerardi"=if("gerardi"%in%type){
          pdata[, gerardi(p=p, r=r, n=n, q=q, base=base, settings=settings)]
        },

        "gkhamis"=if("gkhamis"%in%type){
          pdata[, gkhamis(p=p, r=r, n=n, q=q, base=base, settings=settings)]
        }

      )

    }

  }

  # collect results and order alphabetically:
  out <- do.call("rbind", Pout)
  out <- out[order(rownames(out)), , drop=FALSE]

  # match to initial ordering:
  r.lvl <- levels(factor(r))
  out <- out[ ,match(x=r.lvl, table=colnames(out)), drop=FALSE]
  colnames(out) <- r.lvl

  if(makeplot){

    # compute price ratios:
    pdata[, "ratio":=ratios(p=p, r=r, n=n, base=base, static=TRUE, settings=list(chatty=FALSE))]
    pdata[, "region":=factor(r, levels=r.lvl)]
    plot.pricelevels(data=pdata, P=out)

  }

  # return output:
  return(out)

}

# END
