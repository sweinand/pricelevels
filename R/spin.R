# START

# Title:  Spatial price indices
# Author: Sebastian Weinand
# Date:   6 November 2023

# spatial price indices:
spin <- function(p, r, n, q=NULL, w=NULL, base=NULL, settings=list()){

  # set default if missing:
  if(missing(q)) q <- NULL
  if(missing(w)) w <- NULL

  # set default settings if missing:
  if(is.null(settings$connect)) settings$connect <- TRUE
  if(is.null(settings$chatty)) settings$chatty <- TRUE

  # store and then drop 'settings$type' if provided:
  if(!is.null(settings$type)){
    type <- tolower(settings$type)
    settings$type <- NULL
  }else{
    type <- NULL
  }

  # overwrite non-exported settings:
  settings$missings <- TRUE
  settings$duplicates <- TRUE
  settings$norm.weights <- TRUE # also for cpd() and nlcpd()

  # main input checks:
  .check.num(x=p, int=c(0, Inf))
  .check.char(x=r)
  .check.char(x=n)
  .check.num(x=w, null.ok=TRUE, int=c(0, Inf))
  .check.num(x=q, null.ok=TRUE, int=c(0, Inf))
  .check.char(x=base, miss.ok=TRUE, min.len=1, max.len=1, null.ok=TRUE, na.ok=FALSE)
  .check.lengths(x=r, y=n)
  .check.lengths(x=r, y=p)
  .check.lengths(x=r, y=w)
  .check.lengths(x=r, y=q)

  # check settings:
  .check.log(x=settings$connect, min.len=1, max.len=1, na.ok=FALSE)
  .check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
  .check.char(x=type, min.len=1, max.len=Inf, null.ok=TRUE, na.ok=FALSE)

  # allowed index types:
  type.vals <- c("jevons","carli","harmonic","dutot","laspey","paasche",
                 "fisher","walsh","toernq")
  type.vals <- c(type.vals, paste("geks", type.vals, sep="-"), "cpd", "nlcpd",
                 "rao","gerardi","geary-khamis","idb")

  # check against allowed index types:
  if(is.null(type)){
    type <- type.vals
  }else{
    type <- match.arg(arg=type, choices=type.vals, several.ok=TRUE)
  }

  # initialize data:
  pdata <- spin:::arrange(p=p, r=r, n=n, q=q, w=w, base=base, settings=settings)

  # set base region:
  base <- spin:::set.base(r=pdata$r, base=base, null.ok=FALSE, settings=settings)

  # overwrite settings to avoid double checking:
  settings$missings <- settings$duplicates <- settings$connect <- FALSE

  # indices without quantities or weights:
  Punw <- list(
    "jevons"=if("jevons"%in%type){
      pdata[, spin::jevons(p=p, r=r, n=n, base=base, settings=settings)]
    },

    "carli"=if("carli"%in%type){
      pdata[, spin::carli(p=p, r=r, n=n, base=base, settings=settings)]
    },

    "harmonic"=if("harmonic"%in%type){
      pdata[, spin::harmonic(p=p, r=r, n=n, base=base, settings=settings)]
    },

    "dutot"=if("dutot"%in%type){
      pdata[, spin::dutot(p=p, r=r, n=n, base=base, settings=settings)]
    },

    "geks-jevons"=if("geks-jevons"%in%type){
      pdata[, spin::geks(p=p, r=r, n=n, base=base, settings=c(settings, type="jevons"))]
    },

    "geks-carli"=if("geks-carli"%in%type){
      pdata[, spin::geks(p=p, r=r, n=n, base=base, settings=c(settings, type="carli"))]
    },

    "geks-harmonic"=if("geks-harmonic"%in%type){
      pdata[, spin::geks(p=p, r=r, n=n, base=base, settings=c(settings, type="harmonic"))]
    },

    "geks-dutot"=if("geks-dutot"%in%type){
      pdata[, spin::geks(p=p, r=r, n=n, base=base, settings=c(settings, type="dutot"))]
    }
  )

  # indices with quantities:
  if(!is.null(q)){

    # weighted indices:
    Pw <- list(
      "laspey"=if("laspey"%in%type){
        pdata[, spin::laspey(p=p, r=r, n=n, q=z, base=base, settings=settings)]
      },

      "paasche"=if("paasche"%in%type){
        pdata[, spin::paasche(p=p, r=r, n=n, q=z, base=base, settings=settings)]
      },

      "fisher"=if("fisher"%in%type){
        pdata[, spin::fisher(p=p, r=r, n=n, q=z, base=base, settings=settings)]
      },

      "walsh"=if("walsh"%in%type){
        pdata[, spin::walsh(p=p, r=r, n=n, q=z, base=base, settings=settings)]
      },

      "toernq"=if("toernq"%in%type){
        pdata[, spin::toernq(p=p, r=r, n=n, q=z, base=base, settings=settings)]
      },

      "cpd"=if("cpd"%in%type){
        pdata[, spin::cpd(p=p, r=r, n=n, q=z, base=base, settings=settings)]
      },

      "nlcpd"=if("nlcpd"%in%type){
        pdata[, spin::nlcpd(p=p, r=r, n=n, q=z, base=base, settings=settings)]
      },

      "geks-laspey"=if("geks-laspey"%in%type){
        pdata[, spin::geks(p=p, r=r, n=n, q=z, base=base, settings=c(settings, type="laspey"))]
      },

      "geks-paasche"=if("geks-paasche"%in%type){
        pdata[, spin::geks(p=p, r=r, n=n, q=z, base=base, settings=c(settings, type="paasche"))]
      },

      "geks-fisher"=if("geks-fisher"%in%type){
        pdata[, spin::geks(p=p, r=r, n=n, q=z, base=base, settings=c(settings, type="fisher"))]
      },

      "geks-walsh"=if("geks-walsh"%in%type){
        pdata[, spin::geks(p=p, r=r, n=n, q=z, base=base, settings=c(settings, type="walsh"))]
      },

      "geks-toernq"=if("geks-toernq"%in%type){
        pdata[, spin::geks(p=p, r=r, n=n, q=z, base=base, settings=c(settings, type="toernq"))]
      },

      "geary-khamis"=if("geary-khamis"%in%type){
        pdata[, spin::gk(p=p, r=r, n=n, q=z, base=base, settings=settings)]
      },

      "rao"=if("rao"%in%type){
        pdata[, spin::rao(p=p, r=r, n=n, q=z, base=base, settings=settings)]
      },

      "idb"=if("idb"%in%type){
        pdata[, spin::idb(p=p, r=r, n=n, q=z, base=base, settings=settings)]
      },

      "gerardi"=if("gerardi"%in%type){
        pdata[, spin::gerardi(p=p, r=r, n=n, q=z, base=base, settings=c(settings, method="iterative"))]
      }

    )

  }else{

    if(is.null(w)){

      # no weighted indices but add unweighted cpd and nlcpd:
      Pw <- NULL

      if("cpd"%in%type){
        Punw$cpd <- pdata[, spin::cpd(p=p, r=r, n=n, base=base, settings=settings)]
      }

      if("nlcpd"%in%type){
        Punw$nlcpd <- pdata[, spin::nlcpd(p=p, r=r, n=n, base=base, settings=settings)]
      }

    }else{

      # weighted indices:
      Pw <- list(
        "laspey"=if("laspey"%in%type){
          pdata[, spin::laspey(p=p, r=r, n=n, w=w, base=base, settings=settings)]
        },

        "paasche"=if("paasche"%in%type){
          pdata[, spin::paasche(p=p, r=r, n=n, w=w, base=base, settings=settings)]
        },

        "fisher"=if("fisher"%in%type){
          pdata[, spin::fisher(p=p, r=r, n=n, w=w, base=base, settings=settings)]
        },

        "walsh"=if("walsh"%in%type){
          pdata[, spin::walsh(p=p, r=r, n=n, w=w, base=base, settings=settings)]
        },

        "toernq"=if("toernq"%in%type){
          pdata[, spin::toernq(p=p, r=r, n=n, w=w, base=base, settings=settings)]
        },

        "cpd"=if("cpd"%in%type){
          pdata[, spin::cpd(p=p, r=r, n=n, w=w, base=base, settings=settings)]
        },

        "nlcpd"=if("nlcpd"%in%type){
          pdata[, spin::nlcpd(p=p, r=r, n=n, w=w, base=base, settings=settings)]
        },

        "geks-laspey"=if("geks-laspey"%in%type){
          pdata[, spin::geks(p=p, r=r, n=n, w=w, base=base, settings=c(settings, type="laspey"))]
        },

        "geks-paasche"=if("geks-paasche"%in%type){
          pdata[, spin::geks(p=p, r=r, n=n, w=w, base=base, settings=c(settings, type="paasche"))]
        },

        "geks-fisher"=if("geks-fisher"%in%type){
          pdata[, spin::geks(p=p, r=r, n=n, w=w, base=base, settings=c(settings, type="fisher"))]
        },

        "geks-walsh"=if("geks-walsh"%in%type){
          pdata[, spin::geks(p=p, r=r, n=n, w=w, base=base, settings=c(settings, type="walsh"))]
        },

        "geks-toernq"=if("geks-toernq"%in%type){
          pdata[, spin::geks(p=p, r=r, n=n, w=w, base=base, settings=c(settings, type="toernq"))]
        },

        "geary-khamis"=if("geary-khamis"%in%type){
          NA_real_
        },

        "rao"=if("rao"%in%type){
          pdata[, spin::rao(p=p, r=r, n=n, w=w, base=base, settings=settings)]
        },

        "idb"=if("idb"%in%type){
          pdata[, spin::idb(p=p, r=r, n=n, w=w, base=base, settings=settings)]
        },

        "gerardi"=if("gerardi"%in%type){
          pdata[, spin::gerardi(p=p, r=r, n=n, w=w, base=base, settings=c(settings, method="iterative"))]
        }

      )

    }

  }

  # collect results and order alphabetically:
  out <- do.call("rbind", c(Punw, Pw))
  out <- out[order(rownames(out)), , drop=FALSE]

  # match to initial ordering:
  r.lvl <- levels(factor(r))
  out <- out[ ,match(x=r.lvl, table=colnames(out)), drop=FALSE]
  colnames(out) <- r.lvl

  # return output:
  return(out)

}

# END
