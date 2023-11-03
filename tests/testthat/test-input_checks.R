# START


# .check.char() -----------------------------------------------------------


expect_no_error(
  .check.char(x="test")
)

expect_no_error(
  .check.char(miss.ok=TRUE)
)

expect_no_error(
  .check.char(x=c("test", "abc"), min.len=1)
)

expect_no_error(
  .check.char(x=NULL, null.ok=TRUE)
)

expect_no_error(
  .check.char(x=NA_character_, na.ok=TRUE)
)

expect_error(
  .check.char(x=1)
)

expect_error(
  .check.char(miss.ok=FALSE)
)

expect_error(
  .check.char(x=c("test", "abc"), max.len=1)
)

expect_error(
  .check.char(x=NULL, null.ok=FALSE)
)

expect_error(
  .check.char(x=NA_character_, na.ok=FALSE)
)


# .check.log() ------------------------------------------------------------



expect_no_error(
  .check.log(x=TRUE)
)

expect_no_error(
  .check.log(miss.ok=TRUE)
)

expect_no_error(
  .check.log(x=c(TRUE, FALSE), min.len=1)
)

expect_no_error(
  .check.log(x=NULL, null.ok=TRUE)
)

expect_no_error(
  .check.log(x=NA, na.ok=TRUE)
)

expect_error(
  .check.log(x="test")
)

expect_error(
  .check.log(miss.ok=FALSE)
)

expect_error(
  .check.log(x=c(TRUE, FALSE), max.len=1)
)

expect_error(
  .check.log(x=NULL, null.ok=FALSE)
)

expect_error(
  .check.log(x=NA, na.ok=FALSE)
)


# .check.num() ------------------------------------------------------------


expect_no_error(
  .check.num(x=1)
)

expect_no_error(
  .check.num(miss.ok=TRUE)
)

expect_no_error(
  .check.num(x=1:10, min.len=1)
)

expect_no_error(
  .check.num(x=NULL, null.ok=TRUE)
)

expect_no_error(
  .check.num(x=NA_real_, na.ok=TRUE)
)

expect_no_error(
  .check.num(x=1:10, int=c(0,100))
)

expect_error(
  .check.num(x="test")
)

expect_error(
  .check.num(miss.ok=FALSE)
)

expect_error(
  .check.num(x=1:10, min.len=0, max.len=1)
)

expect_error(
  .check.num(x=NULL, null.ok=FALSE)
)

expect_error(
  .check.num(x=NA_real_, na.ok=FALSE)
)

expect_error(
  .check.num(x=1:10, int=c(0,5))
)


# .check.lenghts() --------------------------------------------------------


expect_no_error(
  .check.lengths(x=1:10, y=1:10)
)

expect_no_error(
  .check.lengths(x=1:10, y=NULL)
)

expect_no_error(
  .check.lengths(x=NULL, y=NULL)
)

expect_error(
  .check.lengths(x=1:10, y=1:8)
)


# .check.nlcpd_start() ----------------------------------------------------


set.seed(123)
r <- sample(letters[1:9])
n <- as.character(sample(1:5))

# wrong length:
expect_error(
  .check.nlcpd.start(x=list("abc"=1, "pi"=1:2, "delta"=1:2), r=r, n=n,
                     min.len=c("abc"=1,"pi"=2,"delta"=3))
)

# missing 'lnP':
expect_error(
  .check.nlcpd.start(x=list("pi"=1:2, "delta"=1:2), r=r, n=n,
                     min.len=c("lnP"=1,"pi"=2,"delta"=3))
)

# wrong lengths:
expect_error(
  .check.nlcpd.start(x=list("lnP"=1, "pi"=1:2, "delta"=1:2), r=r, n=n,
                     min.len=c("lnP"=1,"pi"=2,"delta"=3))
)

# no names:
expect_error(
  .check.nlcpd.start(x=list("lnP"=1, "pi"=1:2, "delta"=1:3), r=r, n=n,
                     min.len=c("lnP"=1,"pi"=2,"delta"=3))
)

# correct input:
pars <- list("lnP"=c("a"=1), "pi"=c("1"=1,"3"=2), "delta"=c("1"=1,"2"=1,"3"=1))
expect_no_error(
  .check.nlcpd.start(x=pars, r=r, n=n, min.len=c("lnP"=1,"pi"=2,"delta"=2))
)

# different order:
pars <- list("pi"=c("1"=1,"3"=2), "delta"=c("1"=1,"2"=1,"3"=1), "lnP"=c("a"=1))
expect_no_error(
  .check.nlcpd.start(x=pars, r=r, n=n, min.len=c("lnP"=1,"pi"=2,"delta"=2))
)

# END
