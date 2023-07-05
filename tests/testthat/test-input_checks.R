# START


# .check.char() -----------------------------------------------------------


expect_no_error(
  spindex:::.check.char(x="test")
)

expect_no_error(
  spindex:::.check.char(miss.ok=TRUE)
)

expect_no_error(
  spindex:::.check.char(x=c("test", "abc"), min.len=1)
)

expect_no_error(
  spindex:::.check.char(x=NULL, null.ok=TRUE)
)

expect_no_error(
  spindex:::.check.char(x=NA_character_, na.ok=TRUE)
)

expect_error(
  spindex:::.check.char(x=1)
)

expect_error(
  spindex:::.check.char(miss.ok=FALSE)
)

expect_error(
  spindex:::.check.char(x=c("test", "abc"), max.len=1)
)

expect_error(
  spindex:::.check.char(x=NULL, null.ok=FALSE)
)

expect_error(
  spindex:::.check.char(x=NA_character_, na.ok=FALSE)
)


# .check.log() ------------------------------------------------------------



expect_no_error(
  spindex:::.check.log(x=TRUE)
)

expect_no_error(
  spindex:::.check.log(miss.ok=TRUE)
)

expect_no_error(
  spindex:::.check.log(x=c(TRUE, FALSE), min.len=1)
)

expect_no_error(
  spindex:::.check.log(x=NULL, null.ok=TRUE)
)

expect_no_error(
  spindex:::.check.log(x=NA, na.ok=TRUE)
)

expect_error(
  spindex:::.check.log(x="test")
)

expect_error(
  spindex:::.check.log(miss.ok=FALSE)
)

expect_error(
  spindex:::.check.log(x=c(TRUE, FALSE), max.len=1)
)

expect_error(
  spindex:::.check.log(x=NULL, null.ok=FALSE)
)

expect_error(
  spindex:::.check.log(x=NA, na.ok=FALSE)
)


# .check.num() ------------------------------------------------------------


expect_no_error(
  spindex:::.check.num(x=1)
)

expect_no_error(
  spindex:::.check.num(miss.ok=TRUE)
)

expect_no_error(
  spindex:::.check.num(x=1:10, min.len=1)
)

expect_no_error(
  spindex:::.check.num(x=NULL, null.ok=TRUE)
)

expect_no_error(
  spindex:::.check.num(x=NA_real_, na.ok=TRUE)
)

expect_no_error(
  spindex:::.check.num(x=1:10, int=c(0,100))
)

expect_error(
  spindex:::.check.num(x="test")
)

expect_error(
  spindex:::.check.num(miss.ok=FALSE)
)

expect_error(
  spindex:::.check.num(x=1:10, min.len=0, max.len=1)
)

expect_error(
  spindex:::.check.num(x=NULL, null.ok=FALSE)
)

expect_error(
  spindex:::.check.num(x=NA_real_, na.ok=FALSE)
)

expect_error(
  spindex:::.check.num(x=1:10, int=c(0,5))
)


# .check.lenghts() --------------------------------------------------------


expect_no_error(
  spindex:::.check.lengths(x=1:10, y=1:10)
)

expect_no_error(
  spindex:::.check.lengths(x=1:10, y=NULL)
)

expect_no_error(
  spindex:::.check.lengths(x=NULL, y=NULL)
)

expect_error(
  spindex:::.check.lengths(x=1:10, y=1:8)
)


# .check.nlcpd_start() ----------------------------------------------------


expect_no_error(
  spindex:::.check.nlcpd.start(x=list("lnP"=1, "pi"=c(1,2), "delta"=1:3), len=c(1,2,3))
)

expect_error(
  spindex:::.check.nlcpd.start(x=list("lnP"=1, "pi"=c(1,2), "delta"=1:3), len=c(1,2,2))
)

expect_error(
  spindex:::.check.nlcpd.start(x=list("pi"=c(1,2), "delta"=1:3), len=c(1,2,2))
)

expect_error(
  spindex:::.check.nlcpd.start(x=list("lnP"="abc", "pi"=c(1,2), "delta"=1:3), len=c(1,2,2))
)

expect_error(
  spindex:::.check.nlcpd.start(x=list("bla"=1, "pi"=c(1,2), "delta"=1:3), len=c(1,2,2))
)

# END
