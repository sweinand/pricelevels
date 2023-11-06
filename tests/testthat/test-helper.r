# START


# set.base() --------------------------------------------------------------


# regions:
r <- factor(c("a","b","b","c"))


expect_equal(
  set.base(r=r, base=NULL, null.ok=FALSE, chatty=FALSE),
  "b"
)

expect_equal(
  set.base(r=r, base="test", null.ok=FALSE, chatty=FALSE),
  "b"
)

expect_equal(
  set.base(r=r, base="a", null.ok=FALSE, chatty=FALSE),
  "a"
)

expect_equal(
  set.base(r=r, base=NULL, null.ok=TRUE, chatty=FALSE),
  NULL
)

expect_equal(
  set.base(r=r, base="test", null.ok=TRUE, chatty=FALSE),
  "b"
)

expect_equal(
  set.base(r=r, base="a", null.ok=TRUE, chatty=FALSE),
  "a"
)

# END
