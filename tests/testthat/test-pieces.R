
context("Internal utilities")

test_that("repeat_string", {

  cases <- list(
    list("rep", 10, "repreprepr"),
    list("rep", 1 , "r"),
    list("rep", 3 , "rep"),
    list("rep", 0 , ""),
    list("rep", c(1,2), c("r", "re")),
    list("rep", c(0,5), c("", "repre"))
  )

  for (c in cases) expect_equal(repeat_string(c[[1]], c[[2]]), c[[3]])

})

test_that("pad_right", {

  cases <- list(
    list("foo", 5, " ", "foo  "),
    list("foo", 3, " ", "foo"),
    list("", 3, " ", "   "),
    list("", 0, " ", ""),
    list("foobar", 10, "-+", "foobar-+-+"),
    list("foobar",  9, "-+", "foobar-+-"),
    list(c("foo", "foobar"), 10, " ", c("foo       ", "foobar    ")),
    list(c("foo", "foobar"), 6, " ", c("foo   ", "foobar"))
  )

  for (c in cases) expect_equal(pad_right(c[[1]], c[[2]], c[[3]]), c[[4]])

})

test_that("pad_left", {

  cases <- list(
    list("foo", 5, " ", "  foo"),
    list("foo", 3, " ", "foo"),
    list("", 3, " ", "   "),
    list("", 0, " ", ""),
    list("foobar", 10, "-+", "-+-+foobar"),
    list("foobar",  9, "-+", "-+-foobar"),
    list(c("foo", "foobar"), 10, " ", c("       foo", "    foobar")),
    list(c("foo", "foobar"), 6, " ", c("   foo", "foobar"))
  )

  for (c in cases) expect_equal(pad_left(c[[1]], c[[2]], c[[3]]), c[[4]])

})

test_that("pad_center", {

  cases <- list(
    list("foo", 5, " ", " foo "),
    list("foo", 3, " ", "foo"),
    list("", 3, " ", "   "),
    list("", 0, " ", ""),
    list("foobar", 10, "-+", "-+foobar-+"),
    list("foobar",  9, "-+", "-foobar-+"),
    list(c("foo", "foobar"), 10, " ", c("   foo    ", "  foobar  ")),
    list(c("foo", "foobar"), 6, " ", c(" foo  ", "foobar"))
  )

  for (c in cases) expect_equal(pad_center(c[[1]], c[[2]], c[[3]]), c[[4]])

})

test_that("split_long_words", {

  cases <- list(
    list("thisistoolongforone", 6, "-", "this- isto- olon- gfor- one"),
    list("a b ab thisislong no not", 6, "-", "a b ab this- islo- ng no not"),
    list(c("onelong", "twolong"), 5, "-", c("one- lon- g", "two- lon- g")),
    list(c("a b longi c d", "xx y longo rt rt"), 5, "-",
         c("a b lon- gi c d", "xx y lon- go rt rt"))
  )

  for (c in cases) expect_equal(split_long_words(c[[1]], c[[2]], c[[3]]), c[[4]])

})

test_that("split_into_lines", {

  cases <- list(
    list("lorem ipsum foo foobar bar bar", 10,
         list(c("lorem", "ipsum foo", "foobar bar", "bar"))),
    list(c("foo bar foobar", "foobar", "foo bar"), 7,
         list(c("foo bar", "foobar"), "foobar", "foo bar"))
  )

  for (c in cases) expect_equal(split_into_lines(c[[1]], c[[2]]), c[[3]])

})
