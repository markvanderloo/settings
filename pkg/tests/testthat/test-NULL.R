context("NULL handling")

test_that("NULL handling", {
  opt <- options_manager(foo=1,bar=2)

  expect_error(opt("baz"),
               regex="Cannot find option name(s): baz",
               fixed=TRUE,
               info="Options that do not exist give an error.")
  
  opt <- options_manager(foo=1,bar=NULL)
  expect_null(opt("bar"),
              info="Initially setting an option to NULL works (individual option output)")
  expect_equal(opt(),
               list(foo=1, bar=NULL),
               info="Initially setting an option to NULL works (all options output)")

  opt <- options_manager(foo=1,bar=NULL)
  opt(foo=NULL)
  expect_null(opt("foo"),
              info="Setting an option to NULL after the initial setup works (individual option output).")
  expect_equal(opt(),
               list(foo=NULL, bar=NULL),
               info="Setting an option to NULL after the initial setup works (all options output).")
})
