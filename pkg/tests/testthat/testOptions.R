
context("setting and getting global options")

test_that("Retrieving options",{
  opt <- options_manager(foo=1,bar=2)
  expect_equal(opt(),list(foo=1,bar=2))
  expect_equal(opt('foo'),1)
  expect_equal(opt('foo','bar'),list(foo=1,bar=2))
})

test_that("Setting/resetting options",{
  opt <- options_manager(foo=1,bar=2)
  expect_equal(opt(foo=3),list(foo=3,bar=2))
  expect_equal(opt(),list(foo=3,bar=2))
  expect_equal(reset(opt), list(foo=1,bar=2))
  expect_equal(opt(),list(foo=1,bar=2))
})

context("Local options")

test_that("Cloning options",{
  opt <- options_manager(foo=1,bar=2)
  op2 <- clone_and_merge(opt,foo=2)
  expect_equal(op2(),list(foo=2,bar=2))
  expect_equal(opt(),list(foo=1,bar=2))
  reset(op2)
  expect_equal(op2(),opt())
})


context("Utilities")
test_that("is_setting",{
  expect_equal(is_setting(foo=1),TRUE)
  expect_true(is_setting(foo=1,bar=2))
  expect_false(is_setting())
  expect_false(is_setting('x'))
  expect_error(is_setting('x',foo=3))
})

