
context("setting and getting global options")

test_that("Retrieving options",{
  opt <- register_options("test-1",default=list(foo=1,bar=2))
  expect_equal(opt(.simplify=FALSE),list(foo=1,bar=2))
  expect_equal(opt(),c(foo=1,bar=2))
  expect_equal(opt('foo'),1)
  expect_equal(opt('foo','bar'),c(1,2))
  expect_equal(opt('foo','bar',.simplify=FALSE),list(foo=1,bar=2))
  clear_optionregister()
})

test_that("Setting/resetting options",{
  opt <- register_options("test-2",default=list(foo=1,bar=2))
  expect_equal(opt(foo=3),list(foo=3,bar=2))
  expect_equal(opt(.simplify=FALSE),list(foo=3,bar=2))
  expect_equal(opt(.reset=TRUE), list(foo=1,bar=2))
  expect_equal(opt(.simplify=FALSE),list(foo=1,bar=2))
  clear_optionregister()
})

context("Local options")

test_that("Cloning options",{
  opt <- register_options("test-3",default=list(foo=1,bar=2))
  op2 <- opt(.clone=TRUE)
  expect_equal(opt(.simplify=FALSE),list(foo=1,bar=2))
  expect_equal(opt(foo=3, .where=op2),list(foo=3,bar=2))
  expect_equal(opt(.simplify=FALSE),list(foo=1,bar=2))
  expect_equal(opt(.where=op2),c(foo=3,bar=2))
  clear_optionregister()
})

test_that("Referencing options",{
  opt <- register_options("test-4",default=list(foo=1,bar=2))
  expect_identical(opt(.ref=TRUE),OPTIONREGISTER[["test-4"]])  
  clear_optionregister()
})



