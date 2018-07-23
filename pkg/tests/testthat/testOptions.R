
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
  expect_warning(opt(fu=1))
})

test_that("Setting/resetting options with .list argument",{
  opt <- options_manager(.list=list(foo=1,bar=2))
  expect_equal(opt(foo=3),list(foo=3,bar=2))
  expect_equal(opt(),list(foo=3,bar=2))
  expect_equal(reset(opt), list(foo=1,bar=2))
  expect_equal(opt(),list(foo=1,bar=2))
  expect_warning(opt(fu=1))
})

test_that("Setting/resetting options with mixed .list argument",{
  opt <- options_manager(foo=1, .list=list(bar=2))
  expect_equal(opt(foo=3),list(foo=3,bar=2))
  expect_equal(opt(),list(foo=3,bar=2))
  expect_equal(reset(opt), list(foo=1,bar=2))
  expect_equal(opt(),list(foo=1,bar=2))
  expect_warning(opt(fu=1))
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

context("Range checks")

test_that("initial range checks", {
  expect_equal(options_manager(foo=1,.allowed=c(foo=inrange(0,1)))(),
               list(foo=1),
               info="Initial values that are valid succeed.")
  expect_error(options_manager(foo=2,.allowed=c(foo=inrange(0,1)))(),
               regex="Option value out of range. Allowed values are in [0, 1]",
               fixed=TRUE,
               info="Initial values that are invalid fail.")
  set_to_1 <- function(x) 1
  expect_equal(options_manager(foo=2,.allowed=c(foo=set_to_1))(),
               list(foo=1),
               info="Initial settings are modified, if needed.")
})

test_that("range checks",{
  expect_error(options_manager(foo=1,.allowed=c(x=inrange(0,1)))  )
  expect_error(options_manager(x=1,.allowed=list(x=inrange(2,3))))
  opt <- options_manager(foo=1,bar=0
    , .allowed=list(
        foo = inrange(min=0,max=1)
        , bar = inlist(0,1,2)
    )
  )
  expect_error(opt(foo=2))
  expect_error(opt(bar=3))
  
})

context("Custom checks")

test_that("custom checks can normalize options",{
  # Function that creates a category checker that is case-independent.
  my_checker <- function(x){
    if (!(tolower(x) %in% c("hey","ho","letsgo!")) ){
      stop("red alert!",call.=FALSE)
    } else {
      tolower(x)
    }
  }
  opts <- options_manager(foo="HEY", .allowed=list(foo=my_checker))
  
  expect_equal(opts(foo="HEY")$foo,"hey")
    
})

