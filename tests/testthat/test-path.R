test_that("works with unquoted input", {

  expect_identical(
    path(a, b, c),
    structure(c("a", "b", "c"), class = "path")
  )

})

test_that("works with quoted input", {

  expect_identical(
    path("a", "b", "c"),
    structure(c("a", "b", "c"), class = "path")
  )

})

test_that("works with quasiquotation", {
  testfunc <- function(b) {
    path(!!b)
  }
  testfunc2 <- function(b) {
    path(!!!b)
  }
  
  expect_identical(
    testfunc("a"),
    structure(c("a"), class = "path")
  )
  
  expect_identical(
    testfunc2(c("a", "b")),
    structure(c("a","b"), class = "path")
  )
})

test_that("works with data masking", {
  testfunc <- function(a) {
    path({{a}})
  }
  
  expect_identical(
    testfunc("chicken"),
    structure(c("chicken"), class = "path")
  )
})

test_that("throws an error on length > 1 input", {

  expect_error(path(c(a, b)))
  expect_error(path(list(a, b)))
  expect_error(path(c("a", "b")))
  expect_error(path(list("a", "b")))

})


test_that("works with a vector input", {
  skip('Vector input not yet supported in path')
  
  v <- c('a','b','c')
  
  expect_identical(path(v)
                   , structure(c('a','b','c'),class='path')
                   )
})
