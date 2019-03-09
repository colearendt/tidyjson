context("path")

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

test_that("throws an error on mixed input", {

  expect_error(path(a, "b"))

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