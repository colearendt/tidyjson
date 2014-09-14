context("jdf: as.jdf.character")

test_that("correctly parses length(json) == 1", {
  expect_identical(
    as.jdf('{"name": "bob", "age": 32}'),
    structure(
      data.frame(document.id = 1L),
      JSON = list(list(name = "bob", age = 32)),
      class = c("jdf", "data.frame")
    )
  )
})

test_that("correctly parses length(json) > 1", {
  expect_identical(
    as.jdf(
      c('{"name": "bob", "age": 32}',
        '{"name": "susan", "age": 25}')
    ),
    structure(
      data.frame(document.id = 1L:2L),
      JSON = list(
        list(name = "bob", age = 32),
        list(name = "susan", age = 25)
      ),
      class = c("jdf", "data.frame")
    )
  )
})

test_that("currectly parses length(json) == 0", {
  expect_identical(
    as.jdf(character(0)),
    structure(
      data.frame(document.id = integer(0)),
      JSON = list(),
      class = c("jdf", "data.frame")
    )
  )
})

test_that("currectly structures an array", {
  expect_identical(
    as.jdf('[{"name": "bob"}, {"name": "susan"}]'),
    structure(
      data.frame(document.id = 1L),
      JSON = list(list(list(name = "bob"), list(name = "susan"))),
      class = c("jdf", "data.frame")
    )
  )
})