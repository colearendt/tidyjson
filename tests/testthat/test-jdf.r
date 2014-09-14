context("jdf: as.jdf.character")

test_that("correctly parses length(json) == 1", {
  expect_identical(
    as.jdf('{"name": "bob", "age": 32}'),
    jdf(
      data.frame(document.id = 1L),
      list(list(name = "bob", age = 32))
    )
  )
})

test_that("correctly parses length(json) > 1", {
  expect_identical(
    as.jdf(
      c('{"name": "bob", "age": 32}',
        '{"name": "susan", "age": 25}')
    ),
    jdf(
      data.frame(document.id = 1L:2L),
      list(
        list(name = "bob", age = 32),
        list(name = "susan", age = 25)
      )
    )
  )
})

test_that("currectly parses length(json) == 0", {
  expect_identical(
    as.jdf(character(0)),
    jdf(
      data.frame(document.id = integer(0)),
      list()
    )
  )
})

test_that("currectly structures an array", {
  expect_identical(
    as.jdf('[{"name": "bob"}, {"name": "susan"}]'),
    jdf(
      data.frame(document.id = 1L),
      list(list(list(name = "bob"), list(name = "susan")))
    )
  )
})