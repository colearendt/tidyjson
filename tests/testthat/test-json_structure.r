context("json_structure")

test_that("simple string works", {

  expect_identical(
    '"a"' %>% json_structure,
    tbl_json(
      data_frame(
        document.id = 1L,
        parent.id = NA_character_,
        level = 1L,
        index = 1L,
        child.id = "1",
        key = NA_character_,
        type = "string" %>% factor(levels = allowed_json_types),
        length = 1L
      ),
      list("a")
    )
  )

})

test_that("simple object works", {

  expect_identical(
    '{"key": "value"}' %>% json_structure,
    tbl_json(
      data_frame(
        document.id = c(1L, 1L),
        parent.id = c(NA_character_, "1"),
        level = c(1L, 2L),
        index = c(1L, 1L),
        child.id = c("1", "1.1"),
        key = c(NA_character_, "key"),
        type = c("object", "string") %>% factor(levels = allowed_json_types),
        length = c(1L, 1L)
      ),
      list(list("key" = "value"), "value")
    )
  )

})

test_that("simple array works", {

  expect_identical(
    '[1, 2]' %>% json_structure,
    tbl_json(
      data_frame(
        document.id = c(1L, 1L, 1L),
        parent.id = c(NA_character_, "1", "1"),
        level = c(1L, 2L, 2L),
        index = c(1L, 1L, 2L),
        child.id = c("1", "1.1", "1.2"),
        key = rep(NA_character_, 3),
        type = c("array", "number", "number") %>% factor(levels = allowed_json_types),
        length = c(2L, 1L, 1L)
      ),
      list(list(1L, 2L), 1L, 2L)
    )
  )

})

test_that("nested object works", {

  expect_identical(
    '{"k1": {"k2": "value"}}' %>% json_structure,
    tbl_json(
      data_frame(
        document.id = c(1L, 1L, 1L),
        parent.id = c(NA_character_, "1", "1.1"),
        level = c(1L, 2L, 3L),
        index = c(1L, 1L, 1L),
        child.id = c("1", "1.1", "1.1.1"),
        key = c(NA_character_, "k1", "k2"),
        type = c("object", "object", "string") %>% factor(levels = allowed_json_types),
        length = c(1L, 1L, 1L)
      ),
      list(list("k1" = list("k2" = "value")),
           list("k2" = "value"),
           "value")
    )
  )

})

test_that("works with empty values appropriately", {

  expect_identical(
    'null' %>% json_structure,
    tbl_json(
      data_frame(
        document.id = 1L,
        parent.id = NA_character_,
        level = 1L,
        index = 1L,
        child.id = "1",
        key = NA_character_,
        type = "null" %>% factor(levels = allowed_json_types),
        length = 0L
      ),
      list(NULL)
    )
  )

})

test_that("works with tbl_json already", {

  expect_identical(
    c('"a"', '"b"') %>% as.tbl_json %>% json_structure,
    tbl_json(
      data_frame(
        document.id = c(1L, 2L),
        parent.id = rep(NA_character_, 2),
        level = rep(1L, 2),
        index = rep(1L, 2),
        child.id = rep("1", 2),
        key = rep(NA_character_, 2),
        type = rep("string", 2) %>% factor(levels = allowed_json_types),
        length = rep(1L, 2)
      ),
      list("a", "b")
    )
  )

})
