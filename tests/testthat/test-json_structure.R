test_that("simple string works", {

  expect_identical(
    '"a"' %>% json_structure,
    tbl_json(
      dplyr::tibble(
        document.id = 1L,
        parent.id = NA_character_,
        level = 0L,
        index = 1L,
        child.id = "1",
        seq = list(list()),
        name = NA_character_,
        type = "string" %>% factor(levels = allowed_json_types),
        length = 1L
      ),
      list("a")
    )
  )

})

test_that("simple object works", {
  actual <- '{"name": "value"}' %>% json_structure
  expected <- tbl_json(
      dplyr::tibble(
        document.id = c(1L, 1L),
        parent.id = c(NA_character_, "1"),
        level = c(0L, 1L),
        index = c(1L, 1L),
        child.id = c("1", "1.1"),
        seq = list(list(), list("name")),
        name = c(NA_character_, "name"),
        type = c("object", "string") %>% factor(levels = allowed_json_types),
        length = c(1L, name = 1L)
      ),
      list(list("name" = "value"), name = "value")
    )
  #attr(expected, 'JSON') <- NULL
  #row.names(actual) <- NULL
  #row.names(expected) <- NULL
  expect_identical(
    actual,
    expected
  )

})

test_that("simple array works", {

  expect_identical(
    '[1, 2]' %>% json_structure,
    tbl_json(
      dplyr::tibble(
        document.id = c(1L, 1L, 1L),
        parent.id = c(NA_character_, "1", "1"),
        level = c(0L, 1L, 1L),
        index = c(1L, 1L, 2L),
        child.id = c("1", "1.1", "1.2"),
        seq = list(list(), list(1L), list(2L)),
        name = rep(NA_character_, 3),
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
      dplyr::tibble(
        document.id = c(1L, 1L, 1L),
        parent.id = c(NA_character_, "1", "1.1"),
        level = c(0L, 1L, 2L),
        index = c(1L, 1L, 1L),
        child.id = c("1", "1.1", "1.1.1"),
        seq = list(list(), list("k1"), list("k1", "k2")),
        name = c(NA_character_, "k1", "k2"),
        type = c("object", "object", "string") %>% factor(levels = allowed_json_types),
        length = c(1L, k1 = 1L, k2 = 1L)
      ),
      list(list("k1" = list("k2" = "value")),
           k1 = list("k2" = "value"),
           k2 = "value")
    )
  )

})

test_that("works with empty values appropriately", {

  expect_identical(
    'null' %>% json_structure,
    tbl_json(
      dplyr::tibble(
        document.id = 1L,
        parent.id = NA_character_,
        level = 0L,
        index = 1L,
        child.id = "1",
        seq = list(list()),
        name = NA_character_,
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
      dplyr::tibble(
        document.id = c(1L, 2L),
        parent.id = rep(NA_character_, 2),
        level = rep(0L, 2),
        index = rep(1L, 2),
        child.id = rep("1", 2),
        seq = list(list(), list()),
        name = rep(NA_character_, 2),
        type = rep("string", 2) %>% factor(levels = allowed_json_types),
        length = rep(1L, 2)
      ),
      list("a", "b")
    )
  )

})

test_that("seq works for a deeply nested sequence", {

  expect_identical(
    '{"a": {"2": [1, {"3": "value"}] } }' %>%
      json_structure %>%
      `[[`("seq") %>%
      `[[`(6),
    list("a", "2", 2L, "3")
  )

})

test_that("works with empty JSON", {

  expect_identical('[]' %>% json_structure %>% nrow, 1L)
  expect_identical('{}' %>% json_structure %>% nrow, 1L)
  expect_identical(character(0) %>% json_structure %>% nrow, 0L)
  expect_identical('null' %>% json_structure %>% nrow, 1L)

})


test_that("imputes document.id when not present", {
  j1 <- dplyr::tibble(id=1, json='"a"') %>% 
    as.tbl_json(json.column = 'json') %>% json_structure()
  j2 <- dplyr::tibble(id=1, json='["a"]') %>% 
    as.tbl_json(json.column = 'json') %>% json_structure()
  j3 <- dplyr::tibble(id=1, json='{"a":1}') %>% 
    as.tbl_json(json.column = 'json') %>% json_structure()
  
  expect_identical(names(j1), names(j2))
  expect_identical(names(j1), names(j3))
  expect_identical(nrow(j2),nrow(j3))
  expect_identical(as.character(j2$type), c('array','string'))
  expect_identical(as.character(j3$type), c('object','number'))
})

test_that("imputed document.id works", {
  j <- dplyr::tibble(id=1, json='[{"a":1},{"a":2}]') %>% 
    as.tbl_json(json.column='json') %>% gather_array() %>%
    json_structure()
  
  expect_identical(j$document.id, c(1L,2L,1L,2L))
  expect_identical(as.character(j$type),c('object','object','number','number'))
  expect_identical(j$child.id,c('1','1','1.1','1.2'))
})

