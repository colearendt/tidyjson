context("spread_all")

test_that("works for simple example", {

  expect_identical(
    '{"a": 1, "b": "x", "c": true}' %>% spread_all,
    tbl_json(
      data_frame(
        document.id = 1L,
        a = 1,
        b = "x",
        c = TRUE
      ),
      list(list(a = 1L, b = "x", c = TRUE))
    )
  )

})

test_that("spreads a null column", {

  expect_identical(
    '{"a": null}' %>% spread_all,
    tbl_json(
      data_frame(
        document.id = 1L,
        a = NA
      ),
      list(list(a = NULL))
    )
  )

})

test_that("handles a more complex document", {

  json <- c(
    '{"a": "x",  "b": 1,    "c": true, "d": null}',
    '{"a": null, "b": null, "c": null, "d": null}',
    '{"e": null}'
  )

  expect_identical(
    json %>% spread_all,
    tbl_json(
      data_frame(
        document.id = 1L:3L,
        a = c("x", NA_character_, NA_character_),
        b = c(1, NA_integer_, NA_integer_),
        c = c(TRUE, NA, NA),
        d = rep(NA, 3),
        e = rep(NA, 3)
      ),
      json %>% map(fromJSON, simplifyVector = FALSE)
    )
  )

})
