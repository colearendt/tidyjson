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
