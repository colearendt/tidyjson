context("read_json")

test_that("read_json correctly loads a .json file", {
    json_path <- system.file("extdata", "sample.json", package="tidyjson")
    json <- read_json(json_path)

    expect_true(json %>% is.tbl_json)
    expect_identical(
      json %>% gather_array(),
      tbl_json(
          data_frame(document.id = 1L, array.index = 1L:8L),
          attr(json, "JSON")[[1]])
    )
})

test_that("read_json correctly infers a .jsonl file", {
    json_path <- system.file("extdata", "sample.jsonl", package="tidyjson")
    json <- read_json(json_path)

    expect_true(json %>% is.tbl_json)
    expect_identical(
      json,
      tbl_json(
          data_frame(document.id = 1:8),
          attr(json, "JSON"))
    )
})
