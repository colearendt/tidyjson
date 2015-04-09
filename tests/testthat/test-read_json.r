context("read_json")

test_that("read_json correctly infers a .json file", {
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

test_that("read_json does not allow incorrect formats", {
    json_path <- system.file("extdata", "sample.jsonl", package="tidyjson")
    expect_error(
      read_json(json_path, format = "json")
    )

    expect_error(
      read_json(json_path, format = "json123"),
      regexp="Unrecognized json format: json123")
})

test_that("read_json fails if it cannot infer format", {
    json_path <- system.file("extdata", "sample_jsonl", package="tidyjson")
    expect_error(
      read_json(json_path)
    )

    expect_error(
      read_json(json_path, format = "infer")
    )
})

test_that("read_json uses given format", {
    json_path <- system.file("extdata", "sample_jsonl", package="tidyjson")
    json <- read_json(json_path, format="jsonl")
    expect_true(json %>% is.tbl_json)
    expect_identical(
      json,
      tbl_json(
          data_frame(document.id = 1:8),
          attr(json, "JSON"))
    )
})
