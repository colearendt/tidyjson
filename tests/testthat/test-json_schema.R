context("json_schema")

test_that("json_schema works for simple examples", {

  expect_identical(json_schema('"a"'), '"string"')
  expect_identical(json_schema('1'), '"number"')
  expect_identical(json_schema('true'), '"logical"')
  expect_identical(json_schema('null'), '"null"')
  expect_identical(json_schema('[1, 2, 3]'), '["number"]')
  expect_identical(json_schema('{"key": "value"}'), '{"key": "string"}')

})

test_that("json_schema works for a more complex example", {

  json <- c(
    '{"k1": "a",  "k2": [1, 2]}',
    '{"k1": null, "k2": [1, 2, 3], "k3": true, "k4": null}',
    '{            "k2": null,                  "k4": {"k5": "a"}}'
  )

  expect_identical(
    json_schema(json),
    '{"k1": "string", "k2": ["number"], "k3": logical, "k4": {"k5": "string"}}'
  )

})
