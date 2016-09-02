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
    '{"k0": null, "k1": "a",  "k2": [1, 2]}',
    '{"k0": null, "k1": null, "k2": [1, 2, 3], "k3": true, "k4": null}',
    '{"k0": null,             "k2": null,                  "k4": {"k5": "a"}}'
  )

  expect_identical(
    json_schema(json),
    '{"k0": "null", "k1": "null", "k1": "string", "k2": "null", "k2": ["number"], "k3": "logical", "k4": "null", "k4": {"k5": "string"}}'
  )

})

test_that("works for empty arrays", {

  expect_identical(json_schema('[]'), '[]')
  expect_identical(json_schema('{"key": []}'), '{"key": []}')

})

test_that("works for complex nested types", {

  # object x object
  expect_identical(
    json_schema('{"k1": {"k2":  null}}'),
                '{"k1": {"k2": "null"}}')
  # object x array
  expect_identical(
    json_schema('{"k1": [1, 2]}'),
    '{"k1": ["number"]}')
  # array x object
  expect_identical(
    json_schema('[{"k1":  null,  "k2": null}]'),
                '[{"k1": "null", "k2": "null"}]')
  # array x array
  expect_identical(
    json_schema('[[1, 2], [1, 2]]'),
                '[["number"]]')

})

test_that("simple mixed type array", {

  expect_identical('["a", 1, true, null]' %>% json_schema,
                   '["string", "number", "logical", "null"]')

})


test_that("problem with mixed type arrays", {

  expect_identical('[[1,2], "a"]' %>% json_schema,
                   '[["number"], "string"]')

})

test_that("json_schema works for a very complex example", {

  expect_is(json_schema(companies[1]), "character")

  expect_is(json_schema(companies[1:5]), "character")

})
