test_that("json_schema works for simple examples", {

  expect_identical(json_schema('"a"'), '"string"')
  expect_identical(json_schema('1'), '"number"')
  expect_identical(json_schema('true'), '"logical"')
  expect_identical(json_schema('null'), '"null"')
  expect_identical(json_schema('[1, 2, 3]'), '["number"]')
  expect_identical(json_schema('{"name": "value"}'), '{"name": "string"}')

})

test_that("json_schema works for a more complex object", {

  json <- c(
    '{"k0": null, "k1": "a",  "k2": [1, 2]}',
    '{"k0": null, "k1": null, "k2": [1, 2, 3], "k3": true, "k4": null}',
    '{"k0": null,             "k2": null,                  "k4": {"k5": "a"}}',
    '{"k0": null,             "k2": [],        "k3": null, "k4": {}}'
  )

  expect_identical(
    json_schema(json),
    '{"k0": "null", "k1": "string", "k2": ["number"], "k3": "logical", "k4": {"k5": "string"}}'
  )

})

test_that("json_schema works for a more complex array", {

  json <- c(
    '[null]',
    '[1]',
    '["string"]',
    '[true]',
    '[{"name": "string"}]',
    '[[1]]',
    '[{"name": {"k1": 1, "k2": 2}}]'
  )

  expect_identical(
    json_schema(json),
    '[{"name": {"k1": "number", "k2": "number"}}]'
  )

})

test_that("works for empty arrays", {

  expect_identical(json_schema('[]'), '[]')
  expect_identical(json_schema('{"name": []}'), '{"name": []}')

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
                   '["string"]')

})


test_that("problem with mixed type arrays", {

  expect_identical('[[1,2], "a"]' %>% json_schema,
                   '[["number"]]')

})

test_that("json_schema works for real examples", {

  expect_type(json_schema(worldbank[1]), "character")
  expect_type(json_schema(companies[1]), "character")

})

test_that("types = 'value' works as intended", {

  expect_identical('"string"' %>% json_schema("value"), '"string"')
  expect_identical('2' %>% json_schema("value"), '1')
  expect_identical('true' %>% json_schema("value"), 'true')
  expect_identical('false' %>% json_schema("value"), 'true')
  expect_identical('null' %>% json_schema("value"), 'null')
  expect_identical('{"name": [2]}' %>% json_schema("value"), '{"name": [1]}')

})
