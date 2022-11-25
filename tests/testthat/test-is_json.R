test_that("works for a simple example", {

  json <- '[1, "string", true, [1, 2], {"name": "value"}, null]' %>%
    gather_array

  expect_identical(json %>% is_json_number  %>% which, 1L)
  expect_identical(json %>% is_json_string  %>% which, 2L)
  expect_identical(json %>% is_json_logical %>% which, 3L)
  expect_identical(json %>% is_json_array   %>% which, 4L)
  expect_identical(json %>% is_json_object  %>% which, 5L)
  expect_identical(json %>% is_json_null    %>% which, 6L)
  expect_identical(json %>% is_json_scalar  %>% which, c(1L, 2L, 3L))

})

test_that("works with filter", {

  json <- '[1, "string", true, [1, 2], {"name": "value"}, null]' %>%
    gather_array

  expect_identical(
    json %>% filter(is_json_array(.)),
    json %>% slice(4)
  )

  expect_identical(
    json %>% filter(is_json_object(.)),
    json %>% slice(5)
  )

})
