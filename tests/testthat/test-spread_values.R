test_that("jstring works with simple input", {

    json <- list(
      fromJSON('{"name": {"first": "bob", "last": "smith"}}'),
      fromJSON('{"name": {"first": "susan", "last": "jones"}}')
    )

    expect_identical(jstring("name", "first")(json), c("bob", "susan"))
    expect_identical(jstring("name", "last")(json), c("smith", "jones"))

    expect_identical(jstring("name", "first", recursive=TRUE)(json), c("bob", "susan"))
    expect_identical(jstring("name", "last", recursive=TRUE)(json), c("smith", "jones"))

  }
)

test_that("jstring works with unquoted strings", {

  json <- list(
    fromJSON('{"name": {"first": "bob", "last": "smith"}}'),
    fromJSON('{"name": {"first": "susan", "last": "jones"}}')
  )

  expect_identical(jstring(name, first)(json), c("bob", "susan"))
  expect_identical(jstring(name, last)(json), c("smith", "jones"))

}
)

test_that("jstring handles missing input properly", {

    json <- list(
      fromJSON('{"name": "bob"}'),
      fromJSON('{"name": ""}'),
      fromJSON('{"name": null}'),
      fromJSON('{"NAME": "bob"}'),
      fromJSON('{}')
    )

    expect_identical(jstring("name")(json),
      c("bob", "", NA_character_, NA_character_, NA_character_)
    )
  }
)

test_that("jdouble handles missing input properly", {
  
  json <- list(
    fromJSON('{"age": 32}'),
    fromJSON('{"age": null}'),
    fromJSON('{"AGE": 32}'),
    fromJSON('{}')
  )
  
  expect_identical(jdouble("age")(json),
                   c(32, NA_real_, NA_real_, NA_real_)
  )
}
)

test_that("jnumber handles missing input properly", {

    json <- list(
      fromJSON('{"age": 32}'),
      fromJSON('{"age": null}'),
      fromJSON('{"AGE": 32}'),
      fromJSON('{}')
    )

    expect_identical(jnumber("age")(json),
      c(32, NA_real_, NA_real_, NA_real_)
    )
  }
)

test_that("jlogical handles missing input properly", {

    json <- list(
      fromJSON('{"is.past": true}'),
      fromJSON('{"is.past": false}'),
      fromJSON('{"is.past": null}'),
      fromJSON('{"IS.PAST": true}'),
      fromJSON('{}')
    )

    expect_identical(jlogical("is.past")(json),
      c(TRUE, FALSE, NA, NA, NA)
    )
    expect_identical(jlogical("is.past", recursive=TRUE)(json),
      c(TRUE, FALSE, NA, NA, NA)
    )

  }
)

test_that("jinteger handles missing input properly", {
  
  json <- list(
    fromJSON('{"age": 32}'),
    fromJSON('{"age": null}'),
    fromJSON('{"AGE": 32}'),
    fromJSON('{}')
  )
  
  expect_identical(jinteger("age")(json),
                   c(32L, NA_integer_, NA_integer_, NA_integer_)
  )
}
)

test_that("spread_values extract various values", {

    json <- '{"name": "bob", "age": 32, "customer": true}'
    expected_value <- tbl_json(
        data.frame(
          document.id = 1L,
          name = "bob",
          age = 32,
          age_dbl = 32.,
          age_int = 32L,
          customer = TRUE,
          stringsAsFactors = FALSE
        ), list(fromJSON(json)))

    expect_identical(
      json %>%
        spread_values(
          name = jstring("name"),
          age = jnumber("age"),
          age_dbl = jdouble("age"),
          age_int = jinteger("age"),
          customer = jlogical("customer")
        ),
      expected_value
    )
  }
)

test_that("spread_values extract down a path", {

    json <- '{"name": {"first": "bob", "last": "smith"}}'
    expected_value <-  tbl_json(
 		       data.frame(
          		document.id = 1L,
          		first.name = "bob",
          		stringsAsFactors = FALSE
        		), list(fromJSON(json)))

    expect_identical(
      json %>%
        spread_values(first.name = jstring("name", "first")),
      expected_value
    )
  }
)

test_that("spread_values correctly handles character(0)", {

    empty <- tbl_json(
      data.frame(
        document.id = integer(0),
        value = character(0),
        stringsAsFactors = FALSE),
      list())

    expect_identical(
      character(0) %>% spread_values(value = jstring("name")),
      empty)
  }
)

test_that("spread_values correctly handles {}", {

    nl <- list()
    names(nl) <- character(0)
    empty <- tbl_json(
      data.frame(
        document.id = 1L,
        value = NA_character_,
        stringsAsFactors = FALSE),
      list(nl))

    expect_identical('{}' %>% spread_values(value = jstring("name")), empty)
  }
)


test_that("spread_values correctly handles []", {

    empty <- tbl_json(
      data.frame(
        document.id = 1L,
        value = NA_character_,
        stringsAsFactors = FALSE),
      list(list()))

    expect_identical('[]' %>% spread_values(value = jstring("name")), empty)
  }
)

test_that('spread_values correctly handles over-specified path', {
  json <- '{ "a" : 1 , "b" : "text", "c" : true }' 
  
  expect_equal(json %>% spread_values(a = jdouble("a", "b")) %>% .$a, as.double(NA))
  expect_equal(json %>% spread_values(a = jdouble("a", "b")) %>% .$a, as.numeric(NA))
  
  expect_equal(json %>% spread_values(a = jnumber("a", "b")) %>% .$a, as.double(NA))
  expect_equal(json %>% spread_values(a = jnumber("a", "b")) %>% .$a, as.numeric(NA))
  
  expect_equal(json %>% spread_values(b = jstring('b','c')) %>% .$b, as.character(NA))
  
  expect_equal(json %>% spread_values(c = jlogical('c','d')) %>% .$c, as.logical(NA))
  
  expect_equal(json %>% spread_values(c = jinteger('c','d')) %>% .$c, as.integer(NA))
})


test_that("recursive works for simple input", {

    json <- list(
      fromJSON('{"name": {"first": "bob", "last": "smith"}}'),
      fromJSON('{"name": {"first": "susan", "last": "jones"}}')
    )

    expect_identical(jstring("name", "first", recursive=TRUE)(json),
                     c("bob", "susan"))
    expect_identical(jstring("name", "last", recursive=TRUE)(json),
                     c("smith", "jones"))

  }
)

test_that("recursive works for complex input", {

    json <- list(
      fromJSON('{"name": {"first": {"string" : "bob"}, "last": "smith"}}'),
      fromJSON('{"name": {"first": "susan", "last": "jones"}}')
    )

    expect_identical(jstring("name", "first", recursive=TRUE)(json),
                     c("bob", "susan"))
    expect_identical(jstring("name", "last", recursive=TRUE)(json),
                     c("smith", "jones"))

    json <- list(
      fromJSON('{"price": {"value" : 30}}'),
      fromJSON('{"price": 30}')
    )

    expect_error(jnumber("price")(json))
    expect_identical(jnumber("price", recursive=TRUE)(json), c(30, 30))

  }
)

test_that("recursive works for complex input and 2 levels of recursion", {

    json <- list(
      fromJSON('{"name": {"first": {"string" : {"string" : "bob"}}, "last": "smith"}}'),
      fromJSON('{"name": {"first": "susan", "last": "jones"}}')
    )

    expect_identical(jstring("name", "first", recursive=TRUE)(json),
                     c("bob", "susan"))
    expect_identical(jstring("name", "last", recursive=TRUE)(json),
                     c("smith", "jones"))

    json <- list(
      fromJSON('{"price": {"value" : {"value" : 30}}}'),
      fromJSON('{"price": 30}')
    )

    expect_error(jnumber("price")(json))
    expect_identical(jnumber("price", recursive=TRUE)(json), c(30, 30))
  }
)

test_that("recursive returns an error when multiple values are present", {

    json <- list(
      fromJSON('{"name": {"first": {"string": "bob", "string" : "robert"}, "last": "smith"}}'),
      fromJSON('{"name": {"first": "susan", "last": "jones"}}')
    )

    expect_error(jstring("name", "first", recursive=TRUE)(json))

    json <- list(
      fromJSON('{"price": {"value" : {"value1" : 30, "value2": 30}}}'),
      fromJSON('{"price": 30}')
    )

    expect_error(jnumber("price", recursive=TRUE)(json))

  }
)

test_that("recursive works when nulls are present", {

    json <- c('{"name": {"first": {"string": "bob"}, "last": "smith"}}',
              '{"name": {"last": "jones"}}')

    expect_identical(
       (json %>% spread_values(name = jstring("name", "first", recursive=TRUE)))$name,
       c("bob", NA_character_))

    json <- c('{"name": {"first": {"string1": "bob", "string2": "robert"}}, "last": "smith"}',
              '{"name": {"first": {"string1": "bob"}}, "last": "jones"}')


  }
)

test_that("either throws an error when type converting", {

  # Regular
  expect_error(
    '{"name": "1"}' %>% spread_values(num = jnumber("name"))
  )

  # Recursive
  expect_error(
    '{"k1": {"k2": "1"}}' %>% spread_values(num = jnumber("k1", recursive = TRUE))
  )

})

test_that("works with x, json as input", {

  expect_identical('{"x": 1}' %>% spread_values(x = jnumber("x")),
                   '{"x": 1}' %>% spread_values(y = jnumber("x")) %>% rename(x = y))

  expect_identical('{"json": 1}' %>% spread_values(json = jnumber("json")),
                   '{"json": 1}' %>% spread_values(y = jnumber("json")) %>% rename(json = y))

})
