context("jstring")

test_that("works with simple input", {
    
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

test_that("handles missing input properly", {
    
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
    expect_identical(jstring("name", recursive=TRUE)(json),
      c("bob", "", NA_character_, NA_character_, NA_character_)
    )
    
  }
)

context("jnumber")

test_that("handles missing input properly", {
    
    json <- list(
      fromJSON('{"age": 32}'),
      fromJSON('{"age": null}'),
      fromJSON('{"AGE": 32}'),
      fromJSON('{}')
    )
    
    expect_identical(jnumber("age")(json),
      c(32, NA_real_, NA_real_, NA_real_)
    )
    expect_identical(jnumber("age", recursive=TRUE)(json),
      c(32, NA_real_, NA_real_, NA_real_)
    )
    
  }
)

context("jlogical")

test_that("handles missing input properly", {
    
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

context("spread_values")

test_that("exctract various values", {
    
    json <- '{"name": "bob", "age": 32, "customer": true}'
    expected_value <- tbl_json(
        data.frame(
          document.id = 1L,
          name = "bob",
          age = 32,
          customer = TRUE,
          stringsAsFactors = FALSE
        ), list(fromJSON(json)))
    
    expect_identical(
      json %>% as.tbl_json %>% 
        spread_values(
          name = jstring("name"),
          age = jnumber("age"),
          customer = jlogical("customer")
        ),
      expected_value
    )

    expect_identical(
      json %>% as.tbl_json %>% 
        spread_values(
          name = jstring("name", recursive=TRUE),
          age = jnumber("age", recursive=TRUE),
          customer = jlogical("customer", recursive=TRUE)
        ),
      expected_value
    )
    
  }
)

test_that("extract down a path", {
    
    json <- '{"name": {"first": "bob", "last": "smith"}}'
    expected_value <-  tbl_json(
 		       data.frame(
          		document.id = 1L,
          		first.name = "bob",
          		stringsAsFactors = FALSE
        		), list(fromJSON(json)))
    
    expect_identical(
      json %>% as.tbl_json %>% 
        spread_values(first.name = jstring("name", "first")),
      expected_value
    )

    expect_identical(
      json %>% as.tbl_json %>% 
        spread_values(first.name = jstring("name", "first", recursive=TRUE)),
      expected_value
    )
    
  }
)

test_that("correctly handles character(0)", {
    
    empty <- tbl_json(
      data.frame(
        document.id = integer(0),
        value = character(0),
        stringsAsFactors = FALSE),
      list())
    
    expect_identical(
      character(0) %>% as.tbl_json %>% spread_values(value = jstring("key")),
      empty)
    expect_identical(
      character(0) %>% as.tbl_json %>% spread_values(value = jstring("key", recursive=TRUE)),
      empty)
     
  }
)

test_that("correctly handles {}", {
    
    nl <- list()
    names(nl) <- character(0)
    empty <- tbl_json(
      data.frame(
        document.id = 1L,
        value = NA_character_,
        stringsAsFactors = FALSE),
      list(nl))
    
    expect_identical(
      '{}' %>% as.tbl_json %>% spread_values(value = jstring("key")),
      empty
    )
    expect_identical(
      '{}' %>% as.tbl_json %>% spread_values(value = jstring("key", recursive=TRUE)),
      empty
    )
  }
)


test_that("correctly handles []", {
    
    empty <- tbl_json(
      data.frame(
        document.id = 1L,
        value = NA_character_,
        stringsAsFactors = FALSE),
      list(list()))
    
    expect_identical(
      '[]' %>% as.tbl_json %>% spread_values(value = jstring("key")),
      empty
    )
    expect_identical(
      '[]' %>% as.tbl_json %>% spread_values(value = jstring("key", recursive=TRUE)),
      empty
    )
    
  }
)

context("recursive option")

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

    expect_identical(jnumber("price")(json), c(NA_real_, 30))
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

    expect_identical(jnumber("price")(json), c(NA_real_, 30))
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

