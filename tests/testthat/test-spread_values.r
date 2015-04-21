context("jstring")

test_that("works with simple input", {
    
    json <- list(
      fromJSON('{"name": {"first": "bob", "last": "smith"}}'),
      fromJSON('{"name": {"first": "susan", "last": "jones"}}')
    )
    
    expect_identical(jstring("name", "first")(json), c("bob", "susan"))
    expect_identical(jstring("name", "last")(json), c("smith", "jones"))
    
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
  
  }
)

context("spread_values")

test_that("exctract various values", {
    
    json <- '{"name": "bob", "age": 32, "customer": true}'
    
    expect_identical(
      json %>%
        spread_values(
          name = jstring("name"),
          age = jnumber("age"),
          customer = jlogical("customer")
        ),
      tbl_json(
        data.frame(
          document.id = 1L,
          name = "bob",
          age = 32,
          customer = TRUE,
          stringsAsFactors = FALSE
        ),
        list(
          fromJSON(json)
        )
      )
    )
    
  }
)

test_that("exctract down a path", {
    
    json <- '{"name": {"first": "bob", "last": "smith"}}'
    
    expect_identical(
      json %>%
        spread_values(first.name = jstring("name", "first")),
      tbl_json(
        data.frame(
          document.id = 1L,
          first.name = "bob",
          stringsAsFactors = FALSE
        ),
        list(
          fromJSON(json)
        )
      )
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
      character(0) %>% spread_values(value = jstring("key")),
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
      '{}' %>% spread_values(value = jstring("key")),
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
      '[]' %>% spread_values(value = jstring("key")),
      empty
    )
    
  }
)