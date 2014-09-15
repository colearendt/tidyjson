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

context("spread_values")

test_that("exctract various values", {
    
    json <- '{"name": "bob", "age": 32, "customer": true}'
    
    expect_identical(
      json %>% as.tbl_json %>% 
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
      json %>% as.tbl_json %>% 
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