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

context("jvalue")

test_that("exctract various values", {
    
    json <- '{"name": "bob", "age": 32, "customer": true}'
    
    expect_identical(
      json %>% as.jdf %>% 
        jvalue(
          name = jstring("name"),
          age = jnumber("age"),
          customer = jlogical("customer")
        ),
      structure(
        data.frame(
          document.id = 1L,
          name = "bob",
          age = 32,
          customer = TRUE,
          stringsAsFactors = FALSE
        ),
        JSON = list(
          fromJSON(json)
        ),
        class = c("jdf", "data.frame")
      )
    )
    
  }
)

test_that("exctract down a path", {
    
    json <- '{"name": {"first": "bob", "last": "smith"}}'
    
    expect_identical(
      json %>% as.jdf %>% 
        jvalue(first.name = jstring("name", "first")),
      structure(
        data.frame(
          document.id = 1L,
          first.name = "bob",
          stringsAsFactors = FALSE
        ),
        JSON = list(
          fromJSON(json)
        ),
        class = c("jdf", "data.frame")
      )
    )
    
  }
)