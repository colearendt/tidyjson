context("enter_object")

test_that("filter works with one path", {
    
    json <- '{"name": "bob", "attributes": {"age": 32, "gender": "male"}}'

    expect_identical(
      json %>% enter_object("attributes"),
      tbl_json(
        data.frame(document.id = 1L),
        list(list(age = 32L, gender = "male"))
      )
    )
    
  }
)

test_that("filter works with multiple depth paths", {
    
    json <- '{"name": "bob", "attributes": { "demographics": {"age": 32, "gender": "male"}}}'

    expect_identical(
      json %>% enter_object("attributes", "demographics"),
      tbl_json(
        data.frame(document.id = 1L),
        list(list(age = 32L, gender = "male"))
      )
    )
    
  }
)

test_that("filter removes records with missing path", {
    
    json <- c(
      '{"name": "bob", "attributes": {"age": 32, "gender": "male"}}',
      '{"name": "susan"}'
    )
    
    expect_identical(
      json %>% spread_values(name = jstring("name")) %>%
        enter_object("attributes"),
      tbl_json(
        data.frame(
          document.id = 1L, 
          name = 'bob', 
          stringsAsFactors = FALSE),
        list(list(age = 32L, gender = "male"))
      )
    )
    
  }
)

test_that("works if no paths exist", {
    
    json <- '{"name": "bob"}'
    
    expect_identical(
      json %>% spread_values(name = jstring("name")) %>%
        enter_object("attributes"),
      tbl_json(
        data.frame(
          document.id = integer(0), 
          name = character(0), 
          stringsAsFactors = FALSE),
        list()
      )
    )
    
  }
)

test_that("correctly handles character(0), {}, []", {
    
    empty <- tbl_json(
      data.frame(
        document.id = integer(0), 
        stringsAsFactors = FALSE),
      list())
    
    expect_identical(
      character(0) %>% enter_object("key"),
      empty)
    
    expect_identical(
      '{}' %>% enter_object("key"),
      empty)
    
    expect_identical(
      '[]' %>% enter_object("key"),
      empty)

  }
)
