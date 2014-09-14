context("jfilter")

test_that("filter works with one path", {
    
    json <- '{"name": "bob", "attributes": {"age": 32, "gender": "male"}}'

    expect_identical(
      json %>% as.jdf %>% jfilter("attributes"),
      jdf(
        data.frame(document.id = 1L),
        list(list(age = 32, gender = "male"))
      )
    )
    
  }
)

test_that("filter works with multiple depth paths", {
    
    json <- '{"name": "bob", "attributes": { "demographics": {"age": 32, "gender": "male"}}}'

    expect_identical(
      json %>% as.jdf %>% jfilter("attributes", "demographics"),
      jdf(
        data.frame(document.id = 1L),
        list(list(age = 32, gender = "male"))
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
      json %>% as.jdf %>% jvalue(name = jstring("name")) %>%
        jfilter("attributes"),
      jdf(
        data.frame(document.id = 1L, name = 'bob', stringsAsFactors = FALSE),
        list(list(age = 32, gender = "male"))
      )
    )
    
  }
)
