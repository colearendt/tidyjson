context("json_lengths")

test_that("works for arrays", {
    
    json <- c('[]', '[1]', '[1, 2]')
    expect_identical(
      json %>% json_lengths %>% `$`(length),
      c(0L, 1L, 2L)
    )
    
  }
)

test_that("works for objects", {
    
    json <- c('{}', '{"k":"v"}', '{"k1":"v1", "k2":"v2"}') 
    expect_identical(
      json %>% json_lengths %>% `$`(length),
      c(0L, 1L, 2L)
    )
    
  }
)

test_that("works for scalars", {
    
    json <- c('[1, "a", true]') 
    expect_identical(
      json %>% gather_array %>% json_lengths %>% `$`(length),
      rep(1L, 3)
    )
     
  }
)

test_that("works for emtpy objects", {
    
    json <- character(0) 
    expect_identical(
      json %>% json_lengths %>% `$`(length),
      integer(0)
    )
    
    json <- c('[null, [], {}]') 
    expect_identical(
      json %>% gather_array %>% json_lengths %>% `$`(length),
      rep(0L, 3)
    )
     
  }
)