context("gather_array")

test_that("works with array of length 1", {
    
    arrays <- '[{"name": "bob"}]'
    
    expect_identical(
      arrays %>% as.jdf %>% gather_array,
      jdf(
        data.frame(
          document.id = 1L,
          array.index = 1L
        ),
        list(
          list(name = "bob")
        )
      )
    )
    
  }
)

test_that("works with single array", {
    
    arrays <- '[{"name": "bob"}, {"name": "susan"}]'
    
    expect_identical(
      arrays %>% as.jdf %>% gather_array,
      jdf(
        data.frame(
          document.id = c(1L, 1L),
          array.index = c(1L, 2L)
        ),
        list(
          list(name = "bob"), list(name = "susan")
        )
      )
    )
    
  }
)

test_that("works with multiple arrays", {
    
    arrays <- c(
      '[{"name": "bob"}, {"name": "susan"}]', 
      '[{"name": "john"}]'
    )
    
    expect_identical(
      arrays %>% as.jdf %>% gather_array,
      jdf(
        data.frame(
          document.id = c(1L, 1L, 2L),
          array.index = c(1L, 2L, 1L)
        ),
        list(
          list(name = "bob"), list(name = "susan"), list(name = "john")
        )
      )
    )
    
  }
)

test_that("empty arrays are dropped", {
    
    arrays <- c('[{"name": "bob"}]', '[]')
    
    expect_identical(
      arrays %>% as.jdf %>% gather_array,
      jdf(
        data.frame(
          document.id = 1L,
          array.index = 1L
        ),
        list(
          list(name = "bob")
        )
      )
    )
    
  }
)

test_that("objects throws error", {
    
    arrays <- c('[{"name": "bob"}]', '{"name": "susan"}')
    
    expect_error(arrays %>% as.jdf %>% gather_array)
    
  }
)

test_that("values throws error", {
    
    arrays <- c('[{"name": "bob"}]', '"bob"')
    
    expect_error(arrays %>% as.jdf %>% gather_array)
    
  }
)

