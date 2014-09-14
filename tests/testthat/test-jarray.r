context("jarray")

test_that("works with array of length 1", {
    
    arrays <- '[{"name": "bob"}]'
    
    expect_identical(
      arrays %>% as.jdf %>% jarray,
      structure(
        data.frame(
          document.id = 1L,
          array.index = 1L
        ),
        JSON = list(
          list(name = "bob")
        ),
        class = c("jdf", "data.frame")
      )
    )
    
  }
)

test_that("works with single array", {
    
    arrays <- '[{"name": "bob"}, {"name": "susan"}]'
    
    expect_identical(
      arrays %>% as.jdf %>% jarray,
      structure(
        data.frame(
          document.id = c(1L, 1L),
          array.index = c(1L, 2L)
        ),
        JSON = list(
          list(name = "bob"), list(name = "susan")
        ),
        class = c("jdf", "data.frame")
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
      arrays %>% as.jdf %>% jarray,
      structure(
        data.frame(
          document.id = c(1L, 1L, 2L),
          array.index = c(1L, 2L, 1L)
        ),
        JSON = list(
          list(name = "bob"), list(name = "susan"), list(name = "john")
        ),
        class = c("jdf", "data.frame")
      )
    )
    
  }
)

test_that("empty arrays are dropped", {
    
    arrays <- c('[{"name": "bob"}]', '[]')
    
    expect_identical(
      arrays %>% as.jdf %>% jarray,
      structure(
        data.frame(
          document.id = 1L,
          array.index = 1L
        ),
        JSON = list(
          list(name = "bob")
        ),
        class = c("jdf", "data.frame")
      )
    )
    
  }
)

test_that("objects throws error", {
    
    arrays <- c('[{"name": "bob"}]', '{"name": "susan"}')
    
    expect_error(arrays %>% as.jdf %>% jarray)
    
  }
)

test_that("values throws error", {
    
    arrays <- c('[{"name": "bob"}]', '"bob"')
    
    expect_error(arrays %>% as.jdf %>% jarray)
    
  }
)

