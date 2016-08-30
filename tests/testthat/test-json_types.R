context("json_types")

test_that("works with simple input", {
    
    json <- '[{"key":"value"}, [1, 2], "string", 1, true, false, null]'

    expect_identical(
      json %>% gather_array %>% json_types,
      tbl_json(
        data.frame(
          document.id = rep(1L, 7),
          array.index = 1L:7L,
          type = factor(
            c("object", "array", "string", "number", "logical", "logical", 
              "null"),
            levels = allowed_json_types)
        ),
        list(list(key = "value"), list(1L, 2L), "string", 1L, TRUE, FALSE, NULL)
      )
    )

  }
)

test_that("works with varying array types", {
    
    json <- '[[1, 2], [1, null], [{"key":"value"}], [null]]'

    expect_identical(
      (json %>% gather_array %>% json_types)$type,
      factor(rep("array", 4), levels = allowed_json_types)
    )
    
  }
)

test_that("works with varying empty data", {
    
    json <- '[[], {}, null]'

    expect_identical(
      (json %>% gather_array %>% json_types)$type,
      factor(c("array", "object", "null"), levels = allowed_json_types)
    )
    
  }
)

test_that("works with character(0)", {
    
    expect_identical(
      (character(0) %>% json_types)$type,
      factor(character(0), levels = allowed_json_types)
    )
    
  }
)