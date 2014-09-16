context("append_types")

test_that("works with simple input", {
    
    json <- '[{"key":"value"}, [1, 2], "string", 1, true, false, null]'

    expect_identical(
      json %>% as.tbl_json %>% gather_array %>% append_types,
      tbl_json(
        data.frame(
          document.id = rep(1L, 7),
          array.index = 1L:7L,
          type = factor(
            c("object", "array", "string", "number", "logical", "logical", 
              "null"),
            levels = json_types)
        ),
        list(list(key = "value"), c(1, 2), "string", 1, TRUE, FALSE, NULL)
      )
    )
    
  }
)

test_that("works with varying array types", {
    
    json <- '[[1, 2], [1, null], [{"key":"value"}], [null]]'

    expect_identical(
      (json %>% as.tbl_json %>% gather_array %>% append_types)$type,
      factor(rep("array", 4), levels = json_types)
    )
    
  }
)

test_that("works with varying empty data", {
    
    json <- '[[], null]'

    expect_identical(
      (json %>% as.tbl_json %>% gather_array %>% append_types)$type,
      factor(c("array", "null"), levels = json_types)
    )
    
  }
)

test_that("cannot determine between {} and [] because of fromJSON", {
    
    json <- '[{}, []]'

    # Should be "object", "array", but is "array", "array" instead
    expect_identical(
      (json %>% as.tbl_json %>% gather_array %>% append_types)$type,
      factor(c("array", "array"), levels = json_types)
    )
  }
)