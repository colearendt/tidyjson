context("append_values")

test_that("has correct complete structure with simple input", {
    
    json <- '[{"name": "anne"}, {"name": "bob"}, {"name": "george"}]'

    expect_identical(
      json %>% as.tbl_json %>% gather_array %>% gather_keys %>% 
        append_values_string,
      tbl_json(
        data.frame(
          document.id = c(1L, 1L, 1L),
          array.index = 1L:3L,
          key = rep("name", 3),
          string = c("anne", "bob", "george"),
          stringsAsFactors = FALSE
        ),
        list("anne", "bob", "george")
      )
    )
    
  }
)

test_that("string works with value array", {
    
    json <- '["a", "b"]'

    expect_identical(
      json %>% as.tbl_json %>% gather_array %>% append_values_string,
      tbl_json(
        data.frame(
          document.id = c(1L, 1L),
          array.index = 1L:2L,
          string = c("a", "b"),
          stringsAsFactors = FALSE
        ),
        list("a", "b")
      )
    )
    
  }
)

test_that("string works with simple input", {
    
    json <- '["a", "b", null]'

    expect_identical(
      (json %>% as.tbl_json %>% gather_array %>% append_values_string)$string,
      c("a", "b", NA_character_)
    )
    
  }
)

test_that("number works with simple input", {
    
    json <- '[1, 2, null]'

    expect_identical(
      (json %>% as.tbl_json %>% gather_array %>% append_values_number)$number,
      c(1, 2, NA_real_)
    )
    
  }
)

test_that("logical works with simple input", {
    
    json <- '[true, false, null]'

    expect_identical(
      (json %>% as.tbl_json %>% gather_array %>% append_values_logical)$logical,
      c(TRUE, FALSE, NA)
    )
    
  }
)

test_that("handles mixed input as appropriate NA", {
    
    data <- '["a", 1, true, null]' %>% as.tbl_json %>% gather_array 

    expect_identical(
      (data %>% append_values_string)$string,
      c("a", rep(NA_character_, 3))
    )
    expect_identical(
      (data %>% append_values_number)$number,
      c(NA_real_, 1, rep(NA_real_, 2))
    )
    expect_identical(
      (data %>% append_values_logical)$logical,
      c(rep(NA, 2), TRUE, NA)
    )
    
  }
)

  
