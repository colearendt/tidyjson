context("append_values")

test_that("has correct complete structure with simple input", {
    
    json <- '[{"name": "anne"}, {"name": "bob"}, {"name": "george"}]'

    expect_identical(
      json %>% gather_array %>% gather_keys %>% 
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
      json %>% gather_array %>% append_values_string,
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
      (json %>% gather_array %>% append_values_string)$string,
      c("a", "b", NA_character_)
    )
    
  }
)

test_that("number works with simple input", {
    
    json <- '[1, 2, null]'

    expect_identical(
      (json %>% gather_array %>% append_values_number)$number,
      c(1, 2, NA_real_)
    )
    
  }
)

test_that("logical works with simple input", {
    
    json <- '[true, false, null]'

    expect_identical(
      (json %>% gather_array %>% append_values_logical)$logical,
      c(TRUE, FALSE, NA)
    )
    
  }
)

test_that("handles mixed input as appropriate NA", {
    
    data <- '["a", 1, true, null]' %>% gather_array 
    expect_identical(
      (data %>% append_values_string)$string,
      c("a", "1", "TRUE", NA_character_)
    )

    expect_warning(tmp_data <- data %>% append_values_number)

    expect_identical(
      tmp_data$number,
      c(NA_real_, 1, NA_real_, NA_real_)
    )
    expect_identical(
      (data %>% append_values_logical)$logical,
      c(NA, NA, TRUE, NA)
    )
    
  }
)

test_that("correctly handles character(0)", {
    
    empty <- tbl_json(
      data.frame(
        document.id = integer(0), 
        string = character(0),
        stringsAsFactors = FALSE),

    list())

    expect_identical(
      character(0) %>% append_values_string,
      empty)

  }
)

test_that("correctly handles {}", {
    
    nl <- list()
    names(nl) <- character(0)
    
    empty <- tbl_json(
      data.frame(
        document.id = 1L, 
        string = NA_character_,
        stringsAsFactors = FALSE),
      list(nl))

    expect_identical(
      '{}' %>% append_values_string,
      empty)
    
  }
)

test_that("correctly handles []", {
    
    empty <- tbl_json(
      data.frame(
        document.id = 1L, 
        string = NA_character_,
        stringsAsFactors = FALSE),
      list(list()))

    expect_identical(
      '[]' %>% append_values_string,
      empty)
    
  }
)

test_that("correctly handles mixed types when force=FALSE", {

    data <- '["a", 1, true, null]' %>% gather_array 

    expect_identical(
      (data %>% append_values_string(force=FALSE))$string,
      c("a", rep(NA_character_,3))
    )
    expect_identical(
      (data %>% append_values_number(force=FALSE))$number,
      c(NA_real_, 1, NA_real_, NA_real_)
    )
    expect_identical(
      (data %>% append_values_logical(force=FALSE))$logical,
      c(NA, NA, TRUE, NA)
    )
  }
)

test_that("correctly handles append when trying to append an array", {

   data <- '[["a", "b", "c"], "d", "e", "f"]' %>% gather_array
   
   expect_identical(
     (data %>% append_values_string())$string,
     c(NA_character_, "d", "e", "f")
   ) 
  }
)

context("my_unlist")
test_that("my_unlist safely handles edge cases", {

    expect_identical(my_unlist(list(1, NA)), c(1, NA_integer_))
    expect_identical(my_unlist(list("a", NA_character_)), c("a", NA_character_))
    expect_identical(my_unlist(list(1, NULL)), c(1, NA_integer_))
    expect_identical(my_unlist(list(1, list(1, 1))), c(1, NA_integer_))

  }
)
