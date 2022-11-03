test_that("has correct structure with simple input", {

  json <- '[{"name": "anne"}, {"name": "bob"}, {"name": "george"}]'

  expect_identical(
    json %>% gather_array %>% gather_object %>%
      append_values_string,
    tbl_json(
      data.frame(
        document.id = c(1L, 1L, 1L),
        array.index = 1L:3L,
        name = rep("name", 3),
        string = c("anne", "bob", "george"),
        stringsAsFactors = FALSE
      ),
      list(name = "anne", name = "bob", name = "george")
    )
  )
})

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

test_that("recursive works as expected", {

   data <- '{"item1": {"price" : {"one": 30}}, "item2" : 40, "item3" : 30}' %>% gather_object()
   expected_na <- c(NA_real_, 40, 30)
   expected_val <- c(30, 40, 30)

   expect_identical(
     (data %>% append_values_number(force=TRUE, recursive=FALSE))$number,
     expected_na)
   expect_identical(
     (data %>% append_values_number(force=TRUE, recursive=TRUE))$number,
     expected_val)
   expect_identical(
     (data %>% append_values_number(force=FALSE, recursive=FALSE))$number,
     expected_na)
   expect_error(
     (data %>% append_values_number(force=FALSE, recursive=TRUE))$number)

   data <- '{"item1": {"price" : {"usd" : {"real" : 30}}}, "item2" : 40, "item3" : 30}' %>%
              gather_object()

   expect_identical(
     (data %>% append_values_number(recursive=FALSE))$number,
     expected_na)
   expect_identical(
     (data %>% append_values_number(recursive=TRUE))$number,
     expected_val)

   data <- '{"item1": {"price" : 30, "qty" : 1}, "item2" : 40, "item3" : 30}' %>% gather_object()

   expect_identical(
     (data %>% append_values_number(recursive=FALSE))$number,
     expected_na)
   expect_identical(
     (data %>% append_values_number(recursive=TRUE))$number,
     expected_na)

  }
)

test_that("works for coerced types", {
  skip("this function had undefined and unexpected behavior")
  coerce_helper <- function(input) {
    as.tbl_json(input) %>% gather_array() %>% append_values() %>% .[["values"]]
  }
  
  expect_identical(coerce_helper('["a", 4]'), c("a", "4"))
  expect_identical(coerce_helper('[4, true, false]'), c(4L, 1L, 0L))
  expect_identical(coerce_helper('[4.1, true, false]'), c(4.1, 1, 0))
  expect_identical(coerce_helper('["a", true]'), c("a", "TRUE"))
  expect_identical(coerce_helper('["a", {"a": "b"}]'), list("a", "a" = "b"))
  expect_identical(coerce_helper('["a", {"a": "b", "b": "c"}]'), list("a", "a" = "b"))
})

test_that("my_unlist safely handles edge cases", {

    expect_identical(my_unlist(list(1, NA)), c(1, NA_integer_))
    expect_identical(my_unlist(list("a", NA_character_)), c("a", NA_character_))
    expect_identical(my_unlist(list(1, NULL)), c(1, NA_integer_))
    expect_identical(my_unlist(list(1, list(1, 1))), c(1, NA_integer_))
    expect_identical(my_unlist(list(1, list(1))), list(1, 1))

    expect_identical(my_unlist(list(1, NA), recursive=TRUE), c(1, NA_integer_))
    expect_identical(my_unlist(list(1, list(1)), recursive=TRUE), c(1,1))

  }
)
