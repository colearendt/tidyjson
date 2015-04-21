context("gather_keys")

test_that("works in a simple case", {
    
    json <- '{"key1": 1, "key2": 2}'

    expect_identical(
      json %>% gather_keys,
      tbl_json(
        data.frame(
          document.id = c(1L, 1L),
          key = c("key1", "key2"),
          stringsAsFactors = FALSE
        ),
        list(1L, 2L)
      )
    )
    
  }
)

test_that("works with compound values", {
    
    json <- '{
		  "key1": 1,
      "key2": {"sub": "a"},
      "key3": [true, false],
      "key4": null
		}'

    expect_identical(
      json %>% gather_keys,
      tbl_json(
        data.frame(
          document.id = c(1L, 1L, 1L, 1L),
          key = c("key1", "key2", "key3", "key4"),
          stringsAsFactors = FALSE
        ),
        list(1L, list(sub = "a"), list(TRUE, FALSE), NULL)
      )
    )
    
  }
)

test_that("throws errors with incorrect types", {
    
    expect_error('1' %>% gather_keys())
    expect_error('["a"]' %>% gather_keys())
    expect_error('null' %>% gather_keys())
    
  }
)

test_that("correctly handles character(0), {}, []", {
    
    empty <- tbl_json(
      data.frame(
        document.id = integer(0),
        key = character(0),
        stringsAsFactors = FALSE),
      list())
    
    expect_identical(
      character(0) %>% gather_keys,
      empty)
    
    expect_identical(
      '{}' %>% gather_keys,
      empty
    )
    
    expect_error('[]' %>% gather_keys)

  }
)