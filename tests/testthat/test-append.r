context("append_string")

test_that("works with simple input", {
    
    json <- '[{"name": "anne"}, {"name": "bob"}, {"name": "george"}]'

    expect_identical(
      json %>% as.tbl_json %>% gather_array %>% gather_keys %>% append_string,
      tbl_json(
        data.frame(
          document.id = c(1L, 1L, 1L),
          array.index = 1L:3L,
          keys = rep("name", 3),
          string = c("anne", "bob", "george"),
          stringsAsFactors = FALSE
        ),
        list("anne", "bob", "george")
      )
    )
    
  }
)