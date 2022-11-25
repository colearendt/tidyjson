test_that("works with array of length 1", {

    json <- '[{"name": "bob"}]'

    expect_identical(
      json %>% gather_array,
      tbl_json(
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

    json <- '[{"name": "bob"}, {"name": "susan"}]'

    expect_identical(
      json %>% gather_array,
      tbl_json(
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

test_that("works with multiple json", {

    json <- c(
      '[{"name": "bob"}, {"name": "susan"}]',
      '[{"name": "john"}]'
    )

    expect_identical(
      json %>% gather_array,
      tbl_json(
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

test_that("works with value array", {

    json <- c('["a", "b"]')

    expect_identical(
      json %>% gather_array,
      tbl_json(
        data.frame(
          document.id = c(1L, 1L),
          array.index = c(1L, 2L)
        ),
        list("a", "b")
      )
    )

  }
)

test_that("empty json are dropped", {

    json <- c('[{"name": "bob"}]', '[]')

    expect_identical(
      json %>% gather_array,
      tbl_json(
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

test_that("null values are kept", {

    json <- '["string", null]'

    expect_identical(
      json %>% gather_array,
      tbl_json(
        data.frame(
          document.id = c(1L, 1L),
          array.index = c(1L, 2L)
        ),
        list("string", NULL)
      )
    )

  }
)

test_that("objects throws error", {

    json <- c('[{"name": "bob"}]', '{"name": "susan"}')

    expect_error(json %>% gather_array)

  }
)

test_that("values throws error", {

    json <- c('[{"name": "bob"}]', '"bob"')

    expect_error(json %>% gather_array)

  }
)

test_that("correctly handles character(0), {}, []", {

    empty <- tbl_json(
      data.frame(
        document.id = integer(0),
        array.index = integer(0),
        stringsAsFactors = FALSE),
      list())

    expect_identical(
      character(0) %>% gather_array,
      empty)

    expect_error('{}' %>% gather_array)

    expect_identical(
      '[]' %>% gather_array,
      empty)

  }
)

test_that("column.name works and doesn't clobber existing index", {

  expect_identical(
    '["a", "b"]' %>%
      as.tbl_json %>%
      mutate(array.index = 1L) %>%
      gather_array("new"),
    tbl_json(
      dplyr::tibble(
        document.id = rep(1L, 2),
        array.index = rep(1L, 2),
        new = c(1L, 2L)
      ),
      list("a", "b")
    )
  )

}
)

test_that("preserves a NULL column", {

  expect_identical(
    '["a", "b"]' %>%
      as.tbl_json %>%
      mutate(col = list(NULL)) %>%
      gather_array,
    tbl_json(
      tibble(
        document.id = rep(1L, 2),
        col = rep(list(NULL), 2),
        array.index = c(1L, 2L)
      ),
      list("a", "b")
    )
  )

}
)

test_that("can call repeatedly without having to change column.name", {

  expect_identical(
    suppressWarnings('[[1, 2], [3, 4]]' %>% gather_array %>% gather_array),
    tbl_json(
      dplyr::tibble(
        document.id = rep(1L, 4),
        array.index   = c(1L, 1L, 2L, 2L),
        array.index.2 = c(1L, 2L, 1L, 2L)
      ),
      list(1L, 2L, 3L, 4L)
    )
  )

})

test_that("gather_array called multiple times throws a warning", {

  expect_warning('[[1, 2], [3, 4]]' %>% gather_array %>% gather_array)

})


