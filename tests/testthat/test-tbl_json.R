context("tbl_json: as.tbl_json.character")

test_that("correctly parses length(json) == 1", {
  expect_identical(
    as.tbl_json('{"name": "bob", "age": 32}'),
    tbl_json(
      data.frame(document.id = 1L),
      list(list(name = "bob", age = 32L))
    )
  )
})

test_that("correctly parses length(json) > 1", {
  expect_identical(
    as.tbl_json(
      c('{"name": "bob", "age": 32}',
        '{"name": "susan", "age": 25}')
    ),
    tbl_json(
      data.frame(document.id = 1L:2L),
      list(
        list(name = "bob", age = 32L),
        list(name = "susan", age = 25L)
      )
    )
  )
})

test_that("currectly parses character(0)", {
  expect_identical(
    as.tbl_json(character(0)),
    tbl_json(
      data.frame(document.id = integer(0)),
      list()
    )
  )
})

test_that("correctly parses empty objects", {

  nl <- list()
  names(nl) <- character(0)

  expect_identical(
    as.tbl_json(c('[]', '{}')),
    tbl_json(
      data.frame(document.id = 1L:2L),
      list(list(), nl)
    )
  )

})

test_that("currectly structures an array", {
  expect_identical(
    as.tbl_json('[{"name": "bob"}, {"name": "susan"}]'),
    tbl_json(
      data.frame(document.id = 1L),
      list(list(list(name = "bob"), list(name = "susan")))
    )
  )
})

test_that("throws error on invalid json", {

    expect_error(as.tbl_json(''))

  }
)

context("tbl_json: as.character.tbl_json")

inverts_json_test <- function(json) {
  expect_identical(json, json %>% as.tbl_json %>% as.character)
}

test_that("works for simple cases", {

  inverts_json_test('"a"')
  inverts_json_test('1')
  inverts_json_test('true')
  inverts_json_test('false')
  inverts_json_test('null')
  inverts_json_test('{}')
  inverts_json_test('[]')

})

test_that("works for more complex cases", {

  inverts_json_test('{"name":"a"}')
  inverts_json_test('{"name":1}')
  inverts_json_test('{"name":[1]}')
  inverts_json_test('{"name":[null]}')
  inverts_json_test('{"name":null}')
  inverts_json_test('[[1,2],1]')

})

test_that("works for worldbank data", {

  inverts_json_test(worldbank[1:5])

})

context("print.tbl_json")

test_that("print.tbl_json works for a simple case", {

  expect_identical(
    capture.output(print(as.tbl_json('"a"'))),
    c('# A tbl_json: 1 x 1 tibble with a \"JSON\" attribute',
      '  `attr(., "JSON")` document.id',
      '              <chr>       <int>',
      '1               "a"           1')
  )

})

test_that("print.tbl_json json.width works correctly", {

  expect_identical(
    capture.output(print(as.tbl_json('"12345"'), json.width = 4)),
    c('# A tbl_json: 1 x 1 tibble with a \"JSON\" attribute',
      '  `attr(., "JSON")` document.id',
      '              <chr>       <int>',
      '1           "123...           1')
  )

})

test_that("print.tbl_json json.n works correctly", {

  expect_identical(
    capture.output(print(as.tbl_json(c('"a"', '"b"')), json.n = 1)),
    c('# A tbl_json: 2 x 1 tibble with a \"JSON\" attribute',
      '  `attr(., "JSON")` document.id',
      '              <chr>       <int>',
      '1               "a"           1',
      '2               ...           2')
  )

})

context("tbl_json: as.tbl_json.data.frame")

test_that("works for a data.frame and data_frame created objects", {

    df <- data.frame(
      document.id = 1:2,
      json = c('{"name": "bob"}', '{"name": "susan"}'),
      stringsAsFactors = FALSE)
    # data.frame
    expect_identical(
      as.tbl_json(df, json.column = "json"),
      as.tbl_json(df$json)
    )
    # data_frame
    df <- data_frame(
      document.id = 1:2,
      json = c('{"name": "bob"}', '{"name": "susan"}'))
    expect_identical(
      as.tbl_json(df, json.column = "json"),
      as.tbl_json(df$json)
    )

  }
)

test_that("works in a pipeline", {

    df <- data_frame(
      age = c(32, 45),
      json = c('{"name": "bob"}', '{"name": "susan"}')
    )

    expect_identical(
      df %>% as.tbl_json(json.column = "json") %>%
        spread_values(name = jstring("name")) %>%
        filter(age == 32) %>%
        `[[`("name"),
      "bob"
    )

  }
)

test_that("throws an error without json.column specified", {
    expect_error(as.tbl_json(iris))
  }
)

test_that("throws an error if json column doesn't exist", {
    expect_error(as.tbl_json(iris, json.column = "json"))
  }
)

context("tbl_json")

test_that("tbl_json constructor works with no data", {

    expect_identical(tbl_json(data.frame(), list()) %>% nrow, 0L)

  }
)

test_that("tbl_json fails if ..JSON is in the names of the data.frame", {

    expect_error(tbl_json(data.frame(..JSON = character(0)), list()))

  }
)

test_that("[ row filtering works with a simple example", {

    expect_identical(
      as.tbl_json(c('{"name": "bob"}', '{"name": "susan"}'))[1, ],
      tbl_json(
        data.frame(document.id = 1L),
        list(list(name = "bob"))
      )
    )

  }
)

test_that("[ column filtering doesn't change the JSON", {

    x <- c(
      '{"name": "bob", "children": [{"name": "george"}]}',
      '{"name": "susan", "children": [{"name": "sally"}, {"name": "bobby"}]}'
        ) %>% as.tbl_json %>%
      spread_values("parent" = jstring("name")) %>%
      enter_object("children") %>%
      gather_array %>%
      spread_values("child" = jstring("name"))

    expect_identical(
      attr(x, "JSON"),
      attr(x[, c("parent", "child")], "JSON")
    )

  }
)

context("tbl_json: dplyr verbs")

test_that("dplyr::filter works with a simple example", {

    x <- as.tbl_json(c('{"name": "bob"}', '{"name": "susan"}'))

    expect_identical(
      filter(x, document.id == 1),
      tbl_json(
        data.frame(document.id = 1L),
        list(list(name = "bob"))
      )
    )

  }
)

test_that("dplyr::filter works in a more complex pipeline", {

    json <- c(
      '{"name": "bob", "children": [{"name": "george"}]}',
      '{"name": "susan", "children": [{"name": "sally"}, {"name": "bobby"}]}'
        )
    susan.children <- json %>% as.tbl_json %>%
      spread_values(name = jstring("name")) %>%
      filter(name == "susan") %>%
      enter_object("children") %>%
      gather_array %>%
      spread_values(child = jstring("name"))

    expect_identical(susan.children$child, c("sally", "bobby"))

  }
)

test_that("dplyr::arrange works with a simple example", {

    x <- as.tbl_json(c('{"name": "bob"}', '{"name": "susan"}'))

    expect_identical(
      x %>% arrange(desc(document.id)),
      tbl_json(
        data.frame(document.id = c(2L, 1L)),
        list(list(name = "susan"), list(name = "bob"))
      )
    )

  }
)

test_that("dplyr::mutate works with a simple example", {

    x <- as.tbl_json(c('{"name": "bob"}', '{"name": "susan"}'))

    expect_identical(
      x %>%
        spread_values(name = jstring("name")) %>%
        mutate(fullname = paste(name, "green")),
      tbl_json(
        data_frame(
          document.id = c(1L, 2L),
          name = c("bob", "susan"),
          fullname = c("bob green", "susan green")),
        list(list(name = "bob"), list(name = "susan"))
      )
    )

  }
)

test_that("dplyr::mutate works in a more complex pipeline", {

    json <- c(
      '{"name": "bob", "children": [{"name": "george"}]}',
      '{"name": "susan", "children": [{"name": "sally"}, {"name": "bobby"}]}')

    children <- json %>% as.tbl_json %>%
      spread_values(name = jstring("name")) %>%
      mutate(parent.rank = rank(name)) %>%
      enter_object("children") %>%
      gather_array %>%
      spread_values(child = jstring("name"))

    expect_identical(children$parent.rank, c(1, 2, 2))
    expect_identical(children$child, c("george", "sally", "bobby"))

  }
)

test_that("dplyr::slice works", {

  new <- '[1, 2, 3]' %>% gather_array %>% slice(1:2)

  expect_is(new, "tbl_json")
  expect_identical(nrow(new), 2L)
  expect_identical(length(attr(new, "JSON")), 2L)

})

test_that("dplyr::rename works", {

  new <- '[1, 2, 3]' %>% gather_array %>% rename(blah = document.id)

  expect_is(new, "tbl_json")
  expect_identical(names(new), c("blah", "array.index"))

})

test_that("dplyr::transmute works", {

  new <- '[1, 2, 3]' %>% gather_array %>% transmute(blah = document.id)

  expect_is(new, "tbl_json")
  expect_identical(names(new), "blah")

})

test_that("dplyr::sample_n works", {

  new <- '[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]' %>% gather_array %>% sample_n(2)

  expect_is(new, "tbl_json")
  expect_identical(new$array.index, attr(new, "JSON") %>% flatten_int)

})


