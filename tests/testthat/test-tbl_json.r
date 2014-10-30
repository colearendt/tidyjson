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
