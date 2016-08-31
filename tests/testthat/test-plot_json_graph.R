context("plot_json_graph")

colors <- RColorBrewer::brewer.pal(6, "Accent")
names(colors) <- allowed_json_types

test_that("works with a simple object", {

  expect_silent(g <- '{"key": "value"}' %>% plot_json_graph)
  expect_is(g, "igraph")
  expect_identical(V(g)$vertex.label, c(NA_character_, "key"))
  expect_identical(V(g)$vertex.color, unname(colors[c("object", "string")]))

})

test_that("works with a simple array", {

  expect_silent(g <- '[1, "a", false, null]' %>% plot_json_graph)
  expect_is(g, "igraph")
  expect_identical(V(g)$vertex.label, rep(NA_character_, 5))
  expect_identical(V(g)$vertex.color,
                   unname(colors[c("array", "number", "string", "logical", "null")]))

})

test_that("works with a nested object", {

  expect_silent(g <- '{"k1": [{"k2": [{"k3": ["value"]}]}]}' %>% plot_json_graph)
  expect_is(g, "igraph")
  expect_identical(V(g)$vertex.label, c(NA_character_,
                                        "k1", NA_character_,
                                        "k2", NA_character_,
                                        "k3", NA_character_))
  expect_identical(V(g)$vertex.color,
                   unname(colors[c("object", "array", "object", "array",
                                   "object", "array", "string")]))

})

test_that("works with empty JSON", {

  expect_silent('null' %>% plot_json_graph)
  expect_silent('[]' %>% plot_json_graph)
  #expect_silent('{}' %>% plot_json_graph)
  #expect_silent(character(0) %>% plot_json_graph)

})

test_that("fails with multiple documents", {

  expect_error(c('1', '2') %>% plot_json_graph)

})

test_that("fails with multiple documents", {

  expect_error(c('1', '2') %>% plot_json_graph)

})
