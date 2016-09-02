json_schema <- function(x) {

  if (!is.tbl_json(x)) x <- as.tbl_json(x)

  x <- x %>% json_types
  json <- attr(x, "JSON")

  schema <- character(nrow(x))

  is_array <- x$type == "array"
  is_object <- x$type == "object"
  is_scalar <- !is_array & !is_object

  if (any(is_array))
    schema[is_array] <- json[is_array] %>% map_chr(json_schema_array)

  if (any(is_object))
    schema[is_object] <- json[is_object] %>% map_chr(json_schema_object)

  if (any(is_scalar))
    schema[is_scalar] <- x$type %>% as.character %>% sprintf('"%s"', .)

  schema

}

list_to_tbl_json <- function(l) {

  tbl_json(data_frame(document.id = 1L), list(l))

}

json_schema_array <- function(json) {

  x <- json %>% list_to_tbl_json %>% gather_array

  schemas <- attr(x, "JSON") %>% map(list_to_tbl_json) %>% map_chr(json_schema)

  schemas <- schemas %>% unique

  schemas %>% paste(collapse = ", ") %>% sprintf("[%s]", .)

}

json_schema_object <- function(json) {

  x <- json %>% list_to_tbl_json %>% gather_keys

  x$schemas <- attr(x, "JSON") %>% map(list_to_tbl_json) %>% map_chr(json_schema)

  schemas <- x %>% select(key, schemas) %>% unique

  schemas %>%
    mutate(key = key %>% sprintf('"%s"', .)) %>%
    mutate(schemas = map2(key, schemas, paste, sep = ": ")) %>%
    `[[`("schemas") %>%
    paste(collapse = ", ") %>%
    sprintf("{%s}", .)

}
