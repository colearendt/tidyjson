json_schema <- function(x) {

  if (!is.tbl_json(x)) x <- as.tbl_json(x)

  x <- x %>% json_types
  json <- attr(x, "JSON")

  schema <- list_along(json)

  is_array <- x$type == "array"
  is_object <- x$type == "object"
  is_scalar <- !is_array & !is_object

  if (any(is_array)) {

    array_schema <- json[is_array] %>% map(json_schema_array)

    array_schema <- array_schema %>%
      unlist(recursive = FALSE) %>%
      unique

    array_schema <- collapse_array(array_schema)

    schema[is_array] <- array_schema

  }

  if (any(is_object)) {

    object_schema <- json[is_object] %>% map(json_schema_object)

    object_schema <- object_schema %>%
      bind_rows %>%
      tbl_df %>%
      unique

    object_schema <- collapse_object(object_schema)

    schema[is_object] <- object_schema

  }

  if (any(is_scalar)) {

    schema[is_scalar] <- x$type %>% as.character %>% sprintf('"%s"', .)

  }

  schema <- schema %>% unique

  if (length(schema) == 1)
    schema <- schema[[1]]

  schema

}

list_to_tbl_json <- function(l) {

  tbl_json(data_frame(document.id = 1L), list(l))

}

json_schema_array <- function(json) {

  x <- json %>% list_to_tbl_json %>% gather_array

  schemas <- attr(x, "JSON") %>% map(list_to_tbl_json) %>% map_chr(json_schema)

  schemas <- schemas %>% unique

  schemas

}

collapse_array <- function(schema) {

  schema %>% paste(collapse = ", ") %>% sprintf("[%s]", .)

}

json_schema_object <- function(json) {

  x <- json %>% list_to_tbl_json %>% gather_keys

  x$schemas <- attr(x, "JSON") %>% map(list_to_tbl_json) %>% map_chr(json_schema)

  schemas <- x %>% select(key, schemas) %>% unique

  schemas

}

collapse_object <- function(schema) {

  schema %>%
    arrange(key, schemas) %>%
    mutate(key = key %>% sprintf('"%s"', .)) %>%
    mutate(schemas = map2(key, schemas, paste, sep = ": ")) %>%
    `[[`("schemas") %>%
    paste(collapse = ", ") %>%
    sprintf("{%s}", .)

}
