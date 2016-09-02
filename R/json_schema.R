#' Create a schema for a JSON document or collection
#'
#' Returns a JSON document that captures the 'schema' of the collection of
#' document(s) passed in, as a JSON string. The schema collapses complex
#' JSON into a simple form using the following rules:
#'
#'   string  -> "string",       e.g., "a sentence" -> "string"
#'   number  -> "number",       e.g., 32000.1 -> "number"
#'   true    -> "logical",      e.g., true -> "logical"
#'   false   -> "logical",      e.g., false -> "logical"
#'   null    -> "null",         e.g., null -> "null"
#'   array   -> [<type>]        e.g., [1, 2] -> ["number"]
#'   object  -> {"key": <type>} e.g., {"age": 32} -> {"age": "number"}
#'
#' For more complex JSON objects, ties are broken by taking the most
#' complex example (using \code{json_complexity}), and then by type
#' (using \code{json_types}).
#'
#' This means that if a key has varying schema across documents, the
#' most complex schema will be chosen as being representative. Similarly,
#' if the elements of an array vary in schema, the most complex element is
#' chosen, and if arrays vary in schema across documents, the most
#' complex is chosen.
#'
#' @param x a json string or a tbl_json object
#' @return a character string JSON document that represents the schema of
#'         the collection
#'
#' @export
#' @examples
#'
#' # A simple string
#' '"string"' %>% json_schema
#'
#' # A simple object
#' '{"key": "value"}' %>% json_schema
#'
#' # A complex array is represented by the most complex example
#' '[{"a": 1}, [1, 2], "a", 1, true, null]' %>% json_schema
#'
#' # Companies example
#' companies[1] %>% json_schema %>% plot_json_graph
#'
#' # Github issues
#' issues_array <- issues %>% gather_array # issues are one large array
#' issues_schema <- issues_array[1:5, ] %>% json_schema # analyze first 5
#' issues_schema %>% plot_json_graph
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

  data_frame(schemas = schema) %>%
    mutate(json = schemas) %>%
    as.tbl_json(json.column = "json") %>%
    json_types %>%
    json_complexity %>%
    tbl_df %>%
    arrange(desc(complexity), type) %>%
    slice(1) %>%
    `[[`("schemas") %>%
    paste(collapse = ", ") %>%
    sprintf("[%s]", .)

}

json_schema_object <- function(json) {

  x <- json %>% list_to_tbl_json %>% gather_keys

  x$schemas <- attr(x, "JSON") %>% map(list_to_tbl_json) %>% map_chr(json_schema)

  schemas <- x %>% select(key, schemas) %>% unique

  schemas

}

collapse_object <- function(schema) {

  schema %>%
    mutate(json = schemas) %>%
    as.tbl_json(json.column = "json") %>%
    json_types %>%
    json_complexity %>%
    tbl_df %>%
    group_by(key) %>%
    arrange(desc(complexity), type) %>%
    slice(1) %>%
    ungroup %>%
    mutate(key = key %>% sprintf('"%s"', .)) %>%
    mutate(schemas = map2(key, schemas, paste, sep = ": ")) %>%
    `[[`("schemas") %>%
    paste(collapse = ", ") %>%
    sprintf("{%s}", .)

}
