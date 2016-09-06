#' Create a schema for a JSON document or collection
#'
#' Returns a JSON document that captures the 'schema' of the collection of
#' document(s) passed in, as a JSON string. The schema collapses complex
#' JSON into a simple form using the following rules:
#'
#' \itemize{
#'   \item string  -> "string",       e.g., "a sentence" -> "string"
#'   \item number  -> "number",       e.g., 32000.1 -> "number"
#'   \item true    -> "logical",      e.g., true -> "logical"
#'   \item false   -> "logical",      e.g., false -> "logical"
#'   \item null    -> "null",         e.g., null -> "null"
#'   \item array   -> [<type>]        e.g., [1, 2] -> ["number"]
#'   \item object  -> {"key": <type>} e.g., {"age": 32} -> {"age": "number"}
#' }
#'
#' For more complex JSON objects, ties are broken by taking the most
#' complex example (using \code{\link{json_complexity}}), and then by type
#' (using \code{\link{json_types}}).
#'
#' This means that if a key has varying schema across documents, the
#' most complex schema will be chosen as being representative. Similarly,
#' if the elements of an array vary in schema, the most complex element is
#' chosen, and if arrays vary in schema across documents, the most
#' complex is chosen.
#'
#' Note that \code{json_schema} can be slow for large JSON document collections,
#' you may want to sample your JSON collection first.
#'
#' @seealso \code{\link{json_structure}} to recursively structure all
#'          documents into a single data frame,
#'          \code{\link{plot_json_graph}} to plot JSON (including results
#'          of \code{json_schema} as a graph
#' @param .x a json string or \code{\link{tbl_json}} object
#' @param type whether to capture scalar nodes using the string that defines
#'        their type (e.g., "logical") or as a representative value
#'        (e.g., "true"), useful in conjunction with plot_json_graph
#' @return a character string JSON document that represents the schema of
#'         the collection
#' @export
#' @examples
#'
#' # A simple string
#' '"string"' %>% json_schema %>% writeLines
#'
#' # A simple object
#' '{"key": "value"}' %>% json_schema %>% writeLines
#'
#' # A more complex JSON array
#' json <- '[{"a": 1}, [1, 2], "a", 1, true, null]'
#'
#' # Using type = 'string' (default)
#' json %>% json_schema %>% writeLines
#'
#' # Using type = 'value' to show a representative value
#' json %>% json_schema(type = "value") %>% writeLines
#'
#' # Plotting the schema of a company example
#' companies[1] %>% json_schema(type = "value") %>% plot_json_graph
#'
#' # Schema of the first 5 github issues
#' library(dplyr)
#' issues %>% gather_array %>% slice(1:10) %>%
#'   json_schema(type = "value") %>% plot_json_graph
json_schema <- function(.x, type = c("string", "value")) {

  type <- match.arg(type)

  if (!is.tbl_json(.x)) .x <- as.tbl_json(.x)

  .x <- .x %>% json_types
  json <- attr(.x, "JSON")

  schema <- list_along(json)

  is_array <- .x$type == "array"
  is_object <- .x$type == "object"
  is_scalar <- !is_array & !is_object

  if (any(is_array)) {

    array_schema <- json[is_array] %>% map(json_schema_array, type)

    array_schema <- array_schema %>%
      unlist(recursive = FALSE) %>%
      unique

    array_schema <- collapse_array(array_schema)

    schema[is_array] <- array_schema

  }

  if (any(is_object)) {

    object_schema <- json[is_object] %>% map(json_schema_object, type)

    object_schema <- object_schema %>%
      bind_rows %>%
      tbl_df %>%
      unique

    object_schema <- collapse_object(object_schema)

    schema[is_object] <- object_schema

  }

  if (any(is_scalar)) {

    if (type == "string")
      schema[is_scalar] <- .x$type %>% as.character %>% sprintf('"%s"', .)
    if (type == "value") {
      type_map <- list(string = '"string"', number = '1',
                       logical = 'true', null = 'null')
      schema[is_scalar] <- type_map[as.character(.x$type)]
    }

  }

  schema <- schema %>% unique

  if (length(schema) == 1)
    schema <- schema[[1]]

  schema

}

list_to_tbl_json <- function(l) {

  tbl_json(data_frame(document.id = 1L), list(l))

}

json_schema_array <- function(json, type) {

  x <- json %>% list_to_tbl_json %>% gather_array

  schemas <- attr(x, "JSON") %>% map(list_to_tbl_json) %>%
    map_chr(json_schema, type)

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
    extract2("schemas") %>%
    paste(collapse = ", ") %>%
    sprintf("[%s]", .)

}

json_schema_object <- function(json, type) {

  x <- json %>% list_to_tbl_json %>% gather_keys

  x$schemas <- attr(x, "JSON") %>% map(list_to_tbl_json) %>%
    map_chr(json_schema, type)

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
    extract2("schemas") %>%
    paste(collapse = ", ") %>%
    sprintf("{%s}", .)

}
