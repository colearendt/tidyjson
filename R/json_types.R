#' Add a column that tells the 'type' of the JSON data
#'
#' The function \code{json_types} inspects the JSON associated with
#' each row of the \code{\link{tbl_json}} object, and adds a new column
#' (\code{"type"} by default) that identifies the type according to the
#' JSON standard at \url{http://json.org/}.
#'
#' This is particularly useful for inspecting your JSON data types, and can
#' often follows after \code{\link{gather_array}}, \code{\link{gather_object}}
#' or \code{\link{enter_object}} to inspect the types of the elements of
#' JSON objects or arrays.
#'
#' @param .x a json string or tbl_json object
#' @param column.name the name to specify for the type column
#' @return a \code{\link{tbl_json}} object
#' @export
#' @examples
#'
#' # A simple example
#' c('{"a": 1}', '[1, 2]', '"a"', '1', 'true', 'null') %>% json_types
#'
#' # Type distribution in the first 10 companies
#' library(dplyr)
#' companies[1:10] %>% gather_object %>% json_types %>% count(type)
json_types <- function(.x, column.name = "type") {

  if (!is.tbl_json(.x)) .x <- as.tbl_json(.x)

  # Extract json
  json <- json_get(.x)

  # Determine types
  types <- determine_types(json)

  # Add as a column to x
  .x[column.name] <- types

  tbl_json(.x, json)

}

#' Fundamental JSON types from http://json.org/, where I collapse 'true' and
#' 'false' into 'logical'
allowed_json_types <-
  c("object", "array", "string", "number", "logical", "null")

#' Determines the types of a list of parsed JSON
#' @param json_list a list of parsed JSON
#' @return a factor with levels json_types
#' @keywords internal
determine_types <- function(json_list) {

  # Get classes
  classes <- purrr::map_chr(json_list, class)

  # Check existence of names
  names <- purrr::map_lgl(json_list, function(x) !is.null(attr(x, "names")))

  # Check if it's a list
  is_list <- classes == "list"

  # Fix class names
  classes[is_list & names] <- "object"
  classes[is_list & !names] <- "array"
  classes[classes == "character"] <- "string"
  classes[classes == "integer"] <- "number"
  classes[classes == "numeric"] <- "number"
  classes[classes == "NULL"] <- "null"

  # Turn into a factor
  factor(classes, levels = allowed_json_types)

}
