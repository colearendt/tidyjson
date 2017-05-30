#' Spreads specific scalar values of a JSON object into new columns
#'
#' The \code{spread_values} function lets you extract extract specific values
#' from (potentiall nested) JSON objects. \code{spread_values} takes
#' \code{\link{json_chr}}, \code{\link{json_dbl}} or \code{\link{json_lgl}} named
#' function calls as arguments in order to specify the type of the data that
#' should be captured at each desired name-value pair location. These values can
#' be of varying types at varying depths.
#'
#' Note that \code{\link{json_chr}}, \code{\link{json_dbl}} and
#' \code{\link{json_lgl}} will fail if they encounter the incorrect type in any
#' document.
#'
#' The advantage of \code{spread_values} over \code{\link{spread_all}} is that
#' you are guaranteed to get a consistent data frame structure (columns and
#' types) out of any \code{spread_values} call. \code{\link{spread_all}}
#' requires less typing, but because it infers the columns and their types from
#' the JSON, it is less suitable when programming.
#'
#' @seealso \code{\link{spread_all}} for spreading all values,
#'          \code{\link[tidyr]{spread}} for spreading data frames,
#'          \code{\link{json_chr}}, \code{\link{json_dbl}},
#'          \code{\link{json_lgl}} for accessing specific names
#' @param .x a json string or \code{\link{tbl_json}} object
#' @param ... \code{column = value} pairs where \code{column} will be the
#'            column name created and \code{value} must be a call to
#'            \code{\link{json_chr}}, \code{\link{json_dbl}} or
#'            \code{\link{json_lgl}} specifying the path to get the value (and
#'            the type implicit in the function name)
#' @return a \code{\link{tbl_json}} object
#' @export
#' @examples
#'
#' # A simple example
#' json <- '{"name": {"first": "Bob", "last": "Jones"}, "age": 32}'
#'
#' # Using spread_values
#' json %>%
#'   spread_values(
#'     first.name = json_chr(name, first),
#'     last.name  = json_chr(name, last),
#'     age        = json_dbl(age)
#'   )
#'
#' # Another document, this time with a middle name (and no age)
#' json2 <- '{"name": {"first": "Ann", "middle": "A", "last": "Smith"}}'
#'
#' # spread_values still gives the same column structure
#' c(json, json2) %>%
#'   spread_values(
#'     first.name = json_chr(name, first),
#'     last.name  = json_chr(name, last),
#'     age        = json_dbl(age)
#'   )
#'
#' # whereas spread_all adds a new column
#' json %>% spread_all
#' c(json, json2) %>% spread_all
spread_values <- function(.x, ...) {

  if (!is.tbl_json(.x)) .x <- as.tbl_json(.x)

  # Get JSON
  json <- attr(.x, "JSON")

  # Get new values
  new_values <- invoke_map(lst(...), .x = list(NULL), json)

  # Add on new values
  y <- dplyr::bind_cols(.x, new_values)

  tbl_json(y, json)

}

#' Factory that creates the json_* functions below
#'
#' @param map.function function to map to collapse
json_factory <- function(map.function) {

  replace_nulls_na <- function(x)
    if (is.null(x)) NA else x

  function(..., recursive = FALSE) {

    path <- path(...)
    if (recursive)  recursive.fun <- unlist
    else            recursive.fun <- identity

    # Return a closure to deal with JSON lists
    function(json) {

      json %>%
        purrr::map(path %>% as.list) %>%
        purrr::map(replace_nulls_na) %>%
        map.function(recursive.fun)

    }

  }

}

#' Navigates nested objects to get at names of a specific type, to be used as
#' arguments to \code{\link{spread_values}}
#'
#' Note that these functions fail if they encounter the incorrect type.
#'
#' @seealso \code{\link{spread_values}} for using these functions to spread
#'          the values of a JSON object into new columns
#' @name json_functions
#' @param ... a quoted or unquoted sequence of strings designating the object
#'            name sequence you wish to follow to find a value
#' @param recursive logical indicating whether second level and beyond objects
#'        should be extracted.  Only works when there exists a single value in
#'        the nested json object
#' @return a function that can operate on parsed JSON data
NULL

#' @rdname json_functions
#' @export
json_chr <- json_factory(map_chr)

#' @rdname json_functions
#' @export
jstring <- function(..., recursive=FALSE) {
  .Deprecated('json_chr')
  json_chr(...)
}
#' @rdname json_functions
#' @export
json_dbl <- json_factory(map_dbl)

#' @rdname json_functions
#' @export
jnumber <- function(..., recursive=FALSE) {
  .Deprecated('json_dbl')
  json_dbl(...)
}

#' @rdname json_functions
#' @export
json_lgl <- json_factory(map_lgl)

#' @rdname json_functions
#' @export
jlogical <- function(..., recursive=FALSE) {
  .Deprecated('json_lgl')
  json_lgl(...)
}
