#' Create new columns with JSON values
#'
#' The spread_values() function lets you dive into (potentially nested) JSON
#' objects and extract specific values. spread_values() takes jstring(),
#' jnumber() or jlogical() named function calls as arguments in order to specify
#' the type of the data that should be captured at each desired key location.
#' These values can be of varying types at varying depths.
#'
#' @param x tbl_json object
#' @param ... column=value list where 'column' will be the column name created
#'   and 'value' must be a call to jstring(), jnumber() or jlogical() specifying
#'   the path to get the value (and the type implicit in the function name)
#' @export
#' @examples
#' library(magrittr)  # for %>%
#' '{"name": {"first": "bob", "last": "jones"}, "age": 32}' %>%
#'   spread_values(
#'     first.name = jstring("name", "first"),
#'     age = jnumber("age")
#'   )
spread_values <- function(x, ...) {

  if (!is.tbl_json(x)) x <- as.tbl_json(x)

  # Get JSON
  json <- attr(x, "JSON")

  # Get new values
  new_values <- map(lst(...), function(f) f(json))

  # Add on new values
  x <- data.frame(x, new_values, stringsAsFactors = FALSE)

  tbl_json(x, json)

}

#' Factory that creates the j* functions below
#'
#' @param na.value value to replace NULL with
#' @param conversion.function function to convert vector to appropriate type
jfactory <- function(na.value, conversion.function) {

  function(..., recursive = FALSE) {

    # Prepare path
    path <- list(...)

    # Return a closure to deal with JSON lists
    function(json) {
      data <- json %>%
        map(path) %>%
        map(`%||%`, na.value)
      if (!recursive) {
         conversion.function(data)
      } else {
         # This must be vapply as we do not know which map_* function to call
         vapply(data, function(d) conversion.function(unlist(d)),
                FUN.VALUE = conversion.function(0))
      }
    }

  }

}

#' Navigates nested objects to get at keys of a specific type, to be used as
#' arguments to spread_values
#'
#' @name jfunctions
#' @param ... the path to follow
#' @param recursive logical indicating whether second level and beyond objects
#'        should be extracted.  Only works when there exists a single value in
#'        the nested json object
#' @return a function that can operate on parsed JSON data
NULL

#' @rdname jfunctions
#' @export
jstring <- jfactory(NA_character_, as.character)

#' @rdname jfunctions
#' @export
jnumber <- jfactory(NA_real_, as.numeric)

#' @rdname jfunctions
#' @export
jlogical <- jfactory(NA, as.logical)
