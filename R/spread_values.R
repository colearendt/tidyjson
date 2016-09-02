#' Create new columns with JSON values
#'
#' The spread_values() function lets you dive into (potentially nested) JSON
#' objects and extract specific values. spread_values() takes jstring(),
#' jnumber() or jlogical() named function calls as arguments in order to specify
#' the type of the data that should be captured at each desired key location.
#' These values can be of varying types at varying depths.
#'
#' Note that jstring, jnumber and jlogical will fail if they encounter the
#' incorrect type in any document
#'
#' @param .x a json string or tbl_json object
#' @param ... column=value list where 'column' will be the column name created
#'   and 'value' must be a call to jstring(), jnumber() or jlogical() specifying
#'   the path to get the value (and the type implicit in the function name)
#' @export
#' @examples
#' '{"name": {"first": "bob", "last": "jones"}, "age": 32}' %>%
#'   spread_values(
#'     first.name = jstring("name", "first"),
#'     age = jnumber("age")
#'   )
spread_values <- function(.x, ...) {

  if (!is.tbl_json(.x)) .x <- as.tbl_json(.x)

  # Get JSON
  json <- attr(.x, "JSON")

  # Get new values
  new_values <- invoke_map(lst(...), .x = list(NULL), json)

  # Add on new values
  y <- bind_cols(.x, new_values)

  tbl_json(y, json)

}

#' Factory that creates the j* functions below
#'
#' @param map.function function to map to collapse
jfactory <- function(map.function) {

  function(..., recursive = FALSE) {

    if (recursive)  recursive.fun <- unlist
    else            recursive.fun <- identity

    # Return a closure to deal with JSON lists
    function(json) {

      json %>%
        map(list(...)) %>%
        map(`%||%`, NA) %>%
        map.function(recursive.fun)

    }

  }

}

#' Navigates nested objects to get at keys of a specific type, to be used as
#' arguments to spread_values
#'
#' Note that these functions fail if they encounter the incorrect type.
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
jstring <- jfactory(map_chr)

#' @rdname jfunctions
#' @export
jnumber <- jfactory(map_dbl)

#' @rdname jfunctions
#' @export
jlogical <- jfactory(map_lgl)
