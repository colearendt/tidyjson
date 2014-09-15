#' Extracts values from JSON refereced by a sequence of keys
#' @param x tbl_json object
#' @param ... column=value list where 'column' will be the column name created
#'   and 'value' must be a call to jstring(), jnumber() or jlogical() specifying
#'   the path to get the value (and the type implicit in the function name) 
#' @export
spread_values <- function(x, ...) {
  
  assert_that(is.tbl_json(x))
  
  # Get JSON
  json <- attr(x, "JSON")
  
  # Get new values
  new_values <- lapply(list(...), function(f) f(json))
  
  # Add on new values
  x <- data.frame(x, new_values, stringsAsFactors = FALSE)
  
  tbl_json(x, json)
  
}

#' Specifies a path to extract for strings
#' @export
jstring <- function(...) {
  
  # Prepare path
  path <- prep_path(...)

  # Return a closure to deal with JSON lists
  function(json) {
    data <- list_path(json, path)
    data <- replace_nulls(data, NA_character_)
    as.character(data)
  }
  
}

#' Specifies a path to extract for numbers
#' @export
jnumber <- function(...) {
  
  # Prepare path
  path <- prep_path(...)

  # Return a closure to deal with JSON lists
  function(json) {
    data <- list_path(json, path)
    data <- replace_nulls(data, NA_real_)
    as.numeric(data)
  }
  
}

#' Specifies a path to extract for logicals
#' @export
jlogical <- function(...) {
  
  # Prepare path
  path <- prep_path(...)

  # Return a closure to deal with JSON lists
  function(json) {
    data <- list_path(json, path)
    data <- replace_nulls(data, NA)
    as.logical(data)
  }
  
}