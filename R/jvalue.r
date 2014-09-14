
#' Extracts values from x
#' @export
jvalue <- function(x, ...) {
  
  assert_that(is.jdf(x))
  
  # Get JSON
  json <- attr(x, "JSON")
  
  # Get new values
  new_values <- lapply(list(...), function(f) f(json))
  
  # Add onto existing x
  structure(
    data.frame(x, new_values, stringsAsFactors = FALSE),
    JSON = json,
    class = c("jdf", "data.frame")
  )
  
}

#' Specifies a path to extract for strings
#' @export
jstring <- function(...) {
  
  # Prepare path
  path <- prep_path(...)

  # Return a closure to deal with JSON lists
  function(json)
    as.character(list_path(json, path))
  
}

#' Specifies a path to extract for numbers
#' @export
jnumber <- function(...) {
  
  # Prepare path
  path <- prep_path(...)

  # Return a closure to deal with JSON lists
  function(json)
    as.numeric(list_path(json, path))
  
}

#' Specifies a path to extract for logicals
#' @export
jlogical <- function(...) {
  
  # Prepare path
  path <- prep_path(...)

  # Return a closure to deal with JSON lists
  function(json)
    as.logical(list_path(json, path))
  
}

#' Prepare a path from ...
prep_path <- function(...) {
  
  # Conver to a list
  path <- list(...)
  
  # Check type
  path_is_char <- vapply(path, inherits, logical(1), "character")
  path_len_1 <- vapply(path, length, integer(1)) == 1
  if (!all(path_is_char & path_len_1))
    stop("malformed path")
  
  # Unlist
  path <- unlist(path)
  
  path
}

#' Recursively access a path
list_path <- function(l, path) {
  
  # Unwind this step
  l <- lapply(l, "[[", path[1])
  
  # Keep going if more remains in the path 
  if (length(path) > 1)
    l <- list_path(l, path[-1])
  
  l
}