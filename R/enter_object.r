#' Selects an object by key and filters rows to just those with matching keys
#' 
#' @param ... path to filter
#' @export
enter_object <- function(x, ...) {
  
  assert_that(is.jdf(x))
  
  # Prepare path
  path <- prep_path(...)
  
  # Extract json
  json <- attr(x, "JSON")
  
  # Access path
  json <- list_path(json, path)
  
  jdf(x, json)
  
}