#' Selects an object by key and filters rows to just those with matching keys
#' 
#' @param x a tbl_json object
#' @param ... path to filter
#' @export
enter_object <- function(x, ...) {
  
  if (!is.tbl_json(x)) x <- as.tbl_json(x)
  
  # Prepare path
  path <- prep_path(...)
  
  # Extract json
  json <- attr(x, "JSON")
  
  # Access path
  json <- list_path(json, path)
  
  tbl_json(x, json, drop.null.json = TRUE)
  
}