#' Append keys to a new column
#' 
#' @name append_values
NULL

#' @export
#' @rdname append_values
append_values_string <- function(x, column.name = "string") {
  
  assert_that(is.tbl_json(x))
  
  # Extract json 
  json <- attr(x, "JSON")
  
  # Determine types
  types <- determine_types(json)
  
  # Initialize column to NAs
  column <- rep(NA_character_, nrow(x))
  
  # Identify correct types
  correct <- types == "string"
  
  # Set correct to data
  column[correct] <- unlist(json[correct])
  
  # Add as a column to x
  x[column.name] <- column
  
  tbl_json(x, json)
  
}

#' @export
#' @rdname append_values
append_values_number <- function(x, column.name = "number") {
  
}

#' @export
#' @rdname append_values
append_values_logical <- function(x, column.name = "logical") {
  
}
