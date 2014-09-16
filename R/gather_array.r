#' Expands a tbl_json to span the indices of a JSON array
#' 
#' @param x a tbl_json whose JSON attribute should always be an array
#' @param column.name the name to give to the array index column created
#' @return a tbl_json with a new column (column.name) that captures the array
#'   index and JSON attribute extracted from the array
#' @export
gather_array <- function(x, column.name = "array.index") {
  
  assert_that(is.tbl_json(x))
  
  # Get JSON
  json <- attr(x, "JSON")
  
  # Determine types
  types <- determine_types(json)
  
  # Check if not arrays
  not_arrays <- types != "array"
  if (any(not_arrays))
    stop(sprintf("%s records are values not arrays", sum(not_arrays)))
  
  # Get array lengths
  lengths <- vapply(json, length, integer(1))
  
  # Compute indices
  indices <- rep(seq_along(json), lengths)
  
  # Expand x
  y <- x[indices, , drop = FALSE]

  # Add sequence column
  y[column.name] <- sequence(lengths)
  
  # Unwind JSON one level
  json <- unlist(json, recursive = FALSE)
  
  # Construct tbl_json
  tbl_json(y, json)
  
}