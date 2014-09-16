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
  
  # Ensure not values
  not_list <- vapply(json, is.list, logical(1))
  if (any(!not_list))
    stop(sprintf("%s records are values not arrays", sum(!not_list)))
  
  # Ensure not objects
  null_names <- vapply(json, function(l) is.null(names(l)), logical(1))
  if (any(!null_names))
    stop(sprintf("%s records are objects not arrays", sum(!null_names)))
  
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