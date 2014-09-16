#' Gathers every key from the top level of the json and stacks them 
#' 
#' @param x a tbl_json whose JSON attribute should always be an object
#' @param column.name the name to give to the column of key names created
#' @return a tbl_json with a new column (column.name) that captures the keys
#'   and JSON attribute of the associated value data
#' @export
gather_keys <- function(x, column.name = "keys") {

  assert_that(is.tbl_json(x))
  
  # Get JSON
  json <- attr(x, "JSON")
  
  # Ensure not values
  not_list <- vapply(json, is.list, logical(1))
  if (any(!not_list))
    stop(sprintf("%s records are values not arrays", sum(!not_list)))
  
  # Ensure not array
  null_names <- vapply(json, function(l) is.null(names(l)), logical(1))
  if (any(null_names))
    stop(sprintf("%s records are arrays not objects", sum(null_names)))
  
  # Get array lengths
  lengths <- vapply(json, length, integer(1))
  
  # Compute indices
  indices <- rep(seq_along(json), lengths)
  
  # Expand x
  y <- x[indices, , drop = FALSE]

  # Add key names
  y[column.name] <- unlist(lapply(json, names))
  
  # Unwind JSON one level
  json <- unlist(json, recursive = FALSE)
  
  # Remove names
  names(json) <- NULL
  
  # Construct tbl_json
  tbl_json(y, json)
  
}