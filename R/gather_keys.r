#' Gathers every key from the top level of the json and stacks them 
#' 
#' @param x a tbl_json whose JSON attribute should always be an object
#' @param column.name the name to give to the column of key names created
#' @return a tbl_json with a new column (column.name) that captures the keys
#'   and JSON attribute of the associated value data
#' @export
gather_keys <- function(x, column.name = "key") {

  if (!is.tbl_json(x)) x <- as.tbl_json(x)
  
  # Get JSON
  json <- attr(x, "JSON")
  
  # Handle the case where json is just an empty list
  if (identical(json, list())) {
    # Drop any rows
    y <- x[integer(0), , drop = FALSE]
    # Setup 
    y[column.name] <- character(0) 
    return(tbl_json(y, list()))
  }

  # Determine types
  types <- determine_types(json)
  
  # Check if not arrays
  not_objects <- types != "object"
  if (any(not_objects))
    stop(sprintf("%s records are values not objects", sum(not_objects)))
  
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
