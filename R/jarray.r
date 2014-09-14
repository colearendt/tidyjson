#' Structures an array
#' 
jarray <- function(x, column.name = "array.index") {

  assert_that(is.jdf(x))
  
  # Get JSON
  json <- attr(x, "JSON")
  
  # Ensure not values
  not_list <- vapply(json, is.list, logical(1))
  if (any(!not_list))
    stop(sprintf("%s records are values not arrays", sum(!not_list)))
  
  
  # Ensure not arrays
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
  
  # Set row.names to null
  row.names(y) <- NULL
  
  # Reset JSON
  attr(y, "JSON") <- unlist(json, recursive = FALSE)
  
  # Return
  y
  
}