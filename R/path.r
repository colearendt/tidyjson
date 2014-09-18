#' Prepare a path from ...
#' @param ... character strings to construct into a path
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
#' @param l a list
#' @param path a path of keys to follow
list_path <- function(l, path) {
  
  # Unwind this step
  l <- lapply(l, "[[", path[1])
  
  # Keep going if more remains in the path 
  if (length(path) > 1)
    l <- list_path(l, path[-1])
  
  l
}

#' Replace nulls with something else
#' @param l a list
#' @param replace what to replace NULLs in the list with
replace_nulls <- function(l, replace) {
  
  # Find the nulls
  nulls <- vapply(l, is.null, logical(1))
  
  if (any(nulls))
    l[nulls] <- replace

  l
  
}