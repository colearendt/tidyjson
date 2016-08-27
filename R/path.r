#' Prepare a path from ...
#' @param ... character strings to construct into a path
prep_path <- function(...) {

  # Conver to a list
  path <- list(...)

  # Check type
  path_is_char <- map_lgl(path, inherits, "character")
  path_len_1 <- map_int(path, length) == 1
  if (!all(path_is_char & path_len_1))
    stop("malformed path")

  # Unlist
  path <- unlist(path)

  path
}
