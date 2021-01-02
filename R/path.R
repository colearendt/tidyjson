#' Create a JSON path with a minimum of typing
#'
#' @param ... a sequence of quoted or unquoted character strings specifying
#'            JSON object names
#' @return a \code{path} object
#' @keywords internal
path <- function(...) {

  dot_quos <- rlang::quos(...)
  dots <- purrr::map_chr(dot_quos, ~ rlang::as_name(.x))

  all_names <- purrr::every(dots, is.name)
  all_char <- purrr::every(dots, is.character)

  if (!all_names && !all_char) {
    stop("Path components must be single names or character strings",
         call. = FALSE)
  }

  out <- structure(
    purrr::map_chr(dots, as.character),
    class = "path"
  )
  names(out) <- NULL
  out
}
