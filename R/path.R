#' Create a JSON path with a minimum of typing
#'
#' @param ... a sequence of quoted or unquoted character strings specifying
#'            JSON object names
#' @return a \code{path} object
path <- function(...) {

  dots <- dots(...)

  all_names <- every(dots, is.name)
  all_char <- every(dots, is.character)

  if (!all_names && !all_char) {
    stop("Path components must be single names or character strings",
         call. = FALSE)
  }

  structure(
    purrr::map_chr(dots, as.character),
    class = "path"
  )
}

dots <- function(...) {
  eval(substitute(alist(...)))
}
