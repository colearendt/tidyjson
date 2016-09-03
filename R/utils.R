#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Bind two tbl_json objects together and preserve JSON attribute
rbind_tbl_json <- function(x, y) {

  tbl_json(
    bind_rows(x, y),
    c(attr(x, "JSON"), attr(y, "JSON"))
  )

}
