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
#'
#' @param x a tbl_json object
#' @param y a tbl_json_object
#' @return x and y row-binded together with appropriate JSON attribute
rbind_tbl_json <- function(x, y) {

  tbl_json(
    bind_rows(x %>% unclass, y %>% unclass),
    c(attr(x, "JSON"), attr(y, "JSON"))
  )

}
