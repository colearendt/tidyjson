#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Convert object to tbl_df
#' 
#' Exported from dplyr package.  Converts an object
#' to a tbl_df.
#' 
#' @name tbl_df
#' @rdname tbl_df
#' @keywords internal
#' @aliases as_data_frame
#' @seealso as_data_frame.tbl_json
#' @export
#' @usage tbl_df(mytbljson)
NULL

#' Bind two tbl_json objects together and preserve JSON attribute
#'
#' @param x a tbl_json object
#' @param y a tbl_json_object
#' @return x and y row-binded together with appropriate JSON attribute
rbind_tbl_json <- function(x, y) {

  tbl_json(
    dplyr::bind_rows(x %>% unclass, y %>% unclass),
    c(attr(x, "JSON"), attr(y, "JSON"))
  )

}
