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
#' @usage tbl_df(data)
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


#' Handles dots or a list
list_or_dots <- function (...) 
{
  dots <- list(...)
  data_lists <- vapply(dots, is_data_list, logical(1))
  dots[data_lists] <- lapply(dots[data_lists], list)
  unlist(dots, recursive = FALSE)
}

#' 
#' Checks whether a list is being provided
#' 
is_data_list <- function (x) 
{
  if (is.data.frame(x) || is.null(x)) 
    return(TRUE)
  if (!is.list(x)) 
    return(FALSE)
  if (!is.null(names(x)) && length(x) == 0) 
    return(TRUE)
  if (any(!has_names(x))) 
    return(FALSE)
  is_1d <- vapply(x, is_1d, logical(1))
  if (any(!is_1d)) 
    return(FALSE)
  n <- vapply(x, length, integer(1))
  if (any(n != n[1])) 
    return(FALSE)
  TRUE
}

#' Check for Names
has_names <- function (x) 
{
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  }
  else {
    !is.na(nms) & nms != ""
  }
}