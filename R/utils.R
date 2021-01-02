#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @export
magrittr::`%>%`

#' @details Exported from dplyr package.  Converts an object
#' to a pure tibble (revert to tbl_df class and drops
#' tbl_json class/attributes).
#' 
#' @name as_tibble
#' @rdname as_tibble
#' @aliases as_data_frame
#' @aliases tbl_df
#' @seealso as_tibble.tbl_json
#' @importFrom tibble as_tibble
#' @keywords internal
#' @export
tibble::as_tibble

#' @name filter
#' @rdname filter
#' @seealso http://r.789695.n4.nabble.com/R-CMD-check-warning-with-S3-method-td4692255.html
#' @details This needs to be re-exported, since `dplyr` implements a generic that 
#' is not a generic in the `stats` package, and `tidyjson` provides a method for that generic
#' @export
dplyr::filter

#' @export
#' @rdname as_tibble
dplyr::as_data_frame

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

#' 
#' List Check
#' 
#' Checks whether a list is being provided
#' 
#' @param x Input object
#' 
#' @return Boolean.  Indicates whether x is a list
#' 
#' @keywords internal
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
#' 
#' Checks the input object for the existence of names
#' 
#' @param x Input object
#' 
#' @return Boolean.  Indicates whether x has names
#' @keywords internal
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
