#' Combines structured JSON (as a data.frame) with remaining JSON
#' 
#' @name jdf
#' @examples
#' json <- as.jdf(commits)
#' attr(json, "JSON")
NULL

#' jdf constructor
#' 
#' Note that json.list must have the same length as nrow(df), and if json.list
#' has any NULL elements, the corresponding rows will be removed from df.
#' 
#' @param df data.frame
#' @param json.list list of json lists parsed with fromJSON
#' @rdname jdf
#' @export
jdf <- function(df, json.list) {

  assert_that(is.data.frame(df))
  assert_that(is.list(json.list))
  assert_that(nrow(df) == length(json.list))
  
  # Remove any row.names
  row.names(df) <- NULL
  
  # Remove any rows of df where json.list is NULL
  nulls <- vapply(json.list, is.null, logical(1))
  df <- df[!nulls, , drop = FALSE]
  json.list <- json.list[!nulls]
  
  structure(df, JSON = json.list, class = c("jdf", "data.frame"))
}


#' @export
#' @rdname jdf
as.jdf <- function(x, ...) UseMethod("as.jdf")

#' @export
#' @rdname jdf
as.jdf.jdf <- function(x, ...) x

#' @rdname jdf
#' @export
is.jdf <- function(x) inherits(x, "jdf")

#' Turns a character vector into a jdf object
#' @param x character vector of json
#' @rdname jdf
#' @export
as.jdf.character <- function(x, ...) {

  # Parse the json
  json <- lapply(x, fromJSON)

  # Setup document ids
  ids <- data.frame(document.id = seq_along(json))

  # Construct jdf
  jdf(ids, json)

}