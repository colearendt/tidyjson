#' Combines structured JSON (as a data.frame) with remaining JSON
#' 
#' @name tbl_json
NULL

#' tbl_json constructor
#' 
#' Note that json.list must have the same length as nrow(df), and if json.list
#' has any NULL elements, the corresponding rows will be removed from df.
#' 
#' @param df data.frame
#' @param json.list list of json lists parsed with fromJSON
#' @rdname tbl_json
#' @export
tbl_json <- function(df, json.list) {

  assert_that(is.data.frame(df))
  assert_that(is.list(json.list))
  assert_that(nrow(df) == length(json.list))
  
  # Remove any row.names
  row.names(df) <- NULL
  
  # Remove any rows of df where json.list is NULL
  # this results in enter_object dropping rows where data does not exist
  nulls <- vapply(json.list, is.null, logical(1))
  df <- df[!nulls, , drop = FALSE]
  json.list <- json.list[!nulls]
  
  structure(df, JSON = json.list, class = c("tbl_json", "tbl", "data.frame"))
}

#' @export
#' @rdname tbl_json
as.tbl_json <- function(x, ...) UseMethod("as.tbl_json")

#' @export
#' @rdname tbl_json
as.tbl_json.tbl_json <- function(x, ...) x

#' Turns a character vector into a tbl_json object
#' @param x character vector of json
#' @rdname tbl_json
#' @export
as.tbl_json.character <- function(x, ...) {

  # Parse the json
  json <- lapply(x, fromJSON)

  # Setup document ids
  ids <- data.frame(document.id = seq_along(json))

  # Construct tbl_json
  tbl_json(ids, json)
}

#' @rdname tbl_json
#' @export
is.tbl_json <- function(x) inherits(x, "tbl_json")
