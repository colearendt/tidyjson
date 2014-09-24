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
#' @param drop.null.json drop NULL json entries from data.frame and json
#' @param ... other arguments
#' @rdname tbl_json
#' @export
tbl_json <- function(df, json.list, drop.null.json = FALSE) {

  assert_that(is.data.frame(df))
  assert_that(is.list(json.list) || is.vector(json.list))
  assert_that(nrow(df) == length(json.list))
  
  # Remove any row.names
  row.names(df) <- NULL
  
  # Remove any rows of df where json.list is NULL
  if (drop.null.json) {
    nulls <- vapply(json.list, is.null, logical(1))
    df <- df[!nulls, , drop = FALSE]
    json.list <- json.list[!nulls]
  }
  
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

  # Check for valid
  is_valid <- vapply(x, validate, logical(1))
  
  if (any(!is_valid))
    stop(sprintf("%s records have invalid json data", sum(!is_valid)))
  
  # Parse the json
  json <- lapply(x, fromJSON, simplifyVector = FALSE)

  # Setup document ids
  ids <- data.frame(document.id = seq_along(json))

  # Construct tbl_json
  tbl_json(ids, json)
}

#' @rdname tbl_json
#' @export
is.tbl_json <- function(x) inherits(x, "tbl_json")

#' Extract subsets of a tbl_json object (not replace)
#' 
#' Extends `[.data.frame` to work with tbl_json objects, so that row filtering
#' of the underlying data.frame also filters the associated JSON.
#'
#' @param x a tbl_json object
#' @param i row elements to extract
#' @param j column elements to extract
#' @param drop whether or not to simplify results
#' @export
`[.tbl_json` <- function(x, i, j, 
  drop = if (missing(i)) TRUE else length(cols) == 1) {
  
  # Same functionality as in `[.data.frame`
  y <- NextMethod("[")
  cols <- names(y)
  
  # Extract JSON to subset later
  json <- attr(x, "JSON")
  
  # Convert x back into a data.frame
  x <- as.data.frame(x)
  
  # Subset x
  x <- `[.data.frame`(x, i, j, drop)
  
  # If i is not missing, subset json as well
  if (!missing(i)) {
    json <- json[i]
  }
  
  tbl_json(x, json)
}

#' @export
filter.tbl_json <- function(.data, ...) {
  
  if ("..JSON" %in% names(.data))
    stop("'..JSON' in the column names of tbl_json object being filtered")
  
  .data$..JSON <- attr(.data, "JSON")
  
  y <- dplyr::filter(tbl_df(.data), ...)
  
  tbl_json(select(y, -..JSON), y$..JSON)
  
}
