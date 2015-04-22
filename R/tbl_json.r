#' Combines structured JSON (as a data.frame) with remaining JSON
#' 
#' @name tbl_json
NULL

#' tbl_json constructor
#' 
#' Note that json.list must have the same length as nrow(df), and if json.list
#' has any NULL elements, the corresponding rows will be removed from df. Also
#' note that "..JSON" is a reserved column name used internally for filtering
#' tbl_json objects, and so is not allowed in the data.frame names.
#' 
#' @param df data.frame
#' @param json.list list of json lists parsed with fromJSON
#' @param drop.null.json drop NULL json entries from data.frame and json
#' @param x an object to convert into a tbl_json object
#' @param json.column the name of the JSON column of data in x, if x is a data.frame
#' @param ... other arguments
#' @rdname tbl_json
#' @export
tbl_json <- function(df, json.list, drop.null.json = FALSE) {

  assert_that(is.data.frame(df))
  assert_that(is.list(json.list) || is.vector(json.list))
  assert_that(nrow(df) == length(json.list))
  assert_that(!("..JSON" %in% names(df)))
  
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

#' @export
#' @rdname tbl_json
as.tbl_json.character <- function(x, ...) {

  # Parse the json
  json <- lapply(x, fromJSON, simplifyVector = FALSE)

  # Setup document ids
  ids <- data.frame(document.id = seq_along(json))

  # Construct tbl_json
  tbl_json(ids, json)
}

#' @export
#' @rdname tbl_json
as.tbl_json.data.frame <- function(x, json.column, ...) {
  
  assert_that(is.character(json.column))
  assert_that(json.column %in% names(x))

  # Parse the json
  json <- lapply(x[[json.column]], fromJSON, simplifyVector = FALSE)

  # Remove json column
  x <- x[, setdiff(names(x), json.column), drop = FALSE]

  # Construct tbl_json
  tbl_json(x, json)
  
}

#' @export
#' @rdname tbl_json
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

#' Wrapper for extending dplyr verbs to tbl_json objects
#' @param dplyr.verb a dplyr::verb such as filter, arrange
wrap_dplyr_verb <- function(dplyr.verb) {
  
  function(.data, ...) {
  
    # Check if reserved ..JSON name already in data.frame
    if ("..JSON" %in% names(.data))
      stop("'..JSON' in the column names of tbl_json object being filtered")
  
    # Assign JSON to the data.frame so it is treated as any other column
    .data$..JSON <- attr(.data, "JSON")
  
    # Apply the transformation
    y <- dplyr.verb(tbl_df(.data), ...)
  
    # Reconstruct tbl_json without ..JSON column
    tbl_json(select_(y, "-..JSON"), y$..JSON)
    
  }
}

#' @export
filter_.tbl_json <- wrap_dplyr_verb(dplyr::filter_)

#' @export
arrange_.tbl_json <- wrap_dplyr_verb(dplyr::arrange_)

#' @export
mutate_.tbl_json <- wrap_dplyr_verb(dplyr::mutate_)
