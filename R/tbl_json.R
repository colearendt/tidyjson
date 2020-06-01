#' Combines structured JSON (as a data.frame) with remaining JSON
#'
#' @name tbl_json
NULL

#' \code{tbl_json} constructor
#'
#' Constructs a \code{tbl_json} object, for further downstream manipulation
#' by other tidyjson functions. Methods exist to convert JSON stored in
#' character strings without any other associated data, as a separate
#' character string and associated data frame, or as a single data frame
#' with a specified character string JSON column.
#'
#' Most tidyjson functions accept a \code{tbl_json} object as the first
#' argument, and return a \code{tbl_json} object unless otherwise specified.
#' tidyjson functions will attempt to convert an object that isn't a
#' \code{tbl_json} object first, and so explicit construction of \code{tidyjson}
#' objects is rarely needed.
#'
#' \code{tbl_json} objects consist of a data frame along with it's associated
#' JSON, where each row of the data frame corresponds to a single JSON
#' document. The JSON is stored in a \code{"JSON"} attribute.
#'
#' Note that \code{json.list} must have the same length as \code{nrow(df)}, and
#' if \code{json.list} has any \code{NULL} elements, the corresponding rows will
#' be removed from \code{df}. Also note that \code{"..JSON"} is a reserved
#' column name used internally for filtering tbl_json objects, and so is not
#' allowed in the names of \code{df}.
#'
#' @seealso \code{read_json} for reading json from files
#' @param df data.frame
#' @param json.list list of json lists parsed with
#'                  \code{\link[jsonlite]{fromJSON}}
#' @param drop.null.json drop \code{NULL} json entries from \code{df} and
#'                       \code{json.list}
#' @param .x an object to convert into a \code{tbl_json} object
#' @param json.column the name of the json column of data in \code{.x}, if
#'                    \code{.x} is a data frame
#' @param ... other arguments
#' @param .column_order Experimental argument to preserve column order for the hidden column
#' @return a \code{\link{tbl_json}} object
#' @rdname tbl_json
#' @export
#' @examples
#'
#' # Construct a tbl_json object using a charater string of JSON
#' json <- '{"animal": "cat", "count": 2}'
#' json %>% as.tbl_json
#'
#' # access the "JSON" argument
#' json %>% as.tbl_json %>% attr("JSON")
#'
#' # Construct a tbl_json object using multiple documents
#' json <- c('{"animal": "cat", "count": 2}', '{"animal": "parrot", "count": 1}')
#' json %>% as.tbl_json
#'
#' # Construct a tbl_json object from a data.frame with a JSON colum
#' library(tibble)
#' farms <- tribble(
#'   ~farm, ~animals,
#'   1L,    '[{"animal": "pig", "count": 50}, {"animal": "cow", "count": 10}]',
#'   2L,    '[{"animal": "chicken", "count": 20}]'
#' )
#' farms %>% as.tbl_json(json.column = "animals")
#' # tidy the farms
#' farms %>% as.tbl_json(json.column = "animals") %>%
#'   gather_array %>% spread_all
tbl_json <- function(df, json.list, drop.null.json = FALSE, ..., .column_order = NULL) {

  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(is.list(json.list) || is.vector(json.list))
  assertthat::assert_that(nrow(df) == length(json.list))

  # Remove any row.names
  row.names(df) <- NULL

  # Remove any rows of df where json.list is NULL
  if (drop.null.json) {
    nulls <- purrr::map_lgl(json.list, is.null)
    df <- df[!nulls, , drop = FALSE]
    json.list <- json.list[!nulls]
  }
  
  df <- reorder_json_column(df, json.list, .preserve_column_order = .column_order)
  
  structure(df, class = c("tbl_json", "tbl_df", "tbl", "data.frame"))
}

reorder_json_column <- function(.data, .json, .preserve_column_order = NULL) {
  # try to make ..JSON the last column consistently
  # this is because otherwise making column order persistent is hard...
  if (is.null(.preserve_column_order)) {
    .data[["..JSON"]] <- NULL
    .data[["..JSON"]] <- .json
  } else {
    .data[["..JSON"]] <- .json
  }
  return(.data)
}

#' @export
#' @rdname tbl_json
as.tbl_json <- function(.x, ...) UseMethod("as.tbl_json")

#' @export
#' @rdname tbl_json
as_tbl_json <- function(.x, ...) UseMethod("as.tbl_json")


#' @export
#' @rdname tbl_json
as.tbl_json.tbl_json <- function(.x, ...) reorder_json_column(.x, json_get(.x))

#' @export
#' @rdname tbl_json
as.tbl_json.character <- function(.x, ...) {

  # Parse the json
  json <- purrr::map(.x, jsonlite::fromJSON, simplifyVector = FALSE)

  # Setup document ids
  ids <- data.frame(document.id = seq_along(json))

  # Construct tbl_json
  tbl_json(ids, json)
}

#' @export
#' @rdname tbl_json
as.tbl_json.list <- function(.x, ...) {
  # Setup document ids
  ids <- data.frame(document.id = seq_along(.x))
  
  # Construct tbl_json
  tbl_json(ids, .x)
}

#' @export
#' @rdname tbl_json
as.tbl_json.data.frame <- function(.x, json.column, ...) {

  assertthat::assert_that(is.character(json.column))
  assertthat::assert_that(json.column %in% names(.x))

  jcol <- .x[[json.column]]
  
  if (is.list(jcol)) {
    # treat the object as already parsed
    json <- jcol
  } else {
    # Parse the json
    json <- purrr::map(jcol, jsonlite::fromJSON, simplifyVector = FALSE)
  }

  # Remove json column
  .x <- .x[, setdiff(names(.x), json.column), drop = FALSE]

  # Construct tbl_json
  tbl_json(.x, json)
}

#' @export
#' @rdname tbl_json
is.tbl_json <- function(.x) inherits(.x, "tbl_json")

#' Extract subsets of a tbl_json object (not replace)
#'
#' Extends `[.data.frame` to work with tbl_json objects, so that row filtering
#' of the underlying data.frame also filters the associated JSON.
#'
#' @param .x a tbl_json object
#' @param i row elements to extract
#' @param j column elements to extract
#' @param drop whether or not to simplify results
#' @return a \code{\link{tbl_json}} object
#' @export
`[.tbl_json` <- function(.x, i, j,
  drop = FALSE) {

  n_real_args <- nargs() - !missing(drop)
  
  # Extract JSON to subset later
  json <- json_get(.x)
  
  # "column" selection behavior
  if (n_real_args <= 2L) {
    if (!missing(drop)) 
      warning("drop ignored")
    if (missing(i)) {
      return(.x)
    }
  
    # Subset x
    .x <- NextMethod('[')
  } else {
    
    # Subset x
    .x <- NextMethod('[')
    # If i is not missing, subset json as well
    if (!missing(i)) {
      json <- json[i]
    }

  }

  # preserving column order is important here
  tbl_json(.x, json, .column_order = TRUE)
}

# debate using full tbl_json() for (1) remove row.names, (2) remove bad rows
#' @export
`$<-.tbl_json` <- function(x, name, value) {
  y <- NextMethod("$<-", x)
  reorder_json_column(y, json_get(y))
}

#' Get JSON from a tbl_json
#' 
#' Extract the raw JSON from a tbl_json object. This is equivalent to reading
#' the "..JSON" hidden column. But is a helper in case of future behavior changes.
#' This replaces previous behavior, where the raw JSON was stored in an attribute.
#' 
#' @param .data A tbl_json object
#' 
#' @return A nested list representing the JSON data
#' 
#' @export
json_get <- function(.data) {
  .data[["..JSON"]]
}

#' Make the JSON data a persistent column
#' 
#' Extract the raw JSON from a tbl_json object. Store it in a column. WARNING:
#' column name collisions will be overwritten
#' 
#' @param .data A tbl_json object
#' @param column_name Optional. The name of the output column (either as a
#'   string or unquoted name). Default "json"
#' 
#' @return A tbl_json object with an added column containing the JSON data
#' 
#' @examples
#' 
#' tj <- as_tbl_json('{"a": "b"}')
#' json_get_column(tj, my_json)
#' 
#' @export
json_get_column <- function(.data, column_name = "json") {
  # TODO: protect against name collisions somehow?
  qnm <- rlang::enquo(column_name)
  colnm <- rlang::as_name(qnm)
  if ("json" %in% names(.data)) {
    warning(paste0("Column `", colnm, "` already exists. It will be overwritten by `json_get_column()`"))
  }
  .data[[colnm]] <- json_get(.data)
  return(.data)
}

#' Wrapper for extending dplyr verbs to tbl_json objects
#' @param dplyr.verb a dplyr::verb such as filter, arrange
#' @param generic character. The name of the generic
#' @keywords internal
wrap_dplyr_verb <- function(dplyr.verb, generic) {

  function(.data, ...) {
    
    json <- json_get(.data)
    
    # Apply the transformation
    if (generic %in% c("select")) {
      # some generics need a tibble
      .data <- tibble::as_tibble(.data)
      
      # remove ..JSON operators
      vars <- rlang::enquos(...)
      vars_lgl <- purrr::map_lgl(
        vars,
        ~ any(as.character(rlang::get_expr(.x)) %in% "..JSON")
        )
      vars[vars_lgl] <- NULL
      
      y <- dplyr::select(.data, !!!vars)
    } else if (generic %in% c("transmute")) {
      .data <- tibble::as_tibble(.data)
      y <- NextMethod(generic, .data)
    } else {
      y <- NextMethod(generic, .data)
    }

    # Reconstruct tbl_json without ..JSON column
    if ("..JSON" %in% names(y)) {
      return(tbl_json(tibble::as_tibble(y), json_get(y)))
    } else {
      # some operations drop the ..JSON column (i.e. transmute)
      return(tbl_json(y, json))
    }
  }
}

#group_by.tbl_json <- function() {
#  # perhaps do not allow grouping by ..JSON column
#}

# remove the ..JSON column from grouped vars
# this fails because set_name cannot handle a missing column
#group_vars.tbl_json <- function(x) {
#  upstream <- NextMethod("group_vars", x)
#  setdiff(upstream, "..JSON")
#}

#' @export
select.tbl_json <- wrap_dplyr_verb(dplyr::select, "select")

#' @export
filter_.tbl_json <- wrap_dplyr_verb(dplyr::filter_, "filter_")

#' @export
#' @method filter tbl_json
filter.tbl_json <- wrap_dplyr_verb(dplyr::filter, "filter")

#' @export
arrange_.tbl_json <- wrap_dplyr_verb(dplyr::arrange_, "arrange_")

#' @export
#' @method arrange tbl_json
arrange.tbl_json <- wrap_dplyr_verb(dplyr::arrange, "arrange")

#' @export
mutate_.tbl_json <- wrap_dplyr_verb(dplyr::mutate_, "mutate_")

#' @export
#' @method mutate tbl_json
mutate.tbl_json <- wrap_dplyr_verb(dplyr::mutate, "mutate")

#' @export
#' @method transmute tbl_json
transmute.tbl_json <- wrap_dplyr_verb(dplyr::transmute, "transmute")

#' @export
slice_.tbl_json <- wrap_dplyr_verb(dplyr::slice_, "slice_")

#' @export
#' @method slice tbl_json
slice.tbl_json <- wrap_dplyr_verb(dplyr::slice, "slice")

#' @export
#' @method inner_join tbl_json
inner_join.tbl_json <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), 
                                ...) {
  x <- as_tibble(x)
  y <- as_tibble(y)
  NextMethod("inner_join", x)
}

#' @export
#' @method full_join tbl_json
full_join.tbl_json <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), 
                                ..., keep = FALSE) {
  x <- as_tibble(x)
  y <- as_tibble(y)
  NextMethod("full_join", x)
}

#' @export
#' @method left_join tbl_json
left_join.tbl_json <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), 
                                ..., keep = FALSE) {
  x <- as_tibble(x)
  y <- as_tibble(y)
  NextMethod("left_join", x)
}

#' @export
#' @method right_join tbl_json
right_join.tbl_json <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), 
                                ..., keep = FALSE) {
  x <- as_tibble(x)
  y <- as_tibble(y)
  NextMethod("right_join", x)
}

#' @export
#' @method dplyr_reconstruct tbl_json
dplyr_reconstruct.tbl_json <- function(data, template) {
  # TODO: improve this handling in ?dplyr_reconstruct
  #   - For now, just drop the tbl_json class
  if ("..JSON" %in% names(data)) {
    as_tbl_json(data, json.column = "..JSON")
  } else {
    tibble::as_tibble(data)
  }
}

#' @name bind_rows
#' @rdname bind_rows
#' @keywords internal
#' @importFrom dplyr bind_rows
#' @export
dplyr::bind_rows

#' Convert the JSON in an tbl_json object back to a JSON string
#'
#' @param x a tbl_json object
#' @param ... not used (\code{\link[purrr]{map_chr}} used instead)
#' @return a character vector of formatted JSON
#' @export
as.character.tbl_json <- function(x, ...) {

  json <- json_get(x)
  if (is.null(json)) {
    warning("the ..JSON column has been removed from this tbl_json object")
    json <- list()
  }
  json %>% purrr::map_chr(jsonlite::toJSON,
                   null = "null",
                   auto_unbox = TRUE)

}

#' Convert a tbl_json back to a tbl_df
#' 
#' Drops the JSON attribute and the tbl_json class, so that
#' we are back to a pure tbl_df.  Useful for some internals.  Also useful
#' when you are done processing the JSON portion of your data and are
#' ready to move on to other tools.
#' 
#' Note that as.tbl calls tbl_df under the covers, which in turn
#' calls as_tibble.  As a result, this should take care of all cases.
#' 
#' @param x a tbl_json object
#' @param ... additional parameters
#' @return a tbl_df object (with no tbl_json component)
#' 
#' @export
as_tibble.tbl_json <- function(x, ...) {
  x$..JSON <- NULL
  as_tibble(
    structure(x, class = class(tibble::tibble()))
  )
}

#' @rdname as_tibble.tbl_json
#' 
#' @export
as_data_frame.tbl_json <- function(x, ...) {
  as_tibble.tbl_json(x,...)
}



#' Print a tbl_json object
#'
#' @param x a \code{\link{tbl_json}} object
#' @param ... other arguments into \code{\link[tibble]{print.tbl_df}}
#' @param json.n number of json records to add (...) otherwise
#' @param json.width number of json characters to print
#' @export
print.tbl_json <- function(x, ..., json.n = 20, json.width = 15) {

  # Extract json
  json <- x %>% as.character
  json <- json[seq_len(min(json.n, nrow(x)))]

  # Truncate json
  lengths <- dplyr::coalesce(json %>% nchar,0L)
  json <- json %>% strtrim(json.width)
  json[lengths > json.width] <- paste0(json[lengths > json.width], "...")

  # Add the json
  .y <- dplyr::as_tibble(x)
  json_name <- '..JSON'
  .y[json_name] <- rep("...", nrow(x))
  .y[[json_name]][seq_len(length(json))] <- json

  # Re-arrange columns
  ncol_y <- ncol(.y)
  .y <- .y[, c("..JSON", names(.y)[names(.y) != "..JSON"])]

  # Print trunc_mat version
  out <- capture.output(print(.y))
  out <- gsub("^\\# A tibble:.*",
              "\\# A tbl_json: %s x %s tibble with a \"JSON\" attribute" %>%
                sprintf(nrow(x) %>% format(big.mark = ','),
                        ncol(x) %>% format(big.mark = ',')),
              out)
  writeLines(out)

  invisible(x)
}
