
#' @export
#' @rdname jdf
as.jdf <- function(x, ...) UseMethod("as.jdf")

#' @export
#' @rdname jdf
as.jdf.jdf <- function(x, ...) x

#' @rdname jdf
#' #' @export
is.jdf <- function(x) inherits(x, "jdf")

#' Turns a character vector into a jdf object
#' @param json character vector of json
#' @rdname jdf
#' @export
as.jdf.character <- function(json, ...) {

  # Parse the json
  json <- lapply(json, fromJSON)

  # Setup document ids
  ids <- data.frame(document.id = seq_along(json))

  # Return a structure with JSON as an attribute
  structure(ids, JSON = json, class = c("jdf", "data.frame"))

}

# 1) Need to add tests for above
# 2) Need to add to.data.frame
# 3) Need to add print
# 4) Need to rename this file
# 5) Need to copy over / cleanup the parse_json
