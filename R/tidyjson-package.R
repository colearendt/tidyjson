#' tidyjson.
#'
#' @name tidyjson
#' @docType package
#' @import assertthat
#' @import jsonlite
#' @importFrom purrr map map_lgl map_dbl map_int map_chr map2 %||% invoke_map
#' @import dplyr
#' @import tidyr
NULL

# Quiet R CMD Check "no visible binding for global variable" when using dplyr
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("child.id", "document.id", "index", "key", "level",
                           "parent.id", "tail", "type",
                           "..key", "..json", "setNames"))
}
