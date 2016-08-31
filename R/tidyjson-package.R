#' tidyjson.
#'
#' @name tidyjson
#' @docType package
#' @import assertthat
#' @import jsonlite
#' @importFrom purrr map map_lgl map_dbl map_int map_chr map2 %||% invoke_map
#' @importFrom purrr map_if is_formula compose
#' @import dplyr
#' @import tidyr
#' @importFrom lazyeval dots_capture uq f_list
NULL

# Quiet R CMD Check "no visible binding for global variable" when using dplyr
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("child.id", "document.id", "index", "key", "level",
                           "parent.id", "tail", "type",
                           "..key", "..json", "setNames"))
}
