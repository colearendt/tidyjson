#' tidyjson.
#'
#' @name tidyjson
#' @docType package
#' @import assertthat
#' @import jsonlite
#' @importFrom purrr map map_lgl map_dbl map_int map_chr map2 %||% invoke_map
#' @importFrom purrr map_if at_depth flatten_chr flatten_dbl compose partial
#' @importFrom purrr map2_chr list_along
#' @import dplyr
#' @import tidyr
#' @importFrom igraph graph_from_data_frame plot.igraph V layout_with_kk
#' @importFrom RColorBrewer brewer.pal
#' @importFrom graphics par
NULL

# Quiet R CMD Check "no visible binding for global variable" when using dplyr
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("child.id", "document.id", "index", "key", "level",
                           "parent.id", "tail", "type",
                           "..key", "..json", "setNames"))
}
