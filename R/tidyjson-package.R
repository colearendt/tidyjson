#' tidyjson
#' 
#' The tidyjson package provides tools to turn complex JSON data into 
#' tidy tibbles and data frames.
#'
#' @name tidyjson
#' @docType package
#' @import assertthat
#' @import jsonlite
#' @importFrom purrr map map_lgl map_dbl map_int map_chr map2 %||%
#' @importFrom purrr map_if at_depth flatten_chr flatten_dbl flatten_int
#' @importFrom purrr compose partial map2_chr rep_along every lift_vl
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr extract2 is_greater_than
#' @importFrom tibble trunc_mat
#' @importFrom utils capture.output
NULL

# Quiet R CMD Check "no visible binding for global variable" when using dplyr
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("child.id", "document.id", "index", "name", "level",
                           "parent.id", "tail", "type",
                           "..name", "..json", "setNames",
                           ".", "schemas", "complexity",
                           "..id", "..name1", "..name2", "..type",
                           "..value", "..suffix", "all.null", "..JSON", ":="))
}
