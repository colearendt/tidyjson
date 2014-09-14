#' Issue data for the dplyr repo from github API
#' 
#' @docType data
#' @name issues
#' @usage issues
#' @format JSON
#' @examples
#' 
#' library(dplyr)
#' 
#' # Show first 2k characters of JSON
#' issues %>% substr(1, 2000) %>% writeLines
NULL