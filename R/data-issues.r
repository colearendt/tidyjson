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
#' 
#' # Extract metadata for every issue
#' issues %>%   # single json document of github issues from dplyr 
#'   as.jdf %>%  # turn into a 'jdf'
#'   jarray %>%  # stack as an array
#'   jvalue(
#'     id          = jnumber("id"),
#'     number      = jnumber("number"),
#'     title       = jstring("title"),
#'     user.login  = jstring("user", "login"),
#'     sate        = jstring("state"),
#'     locked      = jlogical("locked"),
#'     comments    = jnumber("comments")
#'   )
#' 
#' # Extract label content for issues with labels
#' issues %>%   # single json document of github issues from dplyr 
#'   as.jdf %>%  # turn into a 'jdf'
#'   jarray %>%  # stack as an array
#'   jvalue(id = jnumber("id")) %>% # capture issue id for relational purposes
#'   jfilter("labels") %>%          # filter just those with labels
#'   jarray("label.index") %>%      # stack labels
#'   jvalue(
#'     url   = jstring("url"),
#'     name  = jstring("name"),
#'     color = jstring("color")
#'   )
NULL


