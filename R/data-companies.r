#' Startup company information for 1,000 companies
#'
#' From: http://jsonstudio.com/resources/
#' 
#' @docType data
#' @name companies
#' @usage companies
#' @format JSON
#' @examples
#' 
#' library(dplyr)
#' 
#' # Print the first record
#' companies[[1]] %>% writeLines
#' 
#' # Get the key employees data
#' key_employees <- companies %>%
#'   spread_values(
#'     name = jstring("name")
#'   ) %>% 
#'   mutate(
#'     company.sort_order = rank(name)
#'   ) %>%
#'   enter_object("relationships") %>%
#'   gather_array("relationship.index") %>%
#'   spread_values(
#'     is.past = jlogical("is_past"),
#'     name = jstring("person", "permalink"),
#'     title = jstring("title")
#'   )
#' 
#' # Show the top 10 titles
#' key_employees %>%
#'   filter(!is.past) %>%
#'   group_by(title) %>%
#'   tally() %>%
#'   arrange(desc(n)) %>%
#'   top_n(10)
#' 
#' # Extract the top level keys and their types
#' comp_types <- companies %>% as.tbl_json %>% gather_keys %>% json_types
#' 
#' # Aggregate across keys
#' comp_type_counts <- comp_types %>% group_by(key, type) %>% tally()
#' 
#' # Visualize the top-level keys and their types
#' library(ggplot2)
#' ggplot(comp_type_counts, aes(key, n, fill = type)) + 
#'   geom_bar(position = "stack", stat = "identity") +
#'   coord_flip()
NULL
