#' Summarize a collection of JSON documents into a single schema document
#'
#' @param x a json string or a tbl_json object
json_schema <- function(x) {

  # Compute the object structure
  structure <- x %>% json_structure %>% tbl_df

  # Change all array indices to 1L in sequence to unify arrays
  # Also create an id that is the call to construct the seq
  structure <- structure %>%
    mutate(seq = seq %>% at_depth(2, . %>% map_if(is.numeric, pmin, 1))) %>%
    select(seq, key, type) %>%
    unique %>%
    mutate(seq.id = seq %>% map_chr(deparse)) %>%
    mutate(order = 1:n())

  # Which seq.ids are entirely null?
  all_nulls <- structure %>%
    group_by(seq.id) %>%
    summarize(all.null = all(type == "null")) %>%
    filter(all.null)

  # Add nulls in with other non-nulls
  structure <- structure %>%
    filter(seq.id %in% all_nulls$seq.id) %>%
    bind_rows(structure %>% filter(type != "null")) %>%
    arrange(order)

  # Iteratively create JSON
  structure_to_json(structure)

}

structure_to_json <- function(generations) {

  parent <- generations[1, ]

  if (!(parent$type %in% c("object", "array")) && nrow(generations) > 1)
    stop("must not have multiple generations for a scalar parent")

  if (nrow(generations) == 1) {

    if (parent$type == "object")
      return("{}")
    if (parent$type == "array")
      return("[]")
    return(paste0('"', as.character(parent$type), '"'))

  }

  descendents <- generations[-1, ]

  first_gen <- (descendents$seq %>% map_int(length)) == 1
  first_gen_group <- descendents$seq %>% map(`[[`, 1) %>% unlist(recursive = FALSE)

  if (parent$type == "object") {
    lhs <- '{'
    rhs <- '}'
    descendents <- descendents %>% mutate(group = flatten_chr(first_gen_group))
  }
  if (parent$type == "array") {
    lhs <- '['
    rhs <- ']'
    descendents <- descendents %>% mutate(group = flatten_dbl(first_gen_group))
  }

  descendents <- descendents %>%
    mutate(seq = seq %>% at_depth(1, `[`, -1))

  children <- descendents %>%
    split(descendents$group) %>%
    map_chr(structure_to_json)

  if (parent$type == "object") {
    keys <- paste0('"', names(children), '"')
    children <- paste(keys, children, sep = ": ")
  }

  children <- paste(children, collapse = ", ")

  paste0(lhs, children, rhs)

}
