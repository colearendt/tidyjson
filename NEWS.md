# purrr 0.2.1.9000

## New functions

* `json_complexity()` computes the "complexity" (recursively unlisted length) of JSON data (#5)

* `json_structure()` recursively structures arbitrary JSON data into a single data frame (#2)

* `json_schema()` creates a schema for a JSON document or collection (#12)

* `is_json` functions for testing JSON types, such as `is_json_string()`, `is_json_null()` or `is_json_object()` (#39)

* `plot_json_graph()` plots an [igraph](https://github.com/igraph/igraph) visualization of a JSON document (#47)

* `spread_all()` spreads all scalar values of a JSON object into new columns (#56)

# `as.character.tbl_json()` converts `tbl_json` objects back into JSON character strings (#62)

## Bug fixes and minor changes

* `tbl_json` objects now print with a tidy character representation of the JSON attribute (#61)

* Use [purrr](https://github.com/jeremystan/purrr) for most list based internal operations (#1)

* Use [tidyr](https://github.com/hadley/tidyr) for `gather_array` and `gather_keys` functions (#28)

* Imported the magrittr `%>%` operator (#17)

* Fixed `dplyr::slice()` not working correctly with `tbl_json` objects (#18)

* First argument to verbs is `.x` rather than `x` to avoid name conflicts in NSE (#23)

* Fixed `spread_values()` to not coerce types (#24)

## Documentation changes

* "Visualizing JSON" vignette for visualizing the structure of complex JSON data, like the `companies` example (#4)

* Significant updates to all documentation and examples for clarity (#42)

## Other changes

* Migrated development to [jeremystan](https://github.com/jeremystan/tidyjson) from [sailthru](https://github.com/sailthru/tidyjson)