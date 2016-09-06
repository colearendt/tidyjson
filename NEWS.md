# purrr 0.2.1.9000

## New functions

* `json_complexity()` computes the "complexity" (recursively unlisted length) of JSON data (#5)

* `json_structure()` recursively structures arbitrary JSON data into a single data frame (#2)

* `json_schema()` creates a schema for a JSON document or collection (#12)

* `is_json` functions for testing JSON types, such as `is_json_string()`, `is_json_null()` or `is_json_object()` (#39)

* `plot_json_graph()` plots an [igraph](https://github.com/igraph/igraph) visualization of a JSON document (#47)

* `spread_all()` spreads all scalar values of a JSON object into new columns (#56)

* `as.character.tbl_json()` converts `tbl_json` objects back into JSON character strings (#62)

* `gather_object()` replaces `gather_keys()`, with default `column.name` of `name` instead of `key` (#66). This more closely matches the [JSON standard](http://www.json.org/), which refers to objects as name-value pairs, and is now consistent with `gather_array()`.

## Documentation changes

* "Visualizing JSON" vignette for visualizing the structure of complex JSON data, like the `companies` example (#4)

* Significant updates to all documentation and examples for clarity (#42)

## Bug fixes and minor changes

* `tbl_json` objects now print with a tidy character representation of the JSON attribute (#61)

* Use [purrr](https://github.com/jeremystan/purrr) for most list based internal operations (#1)

* Use [tidyr](https://github.com/hadley/tidyr) for `gather_array` and `gather_object` functions (#28)

* Imported the magrittr `%>%` operator (#17)

* Fixed `dplyr::slice()` not working correctly with `tbl_json` objects (#18)

* First argument to verbs is `.x` rather than `x` to avoid name conflicts in NSE (#23)

* Fixed `spread_values()` to not coerce types (#24)

* `gather_array()` and `gather_object()` can be called repeatedly in the same pipeline with the same `column.name` argument, and will simply append an integer identifer to the new columns (#38)

## Other changes

* Migrated development to [jeremystan](https://github.com/jeremystan/tidyjson) from [sailthru](https://github.com/sailthru/tidyjson)

## Deprecated functions

* `gather_keys()` -> use `gather_object()` instead
