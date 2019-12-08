# tidyjson 0.2.4

## New functions

* Add `bind_rows()` support.  Though currently not an S3 implementation, it behaves as much like the `dplyr` variant as possible, preserving the `attr(.,'JSON')` components if all components are `tbl_json` objects. (#58)

## Documentation Changes

* "Using Multiple APIs" vignette added to show support for using tidyjson with multiple APIs (#85)

* Updated README.md to better explain `spread_all()` (#92)

## Bug fixes and minor changes

* Improve compatibility with newer `dplyr` and `tidyr`

* `DROP=TRUE` caused an error.  Altered behavior to be consistent with `tbl_df`

* Fix `spread_all(recursive=FALSE)` bug that caused an error (#65)

* Alter `spread_all()` behavior to recursively check for deduplication of names (and thus avoid an error) (#76)

* Add named support for the `NSE` versions of dplyr functions (`filter()`,`mutate()`,`slice()`, etc.) since the `SE` variants are no longer called behind-the-scenes since `dplyr 0.6.0`.  (#97)

* Fix errors with `print.tbl_json()` when the JSON attribute is missing

* Fix json_structure() failure if `document.id` missing by imputing 
the missing `document.id`.  (#86)

# tidyjson 0.2.2

## New functions

* `json_complexity()` computes the "complexity" (recursively unlisted length) of JSON data (#5)

* `json_structure()` recursively structures arbitrary JSON data into a single data frame (#2)

* `json_schema()` creates a schema for a JSON document or collection (#12)

* `is_json` functions for testing JSON types, such as `is_json_string()`, `is_json_null()` or `is_json_object()` (#39)

* `spread_all()` spreads all scalar values of a JSON object into new columns (#56)

* `as.character.tbl_json()` converts `tbl_json` objects back into JSON character strings (#62)

* `gather_object()` replaces `gather_keys()`, with default `column.name` of `name` instead of `key` (#66). This more closely matches the [JSON standard](http://www.json.org/), which refers to objects as name-value pairs, and is now consistent with `gather_array()`.

## Documentation Changes

* "Using Multiple APIs" vignette added to show support for using tidyjson with multiple APIs (#85)

* Updated README.md to better explain `spread_all()` (#92)

* "Visualizing JSON" vignette for visualizing the structure of complex JSON data, like the `companies` example (#4)

* Significant updates to all documentation and examples for clarity (#42)

* Updated "Introduction to tidyjson" vignette to be more concise and use new functionality (#74)

## Bug fixes and minor changes

* `enter_object` and the `jstring`, `jnumber` and `jlogical` functions now accept unqoted strings to specify their path (#26)

* `tbl_json` objects now print with a tidy character representation of the JSON attribute (#61)

* Use [purrr](https://github.com/tidyverse/purrr) for most list based internal operations (#1)

* Use [tidyr](https://github.com/tidyverse/tidyr) for `gather_array` and `gather_object` functions (#28)

* Imported the magrittr `%>%` operator (#17)

* Fixed `dplyr::slice()` not working correctly with `tbl_json` objects (#18)

* First argument to verbs is `.x` rather than `x` to avoid name conflicts in NSE (#23)

* Fixed `spread_values()` to not coerce types (#24)

* `gather_array()` and `gather_object()` can be called repeatedly in the same pipeline with the same `column.name` argument, and will simply append an integer identifer to the new columns (#38)

## Other changes

* Migrated development to [colearendt](https://github.com/colearendt/tidyjson) from [jeremystan](https://github.com/jeremystan/tidyjson) and [sailthru](https://github.com/sailthru/tidyjson)

## Deprecated functions

* `gather_keys()` -> use `gather_object()` instead
