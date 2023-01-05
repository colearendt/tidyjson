tidyjson
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/tidyjson)](https://cran.r-project.org/package=tidyjson)
[![Build
Status](https://travis-ci.org/colearendt/tidyjson.svg?branch=master)](https://travis-ci.org/colearendt/tidyjson)
[![Coverage
Status](https://codecov.io/github/colearendt/tidyjson/coverage.svg?branch=master)](https://codecov.io/github/colearendt/tidyjson?branch=master)

[![CRAN
Activity](http://cranlogs.r-pkg.org/badges/tidyjson)](https://cran.r-project.org/package=tidyjson/index.html)
[![CRAN
History](http://cranlogs.r-pkg.org/badges/grand-total/tidyjson)](https://cran.r-project.org/package=tidyjson/index.html)

![tidyjson
graphs](https://cloud.githubusercontent.com/assets/2284427/18217882/1b3b2db4-7114-11e6-8ba3-07938f1db9af.png)

tidyjson provides tools for turning complex [json](http://www.json.org/)
into
[tidy](https://cran.r-project.org/package=tidyr/vignettes/tidy-data.html)
data.

## Installation

Get the released version from CRAN:

``` r
install.packages("tidyjson")
```

or the development version from github:

``` r
devtools::install_github("colearendt/tidyjson")
```

## Examples

The following example takes a character vector of 500 documents in the
`worldbank` dataset and spreads out all objects.  
Every JSON object key gets its own column with types inferred, so long
as the key does not represent an array. When `recursive=TRUE` (the
default behavior), `spread_all` does this recursively for nested objects
and creates column names using the `sep` parameter (i.e. `{"a":{"b":1}}`
with `sep='.'` would generate a single column: `a.b`).

``` r
library(dplyr)
library(tidyjson)

worldbank %>% spread_all
#> # A tbl_json: 500 x 9 tibble with a "JSON" attribute
#>    ..JSON        docum…¹ board…² closi…³ count…⁴ proje…⁵ regio…⁶ total…⁷ _id.$…⁸
#>    <chr>           <int> <chr>   <chr>   <chr>   <chr>   <chr>     <dbl> <chr>  
#>  1 "{\"_id\":{\…       1 2013-1… 2018-0… Ethiop… Ethiop… Africa   1.3 e8 52b213…
#>  2 "{\"_id\":{\…       2 2013-1… <NA>    Tunisia TN: DT… Middle…  0      52b213…
#>  3 "{\"_id\":{\…       3 2013-1… <NA>    Tuvalu  Tuvalu… East A…  6.06e6 52b213…
#>  4 "{\"_id\":{\…       4 2013-1… <NA>    Yemen,… Gov't … Middle…  0      52b213…
#>  5 "{\"_id\":{\…       5 2013-1… 2019-0… Lesotho Second… Africa   1.31e7 52b213…
#>  6 "{\"_id\":{\…       6 2013-1… <NA>    Kenya   Additi… Africa   1   e7 52b213…
#>  7 "{\"_id\":{\…       7 2013-1… 2019-0… India   Nation… South …  5   e8 52b213…
#>  8 "{\"_id\":{\…       8 2013-1… <NA>    China   China … East A…  0      52b213…
#>  9 "{\"_id\":{\…       9 2013-1… 2018-1… India   Rajast… South …  1.6 e8 52b213…
#> 10 "{\"_id\":{\…      10 2013-1… 2014-1… Morocco MA Acc… Middle…  2   e8 52b213…
#> # … with 490 more rows, and abbreviated variable names ¹​document.id,
#> #   ²​boardapprovaldate, ³​closingdate, ⁴​countryshortname, ⁵​project_name,
#> #   ⁶​regionname, ⁷​totalamt, ⁸​`_id.$oid`
```

Some objects in `worldbank` are arrays, which are not handled by
`spread_all`. This example shows how to quickly summarize the top level
structure of a JSON collection

``` r
worldbank %>% gather_object %>% json_types %>% count(name, type)
#> # A tibble: 8 × 3
#>   name                type       n
#>   <chr>               <fct>  <int>
#> 1 _id                 object   500
#> 2 boardapprovaldate   string   500
#> 3 closingdate         string   370
#> 4 countryshortname    string   500
#> 5 majorsector_percent array    500
#> 6 project_name        string   500
#> 7 regionname          string   500
#> 8 totalamt            number   500
```

In order to capture the data in the `majorsector_percent` array, we can
use `enter_object` to enter into that object, `gather_array` to stack
the array and `spread_all` to capture the object items under the array.

``` r
worldbank %>%
  enter_object(majorsector_percent) %>%
  gather_array %>%
  spread_all %>%
  select(-document.id, -array.index)
#> # A tbl_json: 1,405 x 3 tibble with a "JSON" attribute
#>    ..JSON                  Name                                    Percent
#>    <chr>                   <chr>                                     <dbl>
#>  1 "{\"Name\":\"Educat..." Education                                    46
#>  2 "{\"Name\":\"Educat..." Education                                    26
#>  3 "{\"Name\":\"Public..." Public Administration, Law, and Justice      16
#>  4 "{\"Name\":\"Educat..." Education                                    12
#>  5 "{\"Name\":\"Public..." Public Administration, Law, and Justice      70
#>  6 "{\"Name\":\"Public..." Public Administration, Law, and Justice      30
#>  7 "{\"Name\":\"Transp..." Transportation                              100
#>  8 "{\"Name\":\"Health..." Health and other social services            100
#>  9 "{\"Name\":\"Indust..." Industry and trade                           50
#> 10 "{\"Name\":\"Indust..." Industry and trade                           40
#> # … with 1,395 more rows
```

## API

### Spreading objects into columns

- `spread_all()` for spreading all object values into new columns, with
  nested objects having concatenated names

- `spread_values()` for specifying a subset of object values to spread
  into new columns using the `jstring()`, `jinteger()`, `jdouble()` and
  `jlogical()` functions. It is possible to specify multiple parameters
  to extract data from nested objects (i.e. `jstring('a','b')`).

### Object navigation

- `enter_object()` for entering into an object by name, discarding all
  other JSON (and rows without the corresponding object name) and
  allowing further operations on the object value

- `gather_object()` for stacking all object name-value pairs by name,
  expanding the rows of the `tbl_json` object accordingly

### Array navigation

- `gather_array()` for stacking all array values by index, expanding the
  rows of the `tbl_json` object accordingly

### JSON inspection

- `json_types()` for identifying JSON data types

- `json_length()` for computing the length of JSON data (can be larger
  than `1` for objects and arrays)

- `json_complexity()` for computing the length of the unnested JSON,
  i.e., how many terminal leaves there are in a complex JSON structure

- `is_json` family of functions for testing the type of JSON data

### JSON summarization

- `json_structure()` for creating a single fixed column data.frame that
  recursively structures arbitrary JSON data

- `json_schema()` for representing the schema of complex JSON, unioned
  across disparate JSON documents, and collapsing arrays to their most
  complex type representation

### Creating tbl_json objects

- `as.tbl_json()` for converting a string or character vector into a
  `tbl_json` object, or for converting a `data.frame` with a JSON column
  using the `json.column` argument

- `tbl_json()` for combining a `data.frame` and associated `list`
  derived from JSON data into a `tbl_json` object

- `read_json()` for reading JSON data from a file

### Converting tbl_json objects

- `as.character.tbl_json` for converting the JSON attribute of a
  `tbl_json` object back into a JSON character string

### Included JSON data

- `commits`: commit data for the dplyr repo from github API

- `issues`: issue data for the dplyr repo from github API

- `worldbank`: world bank funded projects from jsonstudio

- `companies`: startup company data from jsonstudio

## Philosophy

The goal is to turn complex JSON data, which is often represented as
nested lists, into tidy data frames that can be more easily manipulated.

- Work on a single JSON document, or on a collection of related
  documents

- Create pipelines with `%>%`, producing code that can be read from left
  to right

- Guarantee the structure of the data produced, even if the input JSON
  structure changes (with the exception of `spread_all`)

- Work with arbitrarily nested arrays or objects

- Handle ‘ragged’ arrays and / or objects (varying lengths by document)

- Allow for extraction of data in values or object names

- Ensure edge cases are handled correctly (especially empty data)

- Integrate seamlessly with `dplyr`, allowing `tbl_json` objects to pipe
  in and out of `dplyr` verbs where reasonable

## Related Work

Tidyjson depends upon

- [magrritr](https://github.com/tidyverse/magrittr) for the `%>%` pipe
  operator
- [jsonlite](https://github.com/jeroen/jsonlite) for converting JSON
  strings into nested lists
- [purrr](https://github.com/tidyverse/purrr) for list operators
- [tidyr](https://github.com/tidyverse/tidyr) for unnesting and
  spreading

Further, there are other R packages that can be used to better
understand JSON data

- [listviewer](https://github.com/timelyportfolio/listviewer) for
  viewing JSON data interactively
