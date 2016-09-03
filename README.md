<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyjson
========

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tidyjson)](http://cran.r-project.org/package=tidyjson) [![Build Status](https://travis-ci.org/jeremystan/tidyjson.svg?branch=master)](https://travis-ci.org/jeremystan/tidyjson) [![Coverage Status](https://img.shields.io/codecov/c/github/jeremystan/tidyjson/master.svg)](https://codecov.io/github/jeremystan/tidyjson?branch=master)

![tidyjson graphs](https://cloud.githubusercontent.com/assets/2284427/18217882/1b3b2db4-7114-11e6-8ba3-07938f1db9af.png)

tidyjson provides tools for turning [json](http://www.json.org/) into [tidy](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html) data.

Installation
------------

Get the released version from CRAN:

``` r
install.packages("tidyjson")
```

or the development version from github:

``` r
devtools::install_github("jeremystan/tidyjson")
```

Examples
--------

The following example takes a character vector of 500 documents in the `worldbank` dataset and spreads out all objects into new columns

``` r
library(tidyjson)
library(dplyr)

worldbank %>% spread_all %>% tbl_df
#> # A tibble: 500 × 8
#>    document.id    boardapprovaldate          closingdate
#> *        <int>                <chr>                <chr>
#> 1            1 2013-11-12T00:00:00Z 2018-07-07T00:00:00Z
#> 2            2 2013-11-04T00:00:00Z                 <NA>
#> 3            3 2013-11-01T00:00:00Z                 <NA>
#> 4            4 2013-10-31T00:00:00Z                 <NA>
#> 5            5 2013-10-31T00:00:00Z 2019-04-30T00:00:00Z
#> 6            6 2013-10-31T00:00:00Z                 <NA>
#> 7            7 2013-10-29T00:00:00Z 2019-06-30T00:00:00Z
#> 8            8 2013-10-29T00:00:00Z                 <NA>
#> 9            9 2013-10-29T00:00:00Z 2018-12-31T00:00:00Z
#> 10          10 2013-10-29T00:00:00Z 2014-12-31T00:00:00Z
#> # ... with 490 more rows, and 5 more variables: countryshortname <chr>,
#> #   project_name <chr>, regionname <chr>, totalamt <dbl>, `_id.$oid` <chr>
```

However, some objects in `worldbank` are arrays, this example shows how to quickly summarize the top level structure of a JSON collection

``` r
worldbank %>% gather_keys %>% json_types %>% count(key, type)
#> Source: local data frame [8 x 3]
#> Groups: key [?]
#> 
#>                   key   type     n
#>                 <chr> <fctr> <int>
#> 1                 _id object   500
#> 2   boardapprovaldate string   500
#> 3         closingdate string   370
#> 4    countryshortname string   500
#> 5 majorsector_percent  array   500
#> 6        project_name string   500
#> 7          regionname string   500
#> 8            totalamt number   500
```

In order to capture the data in `majorsector_percent` we can use `enter_object` to enter into that object, `gather_array` to stack the array and `spread_all` to capture the object keys under the array.

``` r
worldbank %>%
  enter_object("majorsector_percent") %>%
  gather_array %>%
  spread_all %>%
  tbl_df
#> # A tibble: 1,405 × 4
#>    document.id array.index                                    Name Percent
#> *        <int>       <int>                                   <chr>   <dbl>
#> 1            1           1                               Education      46
#> 2            1           2                               Education      26
#> 3            1           3 Public Administration, Law, and Justice      16
#> 4            1           4                               Education      12
#> 5            2           1 Public Administration, Law, and Justice      70
#> 6            2           2 Public Administration, Law, and Justice      30
#> 7            3           1                          Transportation     100
#> 8            4           1        Health and other social services     100
#> 9            5           1                      Industry and trade      50
#> 10           5           2                      Industry and trade      40
#> # ... with 1,395 more rows
```

API
---

### Spreading objects into columns

-   `spread_all()` for spreading all object values into new columns, with nested objects having column names with concatenated keys

-   `spread_values()` for specifying a subset of object values to spread into new columns using the `jstring()`, `jnumber()` and `jlogical()` functions

### Object navigation

-   `enter_object()` for entering into an object by name, discarding all other JSON (and rows without the corresponding object key) and allowing further operations on the object value

-   `gather_keys()` for stacking all object values by key name, expanding the rows of the `tbl_json` object accordingly

### Array navigation

-   `gather_array()` for stacking all array values by index, expanding the rows of the `tbl_json` object accordingly

### JSON inspection

-   `json_types()` for identifying JSON data types

-   `json_length()` for computing the length of JSON data (can be larger than `1` for objects and arrays)

-   `json_complexity()` for computing the length of the unnested JSON, i.e., how many terminal leaves there are in a complex JSON structure

### JSON summarization

-   `json_structure()` for creating a single fixed column data.frame that recursively structures arbitrary JSON data

-   `json_schema()` for representing the schema of complex JSON, unioned across disparate JSON documents, and collapsing arrays to their most complex type representation

### JSON plotting

-   `plot_json_graph()` for plotting JSON (or `json_schema()`) as a graph using the `igraph` package

### Creating tbl\_json objects

-   `as.tbl_json()` for converting a string or character vector into a `tbl_json` object, or for converting a `data.frame` with a JSON column using the `json.column` argument

-   `tbl_json()` for combining a `data.frame` and associated `list` derived from JSON data into a `tbl_json` object

-   `read_json()` for reading JSON data from a file

### Converting tbl\_json objects

-   `as.character.tbl_json` for converting the JSON attribute of a `tbl_json` object back into a JSON character string

### Included JSON data

-   `commits`: commit data for the dplyr repo from github API

-   `issues`: issue data for the dplyr repo from github API

-   `worldbank`: world bank funded projects from [jsonstudio](http://jsonstudio.com/resources/)

-   `companies`: startup company data from [jsonstudio](http://jsonstudio.com/resources/)

Philosophy
----------

The goal is to turn complex JSON data, which is often represented as nested lists, into tidy data frames that can be more easily manipulated.

-   Work on a single JSON document, or on a collection of related documents (where the schema of each document may vary)
-   Perform complex JSON manipulation using the pipe, `%>%`, producing code that can be read from left to right
-   Guarantee the structure of the data produced, even if the input JSON structure changes (the exception being `spread_all`, which produces new columns based on the input JSON alone)
-   Allow for structuring in tidy form arbitrarily nested (arrays or objects) JSON Naturally handle 'ragged' arrays and / or objects (varying lengths by document)
-   Allow for extraction of data in values or key names
-   Ensure edge cases are handled correctly (especially empty data)
-   Integrate seamlessly with `dplyr`, allowing `tbl_json` objects to pipe into `dplyr` verbs, and (when reasonable) back into further `tidyjson` verbs

Related Work
------------

Tidyjson depends upon

-   [magrritr](https://github.com/smbache/magrittr) for the `%>%` pipe operator
-   [jsonlite](https://github.com/jeroenooms/jsonlite) for converting JSON strings into nested lists
-   [purrr](https://github.com/hadley/purrr) for list operators
-   [tidyr](https://github.com/hadley/tidyr) for unnesting and spreading

Further, there are other R packages that can be used to better understand JSON data

-   [listviewer](https://github.com/timelyportfolio/listviewer) for viewing JSON data interactively
