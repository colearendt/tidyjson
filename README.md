# tidyjson

tidyjson is a complementary set of tools to [tidyr](https://github.com/hadley/tidyr)
for working with JSON data. It's primary objective is to turn JSON data into 
[tidy](http://vita.had.co.nz/papers/tidy-data.pdf) tables for downstream use by 
[dplyr](http://github.com/hadley/dplyr) or other relational, analytical or 
machine learning frameworks in R. Behind the scenes, tidyjson uses 
[rjson](http://cran.r-project.org/web/packages/rjson/index.html) 
to quickly parse the JSON data. Tidyjson is also designed to be used with the 
%>% operator imported into dplyr from the 
[magrittr](https://github.com/smbache/magrittr) package. 

You can install tidyjson from github directly by running:

```R
devtools::install_github("sailthru/tidyjson")
```

tidyjson comes with several JSON examples pulled from APIs:

* `commits`: commit data for the dplyr repo from github API
* `issues`: issue data for the dplyr repo from github API
* `worldbank`: world bank funded projects from 
[jsonstudio](http://jsonstudio.com/resources/)
* `companies`: startup company data from 
[jsonstudio](http://jsonstudio.com/resources/)

Note that the tidyjson package closely follows the definition and semantics of 
the [JSON standard](http://json.org/).

An example of how tidyjson works is as follows:

```R
library(tidyjson) # for functions
library(dplyr)    # for %>% and other dplyr functions

json <- '[{"name": "bob", "age": 32}, {"name": "susan", "age": 54}]'

json %>%            # Use the %>% pipe operator to pass json through a pipeline 
  as.tbl_json %>%   # Parse the JSON and setup a 'tbl_json' object
  gather_array %>%  # Gather (stack) the array by index
  spread_values(    # Spread (widen) values to widen the data.frame
    user.name = jstring("name"),  # Extract the "name" object as a character column "user.name"
    user.age = jnumber("age")     # Extract the "age" object as a numeric column "user.age"
  )
#  document.id array.index user.name user.age
#1           1           1       bob       32
#2           1           2     susan       54
```

For more complex uses, see the examples in `help(commits)`, `help(issues)`,
`help(worldbank)` and `help(companies)`.

## `tbl_json`

The first step in using tidyjson is to convert your JSON into a tbl_json object.
Almost every function in tidyjson accepts a `tbl_json` object as it's first 
parameter, and returns a `tbl_json` object for downstream use. `tbl_json` 
inherits from `dplyr::tbl`.

A `tbl_json` object is comprised of a `data.frame` with an additional attribute,
`JSON`, that contains a list of JSON data of the same length as the number of
rows in the data.frame. Each row of data in the data.frame corresponds to the
JSON found in the same index of the JSON attribute.

The easiest way to construct a `tbl_json` object is directly from a character
string or vector.

```R
# Will return a 1 row data.frame with a length 1 JSON attribute
'{"key": "value"}' %>% as.tbl_json

# Will still return a 1 row data.frame with a length 1 JSON attribute as
# the character string is of length 1 (even though it contains a JSON array of
# length 2)
'[{"key1": "value1"}, {"key2": "value2"}]' %>% as.tbl_json

# Will return a 2 row data.frame with a length 2 JSON attribute
c('{"key1": "value1"}', '{"key2": "value2"}' %>% as.tbl_json
```

Behind the scenes, as.tbl_json is parsing the JSON strings and creating a
data.frame with 1 column, document.id, which keeps track of the character vector
position (index) where the JSON data came from.

## Verbs

The rest of tidyjson is comprised of various verbs with operate on `tbl_json`
objects and return `tbl_json` objects. They are meant to be used in a pipeline
with the `%>%` operator.

Note that these verbs all operate on *both* the underlying data.frame and the
JSON, iteratively moving data from the JSON into the data.frame. Any
modifications of the data.frame may produce unintended consequences where the
data.frame and JSON become out of synch.

### `json_types`

`json_types` inspects the JSON associated with each row of the data.frame, and
adds a new column that identifies the type according to the 
[JSON standard](http://json.org/). This is particularly useful for inspecting
your JSON data types, and can added after `gather_array` (or `gather_keys`)
to inspect the types of the elements (or values) in arrays (or objects).

### `gather_array`

`gather_array` takes JSON arrays and duplicates the rows in the data.frame to
correspond to the indices of the array, and puts the elements of the array into
the JSON attribute. This is equivalent to "stacking" the array in the
data.frame, and lets you continue to manipulate the remaining JSON in the
elements of the array.

### `gather_keys`

Similar to `gather_array`, `gather_keys` takes JSON objects and duplicates the
rows in the data.frame to correspond to the keys of the object, and puts the 
values of the object into the JSON attribute.

### `spread_values`

`spread_values` lets you dive into (potentially nested) JSON objects and
extract specific values. `spread_values` takes `jstring`, `jnumber` or 
`jlogical` function calls as arguments in order to specify the type of the
data that should be captured at each desired key location. 

These values can be of varying types at varying depths, e.g.,

```R
'{"name": {"first": "bob", "last": "jones"}, "age": 32}' %>% as.tbl_json %>% 
  spread_values(first.name = jstring("name", "first"), age = jnumber("age"))
#  document.id first.name age
#1           1        bob  32
```

### `enter_object`

`enter_object` lets you dive into a specific object key in the JSON attribute,
so that all further tidyjson calls happen inside that object (all other JSON 
data outside the object is discarded). If the object doesn't exist for a given
row / index, then that data.frame row will be discarded.

This is useful when you want to limit your data to just information found in
a specific key. 

### `append_values_X`

The `append_values_X` functions let you take the remaining JSON and add it as
a column X (for X in "string", "number", "logical") insofar as it is of the
JSON type specified. For example:

```R
'{"first": "bob", "last": "jones"}' %>% as.tbl_json %>% 
  gather_keys() %>% append_values_string()
#  document.id   key string
#1           1 first    bob
#2           1  last  jones
```

Any values that do not conform to the type specified will be NA in the resulting
column. This includes other scalar types (e.g., numbers or logicals if you are
using append_values_string) and *also* any rows where the JSON is still an
object or an array.