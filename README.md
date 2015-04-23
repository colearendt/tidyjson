# tidyjson

[![Build Status](https://travis-ci.org/sailthru/tidyjson.png?branch=master)](https://travis-ci.org/sailthru/tidyjson)

tidyjson is a complementary set of tools to [tidyr](https://github.com/hadley/tidyr)
for working with JSON data. It's primary objective is to turn JSON data into 
[tidy](http://vita.had.co.nz/papers/tidy-data.pdf) tables for downstream use by 
[dplyr](http://github.com/hadley/dplyr) or other relational, analytical or 
machine learning frameworks in R. Behind the scenes, tidyjson uses 
[jsonlite](https://github.com/jeroenooms/jsonlite) to parse the JSON data. 
tidyjson is also designed to be used with the `%>%` operator imported into dplyr
from the [magrittr](https://github.com/smbache/magrittr) package.

tidyjson operates on the following principles:

* Allow for structuring in tidy form arbitrarily nested (arrays or objects) JSON
* Naturally handle 'ragged' arrays and / or objects (varying lengths by document)
* Allow for extraction of data in values *or* key names
* Integrate with pipelines built on `dplyr` and the `%>%` operator
* Ensure edge cases are handled correctly (especially empty data)

You can install tidyjson from github directly by running:

```R
devtools::install_github("sailthru/tidyjson")
```

tidyjson comes with several JSON examples:

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
library(tidyjson)   # this package
library(dplyr)      # for %>% and other dplyr functions

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

The first step in using tidyjson is to convert your JSON into a `tbl_json` object.
Almost every function in tidyjson accepts a `tbl_json` object as it's first 
parameter, and returns a `tbl_json` object for downstream use. `tbl_json` 
inherits from `dplyr::tbl`.

A `tbl_json` object is comprised of a `data.frame` with an additional attribute,
`JSON`, that contains a list of JSON data of the same length as the number of
rows in the `data.frame`. Each row of data in the `data.frame` corresponds to the
JSON found in the same index of the `JSON` attribute.

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
c('{"key1": "value1"}', '{"key2": "value2"}') %>% as.tbl_json
```

Behind the scenes, `as.tbl_json` is parsing the JSON strings and creating a
data.frame with 1 column, `document.id`, which keeps track of the character 
vector position (index) where the JSON data came from.

## Verbs

The rest of tidyjson is comprised of various verbs with operate on `tbl_json`
objects and return `tbl_json` objects. They are meant to be used in a pipeline
with the `%>%` operator.

Note that these verbs all operate on *both* the underlying data.frame and the
JSON, iteratively moving data from the JSON into the data.frame. Any
modifications of the underlying data.frame outside of these operations
may produce unintended consequences where the data.frame and JSON become out of
synch.

### `json_types`

`json_types` inspects the JSON associated with each row of the data.frame, and
adds a new column (`type` by default) that identifies the type according to the
[JSON standard](http://json.org/).

```R
types <- c('{"a": 1}', '[1, 2]', '"a"', '1', 'true', 'null') %>% as.tbl_json %>%
   json_types
types$type
#[1] object  array   string  number  logical null
#Levels: object array string number logical null
```

This is particularly useful for inspecting your JSON data types, and can added
after `gather_array` (or `gather_keys`) to inspect the types of the elements
(or values) in arrays (or objects).

### `gather_array`

`gather_array` takes JSON arrays and duplicates the rows in the data.frame to
correspond to the indices of the array, and puts the elements of the array into
the JSON attribute. This is equivalent to "stacking" the array in the
data.frame, and lets you continue to manipulate the remaining JSON in the
elements of the array.

```R
'[1, "a", {"k": "v"}]' %>% as.tbl_json %>% gather_array %>% json_types
#  document.id array.index   type
#1           1           1 number
#2           1           2 string
#3           1           3 object
```

This allows you to *enter into* an array and begin processing it's elements
with other tidyjson functions. It retains the array.index in case the relative
position of elements in the array is useful information.

### `gather_keys`

Similar to `gather_array`, `gather_keys` takes JSON objects and duplicates the
rows in the data.frame to correspond to the keys of the object, and puts the 
values of the object into the JSON attribute.

```R
'{"name": "bob", "age": 32}' %>% as.tbl_json %>% gather_keys %>% json_types
#  document.id  key   type
#1           1 name string
#2           1  age number
```

This allows you to *enter into* the keys of the objects just like `gather_array`
let you enter elements of the array.

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

### `enter_object`

`enter_object` lets you dive into a specific object key in the JSON attribute,
so that all further tidyjson calls happen inside that object (all other JSON 
data outside the object is discarded). If the object doesn't exist for a given
row / index, then that data.frame row will be discarded.

```R
c('{"name": "bob", "children": ["sally", "george"]}', '{"name": "anne"}') %>% 
  as.tbl_json %>% spread_values(parent.name = jstring("name")) %>%
  enter_object("children") %>% 
  gather_array %>% append_values_string("children")
#  document.id parent.name array.index children
#1           1         bob           1    sally
#2           1         bob           2   george
```

This is useful when you want to limit your data to just information found in
a specific key.

## Strategies

When beginning to work with JSON data, you often don't have easy access to a
schema describing what is in the JSON. One of the benefits of document oriented
data structures is that they let developers create data without having to worry
about defining the schema explicitly.

Thus, the first step is to usually understand the structure of the JSON. A first
step can be to look at individual records with `jsonlite::prettify`:

```
library(jsonlite)
prettify(json)
```

Examining various random records can begin to give you a sense of what the JSON
contains and how it it structured. However, keep in mind that in many cases
documents that are missing data (either unknown or unrelevant) may omit the
entire JSON structure.

Next, you can begin working with the data in R.

```R
# Inspect the types of objects
read_json("myfile.json") %>% json_types %>% table
```

Then, if you want to work with a single row of data for each JSON object, use
`spread_values` to get at (potentially nested) key-value pairs.

If all you care about is data from a certain sub-object, then use `enter_object`
to dive into that object directly. Make sure you first use `spread_values` to
capture any top level identifiers you might need for analytics, summarization or
relational uses downstream.

If you want to access arrays, use `gather_array` to stack their elements, and
then proceed as though you had separate documents. (Again, first spread any
top-level keys you need.)

Finally, if you have data where information is encoded in both keys and values,
then consider using `gather_keys` and `append_values_X` where `X` is the type
of JSON scalar data you expect in the values.

It's important to remember that any of the above can be combined together
iteratively to do some fairly complex data extraction. For example:

```R
json <- '{
  "name": "bob",
  "shopping cart": 
    [
      {
        "date": "2014-04-02",
        "basket": {"books": 2, "shirts": 0}
      },
      {
        "date": "2014-08-23",
        "basket": {"books": 1}
      }
    ]
}'
json %>% as.tbl_json %>% 
  spread_values(customer = jstring("name")) %>% # Keep the customer name
  enter_object("shopping cart") %>%             # Look at their cart
  gather_array %>%                              # Expand the data.frame and dive into each array element
  spread_values(date = jstring("date")) %>%     # Keep the date of the cart
  enter_object("basket") %>%                    # Look at their basket
  gather_keys("product") %>%                    # Expand the data.frame for each product and capture it's name
  append_values_number("quantity")              # Capture the values as the quantity
#  document.id customer array.index       date product quantity
#1           1      bob           1 2014-04-02   books        2
#2           1      bob           1 2014-04-02  shirts        0
#3           1      bob           2 2014-08-23   books        1
```

Note that there are often situations where there are multiple arrays or objects
of differing types that exist at the same level of the JSON hierarchy. In this
case, you need to use `enter_object` to enter each of them in *separate*
pipelines to create *separate* `data.frames` that can then be joined 
relationally.

Finally, don't forget that once you are done with your JSON tidying, you can
use [dplyr](http://github.com/hadley/dplyr) to continue manipulating the
resulting data at your leisure!
