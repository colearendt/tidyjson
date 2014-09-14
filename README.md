# jplyr

jplyr is a complementary set of tools to (dplyr)[http://github.com/hadley/dplyr]
for working with JSON data (hence the `j`-plyr name). It's primary objective is 
to turn JSON data into (tidy)[http://vita.had.co.nz/papers/tidy-data.pdf] tables 
for downstream use by dplyr or other relational, analytical or machine learning 
frameworks in R.

jplyr uses [rjson](http://cran.r-project.org/web/packages/rjson/index.html) to
parse the json data into nested lists.

You can install from github directly by running:

```R
devtools::install_github("sailthru/jplyr")
```

jplyr comes with several JSON examples pulled from APIs:

* `commits`: commit data for the dplyr repo from github API
* `issues`: issue data for the dplyr repo from github API

Note that the jplyr package closely follows the definition and semantics of the
[JSON standard](http://json.org/).

An example of how jplyr works is as follows:

```R
library(jplyr) # for functions
library(dplyr) # for %>% and other dplyr functions

json <- '[{"name": "bob", "age": 32}, {"name": "susan", "age": 54}]'

json %>%         # Use the %>% pipe operator to pass json through a pipeline 
  as.jdf %>%     # Parse the JSON and setup a 'jdf' object
  jarray %>%     # 'stack' the array by index
  jvalue(        # Extract (several) values
    user.name = jstring("name"),  # Extract the "name" object as a character column "user.name"
    user.age = jnumber("age")     # Extract the "age" object as a numeric column "user.age"
  )
#  document.id array.index user.name user.age
#1           1           1       bob       32
#2           1           2     susan       54
```

For more complex uses, see the examples in `help(commits)` and `help(issues)`.


