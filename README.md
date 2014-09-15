# tidyjson

tidyjson is a complementary set of tools to [tidyr](https://github.com/hadley/tidyr)
for working with JSON data. It's primary objective is to turn JSON data into 
[tidy](http://vita.had.co.nz/papers/tidy-data.pdf) tables for downstream use by 
[dplyr](http://github.com/hadley/dplyr) or other relational, analytical or 
machine learning frameworks in R. Behind the scenes, tidyjson uses 
[rjson](http://cran.r-project.org/web/packages/rjson/index.html) 
to quickly parse the JSON data.

You can install tidyjson from github directly by running:

```R
devtools::install_github("sailthru/tidyjson")
```

tidyjson comes with several JSON examples pulled from APIs:

* `commits`: commit data for the dplyr repo from github API
* `issues`: issue data for the dplyr repo from github API
* `worldbank`: world bank funded projects from 
[jsonstudio](http://jsonstudio.com/resources/)

Note that the tidyjson package closely follows the definition and semantics of 
the [JSON standard](http://json.org/).

An example of how tidyjson works is as follows:

```R
library(tidyjson) # for functions
library(dplyr)    # for %>% and other dplyr functions

json <- '[{"name": "bob", "age": 32}, {"name": "susan", "age": 54}]'

json %>%          # Use the %>% pipe operator to pass json through a pipeline 
  as.jdf %>%      # Parse the JSON and setup a 'jdf' object
  jarray %>%      # 'stack' the array by index
  jvalue(         # Extract (several) values
    user.name = jstring("name"),  # Extract the "name" object as a character column "user.name"
    user.age = jnumber("age")     # Extract the "age" object as a numeric column "user.age"
  )
#  document.id array.index user.name user.age
#1           1           1       bob       32
#2           1           2     susan       54
```

For more complex uses, see the examples in `help(commits)`, `help(issues)`,
and `help(worldbank)`.


