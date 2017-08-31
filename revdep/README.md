# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.4.1 (2017-06-30) |
|system   |x86_64, linux-gnu            |
|ui       |X11                          |
|language |en_US                        |
|collate  |en_US.UTF-8                  |
|tz       |America/New_York             |
|date     |2017-08-29                   |

## Packages

|package      |*  |version    |date       |source                         |
|:------------|:--|:----------|:----------|:------------------------------|
|assertthat   |   |0.2.0      |2017-04-11 |cran (@0.2.0)                  |
|covr         |   |3.0.0      |2017-06-26 |cran (@3.0.0)                  |
|dplyr        |   |0.7.2      |2017-07-20 |cran (@0.7.2)                  |
|forcats      |   |0.2.0      |2017-01-23 |cran (@0.2.0)                  |
|ggplot2      |   |2.2.1      |2016-12-30 |cran (@2.2.1)                  |
|igraph       |   |1.1.2      |2017-07-21 |cran (@1.1.2)                  |
|jsonlite     |   |1.5        |2017-06-01 |cran (@1.5)                    |
|knitr        |   |1.17       |2017-08-10 |cran (@1.17)                   |
|listviewer   |   |1.4.0      |2016-11-03 |cran (@1.4.0)                  |
|lubridate    |   |1.6.0      |2016-09-13 |cran (@1.6.0)                  |
|magrittr     |   |1.5        |2014-11-22 |cran (@1.5)                    |
|needs        |   |0.0.3      |2016-03-28 |cran (@0.0.3)                  |
|purrr        |   |0.2.3      |2017-08-02 |cran (@0.2.3)                  |
|RColorBrewer |   |1.1-2      |2014-12-07 |cran (@1.1-2)                  |
|rmarkdown    |   |1.6        |2017-06-15 |cran (@1.6)                    |
|testthat     |   |1.0.2      |2016-04-23 |cran (@1.0.2)                  |
|tibble       |   |1.3.4      |2017-08-22 |cran (@1.3.4)                  |
|tidyjson     |   |0.2.1.9001 |2017-08-29 |local (colearendt/tidyjson@NA) |
|tidyr        |   |0.7.0      |2017-08-16 |cran (@0.7.0)                  |
|viridis      |   |0.4.0      |2017-03-27 |cran (@0.4.0)                  |
|wordcloud    |   |2.5        |2014-06-13 |cran (@2.5)                    |

# Check results

1 packages

|package     |version | errors| warnings| notes|
|:-----------|:-------|------:|--------:|-----:|
|fingertipsR |0.1.0   |      1|        2|     0|

## fingertipsR (0.1.0)
Maintainer: Sebastian Fox <sebastian.fox@phe.gov.uk>  
Bug reports: https://github.com/PublicHealthEngland/fingertipsR/issues

1 error  | 2 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’ [33s/249s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  4: In gather_array(.) :
    array.index column name already exists, changing to array.index.2
  5: In gather_array(.) :
    array.index column name already exists, changing to array.index.2
  6: In gather_array(.) :
    array.index column name already exists, changing to array.index.2
  7: In gather_array(.) :
    array.index column name already exists, changing to array.index.2
  8: In gather_array(.) :
    array.index column name already exists, changing to array.index.2
  9: In gather_array(.) :
    array.index column name already exists, changing to array.index.2
  10: In gather_array(.) :
    array.index column name already exists, changing to array.index.2
  Execution halted

checking whether package ‘fingertipsR’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import ‘dplyr::bind_rows’ by ‘tidyjson::bind_rows’ when loading ‘fingertipsR’
See ‘/home/carendt/r/tidyjson/revdep/checks/fingertipsR.Rcheck/00install.out’ for details.

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Warning in engine$weave(file, quiet = quiet, encoding = enc) :
  Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
Quitting from lines 72-74 (lifeExpectancy.Rmd) 
Error: processing vignette 'lifeExpectancy.Rmd' failed with diagnostics:
there is no package called 'webshot'
Execution halted

```

