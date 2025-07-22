
# cougarRA

<!-- badges: start -->
<!-- badges: end -->

The goal of cougarRA is to make election survey analysis easy. 

## Installation

You can install the development version of cougarRA from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("phdavis8/cougarRA")
```

## Example

This is an example of the weighted_crosstab function

``` r
library(cougarRA)
## basic example code
weighted_crosstab(data = df, x = x variable, y = y variable, weight = weight, digits = 2, total_row = FALSE)
```

