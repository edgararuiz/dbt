
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dbt

<!-- badges: start -->
<!-- badges: end -->

The goal of `dbt` is to provide a simple interface to run a battery of
tests of `dplyr` operations against a new back-end, such as a new
database translation.

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("edgararuiz/dbt")
```

## Using

``` r
library(dbt)
library(pracma)

script_results <- dbt_read_run_script(silent = TRUE)
#> Warning: Outer names are only allowed for unnamed scalar atomic inputs

script_results
#> Succesful tests: 75
#> Test with errors: 0
#> Failed tests: 0
#> Total number of tests: 75
#> 
```

``` r
dbt_read_run_script()
#> SUCCESS | mutate() | acos | acos(1/fld_double)
#> SUCCESS | filter() | acos | acos(1/fld_double) < 1
#> SUCCESS | summarise() | acos | sum(acos(1/fld_double), na.rm = TRUE)
#> SUCCESS | group_by() | acos | acos(1/fld_double)
#> SUCCESS | arrange() | acos | acos(1/fld_double)
#> SUCCESS | mutate() | acosh | acosh(abs(fld_double) + 1)
#> SUCCESS | filter() | acosh | acosh(abs(fld_double) + 1) < 10
#> SUCCESS | summarise() | acosh | sum(acosh(abs(fld_double) + 1), na.rm = TRUE)
#> SUCCESS | group_by() | acosh | acosh(abs(fld_double) + 1)
#> SUCCESS | arrange() | acosh | acosh(abs(fld_double) + 1)
#> SUCCESS | mutate() | asin | asin(1/fld_double)
#> SUCCESS | filter() | asin | asin(1/fld_double) < 1
#> SUCCESS | summarise() | asin | sum(asin(1/fld_double), na.rm = TRUE)
#> SUCCESS | group_by() | asin | asin(1/fld_double)
#> SUCCESS | arrange() | asin | asin(1/fld_double)
#> SUCCESS | mutate() | asinh | asinh(fld_double)
#> SUCCESS | filter() | asinh | asinh(fld_double) < 10
#> SUCCESS | summarise() | asinh | sum(asinh(fld_double), na.rm = TRUE)
#> SUCCESS | group_by() | asinh | asinh(fld_double)
#> SUCCESS | arrange() | asinh | asinh(fld_double)
#> SUCCESS | mutate() | atan | atan(fld_double)
#> SUCCESS | filter() | atan | atan(fld_double) < 0.5
#> SUCCESS | summarise() | atan | sum(atan(fld_double), na.rm = TRUE)
#> SUCCESS | group_by() | atan | atan(fld_double)
#> SUCCESS | arrange() | atan | atan(fld_double)
#> SUCCESS | mutate() | atan2 | atan2(fld_double, fld_double)
#> SUCCESS | filter() | atan2 | atan2(fld_double, fld_double) < 0.5
#> SUCCESS | summarise() | atan2 | sum(atan2(fld_double, fld_double), na.rm = TRUE)
#> SUCCESS | group_by() | atan2 | atan2(fld_double, fld_double)
#> SUCCESS | arrange() | atan2 | atan2(fld_double, fld_double)
#> SUCCESS | mutate() | atanh | atanh(1/fld_double)
#> SUCCESS | filter() | atanh | atanh(1/fld_double) < 10
#> SUCCESS | summarise() | atanh | sum(atanh(1/fld_double), na.rm = TRUE)
#> SUCCESS | group_by() | atanh | atanh(1/fld_double)
#> SUCCESS | arrange() | atanh | atanh(1/fld_double)
#> SUCCESS | mutate() | cos | cos(fld_double)
#> SUCCESS | filter() | cos | cos(fld_double) < 0.5
#> SUCCESS | summarise() | cos | sum(cos(fld_double), na.rm = TRUE)
#> SUCCESS | group_by() | cos | cos(fld_double)
#> SUCCESS | arrange() | cos | cos(fld_double)
#> SUCCESS | mutate() | cosh | cosh(fld_double)
#> SUCCESS | filter() | cosh | cosh(fld_double) < 10
#> SUCCESS | summarise() | cosh | sum(cosh(fld_double), na.rm = TRUE)
#> SUCCESS | group_by() | cosh | cosh(fld_double)
#> SUCCESS | arrange() | cosh | cosh(fld_double)
#> SUCCESS | mutate() | cot | cot(fld_double)
#> SUCCESS | filter() | cot | cot(fld_double) < 10
#> SUCCESS | summarise() | cot | sum(cot(fld_double), na.rm = TRUE)
#> SUCCESS | group_by() | cot | cot(fld_double)
#> SUCCESS | arrange() | cot | cot(fld_double)
#> SUCCESS | mutate() | coth | coth(fld_double - 1/(abs(fld_double) + 1))
#> SUCCESS | filter() | coth | coth(fld_double - 1/(abs(fld_double) + 1)) > 10
#> SUCCESS | summarise() | coth | sum(coth(fld_double - 1/(abs(fld_double) + 1)), na.rm = TRUE)
#> SUCCESS | group_by() | coth | coth(fld_double - 1/(abs(fld_double) + 1))
#> SUCCESS | arrange() | coth | coth(fld_double - 1/(abs(fld_double) + 1))
#> SUCCESS | mutate() | sin | sin(fld_double)
#> SUCCESS | filter() | sin | sin(fld_double) < 0.5
#> SUCCESS | summarise() | sin | sum(sin(fld_double), na.rm = TRUE)
#> SUCCESS | group_by() | sin | sin(fld_double)
#> SUCCESS | arrange() | sin | sin(fld_double)
#> SUCCESS | mutate() | sinh | sinh(fld_double)
#> SUCCESS | filter() | sinh | sinh(fld_double) < 10
#> SUCCESS | summarise() | sinh | sum(sinh(fld_double), na.rm = TRUE)
#> SUCCESS | group_by() | sinh | sinh(fld_double)
#> SUCCESS | arrange() | sinh | sinh(fld_double)
#> SUCCESS | mutate() | tan | tan(fld_double)
#> SUCCESS | filter() | tan | tan(fld_double) < 10
#> SUCCESS | summarise() | tan | sum(tan(fld_double), na.rm = TRUE)
#> SUCCESS | group_by() | tan | tan(fld_double)
#> SUCCESS | arrange() | tan | tan(fld_double)
#> SUCCESS | mutate() | tanh | tanh(fld_double)
#> SUCCESS | filter() | tanh | tanh(fld_double) < 0.5
#> SUCCESS | summarise() | tanh | sum(tanh(fld_double), na.rm = TRUE)
#> SUCCESS | group_by() | tanh | tanh(fld_double)
#> SUCCESS | arrange() | tanh | tanh(fld_double)
#> Warning: Outer names are only allowed for unnamed scalar atomic inputs
#> Succesful tests: 75
#> Test with errors: 0
#> Failed tests: 0
#> Total number of tests: 75
#> 
```
