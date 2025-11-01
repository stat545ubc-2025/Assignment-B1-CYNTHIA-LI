Assignment B1
================
Cynthia
2025-10-24

## Exercise 1: Make a Function (25 points)

In this exercise, you’ll be making a function and fortifying it.

``` r
#Load appropriate packages
library(roxygen2)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(datateachr)
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

``` r
#' @title Summarise a numeric column by group
#'
#' @description This function takes a data frame and summarises a numeric column
#' (ex. income, score) by a chosen grouping variable (ex. region, category). 
#' It returns the mean and median of the numeric variable for each group.
#'
#' @param data A data frame containing the variables of interest.
#' @param group_col The column used to group observations (e.g., decade or region).
#' @param value_col The numeric column to summarise (e.g. income, score).
#'
#' @return A tibble containing one row per group with two summary statistics:
#' the mean and median of the selected numeric column. This function will automatically remove the NAs from your data and thus your resulting tibble will not contain NAs


summarise_by_group <- function(data, group_col, value_col) {
  
  #Show error if volue_col is not numeric
  if(!is.numeric(pull(data,{{value_col}}))){
    stop("The value_col column must be numeric")
  }
  
  data %>%
    group_by({{ group_col }}) %>%
    summarise(
      mean = mean({{ value_col }}, na.rm = TRUE),
      median = median({{ value_col }}, na.rm = TRUE)
    )
}
```

## Exercise 2: Document your Function (20 points)

In the same code chunk where you made your function, document the
function using roxygen2 tags. Be sure to include:

1.  Title.
2.  Function description: In 1-2 brief sentences, describe what the
    function does.
3.  Document each argument with the @param tag, making sure to justify
    why you named the parameter as you did. • (Justification for naming
    is not often needed, but we want to hear your reasoning.)
4.  What the function returns, using the @return tag.

## Exercise 3: Include examples (15 points)

Demonstrate the usage of your function with a few examples. Use one or
more new code chunks, describing what you’re doing.

### Example 1: Applying the function to the flow_sample dataset

Here we are using our function to group by year, and then obtain the
mean and median flow values for each year

``` r
head(flow_sample)
```

    ## # A tibble: 6 × 7
    ##   station_id  year extreme_type month   day  flow sym  
    ##   <chr>      <dbl> <chr>        <dbl> <dbl> <dbl> <chr>
    ## 1 05BB001     1909 maximum          7     7   314 <NA> 
    ## 2 05BB001     1910 maximum          6    12   230 <NA> 
    ## 3 05BB001     1911 maximum          6    14   264 <NA> 
    ## 4 05BB001     1912 maximum          8    25   174 <NA> 
    ## 5 05BB001     1913 maximum          6    11   232 <NA> 
    ## 6 05BB001     1914 maximum          6    18   214 <NA>

``` r
summarise_by_group(flow_sample, year, flow)
```

    ## # A tibble: 109 × 3
    ##     year  mean median
    ##    <dbl> <dbl>  <dbl>
    ##  1  1909 314    314  
    ##  2  1910 230    230  
    ##  3  1911 135.   135. 
    ##  4  1912  89.9   89.9
    ##  5  1913 119.   119. 
    ##  6  1914 111.   111. 
    ##  7  1915 121.   121. 
    ##  8  1916 158.   158. 
    ##  9  1917  90.0   90.0
    ## 10  1918 176.   176. 
    ## # ℹ 99 more rows

### Example 2: Applying the function to the mtcars dataset

Here we are grouping by gear number, and then calculating the mean and
median weight for each group

``` r
head(mtcars)
```

    ##                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
    ## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
    ## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
    ## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
    ## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
    ## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
    ## Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

``` r
summarise_by_group(mtcars, gear, wt)
```

    ## # A tibble: 3 × 3
    ##    gear  mean median
    ##   <dbl> <dbl>  <dbl>
    ## 1     3  3.89   3.73
    ## 2     4  2.62   2.7 
    ## 3     5  2.63   2.77

## Exercise 4: Test the Function (25 points)

Running examples is a good way of checking by-eye whether your function
is working as expected. But, having a formal “yes or no” check is useful
when you move on to other parts of your analysis. Write formal tests for
your function. You should use at least three non-redundant uses of an
expect\_() function from the testthat package, and they should be
contained in a test_that() function (or more than one). They should all
pass.

### Creating tibbles for testing

``` r
#Tibble for Test 1
my_data1 <- tibble(
  group = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
  category = c("X", "Y", "Z", "X", "Z", "Y", "Y", "Z", "X"),
  values = c(9, 0, 0, 12, 89, 56, 34, 78, 90)
)

my_data1
```

    ## # A tibble: 9 × 3
    ##   group category values
    ##   <chr> <chr>     <dbl>
    ## 1 A     X             9
    ## 2 A     Y             0
    ## 3 A     Z             0
    ## 4 B     X            12
    ## 5 B     Z            89
    ## 6 B     Y            56
    ## 7 C     Y            34
    ## 8 C     Z            78
    ## 9 C     X            90

``` r
#Tibble for Test 2
my_data2 <- tibble(
  group = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
  category = c("X", "Y", "Z", "X", "Z", "Y", "Y", "Z", "X"),
  values = c(NA, NA, NA, 12, 89, 56, 34, 78, 90)
)

my_data2
```

    ## # A tibble: 9 × 3
    ##   group category values
    ##   <chr> <chr>     <dbl>
    ## 1 A     X            NA
    ## 2 A     Y            NA
    ## 3 A     Z            NA
    ## 4 B     X            12
    ## 5 B     Z            89
    ## 6 B     Y            56
    ## 7 C     Y            34
    ## 8 C     Z            78
    ## 9 C     X            90

``` r
#Tibble for Test 3
my_data3 <- tibble(
  group = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
  category = c("X", "Y", "Z", "X", "Z", "Y", "Y", "Z", "X"),
  values = c(2, -4, -1, 12, 89, 56, 34, 78, 90)
)

my_data3
```

    ## # A tibble: 9 × 3
    ##   group category values
    ##   <chr> <chr>     <dbl>
    ## 1 A     X             2
    ## 2 A     Y            -4
    ## 3 A     Z            -1
    ## 4 B     X            12
    ## 5 B     Z            89
    ## 6 B     Y            56
    ## 7 C     Y            34
    ## 8 C     Z            78
    ## 9 C     X            90

### Test \#1: Handles values of zero correctly

``` r
test_that("Function correctly incorporates 0 values when summarising", {
  result <- summarise_by_group(my_data1, group, values)
  
  #Manually calculating the value, we should get 3 for mean and 0 for median
  
  #Expect manually calculated value in the result
  expect_equal(result %>% filter(group == "A") %>% pull(mean), 3) #this allows us to filter for only group A's mean value
  expect_equal(result %>% filter(group == "A") %>% pull(median), 0) #this allows us to filter for only group A's median value
 
})
```

    ## Test passed 🎊

### Test \#2: Handles NA values correctly

``` r
test_that("Function correctly incorporates NA values when summarising", {
  result <- summarise_by_group(my_data2, group, values)
  
  #Since all values for group A are NA, we should get NA for mean and median
  
  #Expect manually calculated value in the result
  expect_true(is.na(result %>% filter(group == "A") %>% pull(mean))) #this allows us to filter for only group A's mean value
  expect_true(is.na(result %>% filter(group == "A") %>% pull(median))) #this allows us to filter for only group A's median value
 
})
```

    ## Test passed 🎉

### Test \#3: Handles negative values correctly

``` r
test_that("Function correctly incorporates negative values when summarising", {
  result <- summarise_by_group(my_data3, group, values)
  
  #Manually calculating the value, we should get -1 for mean and -1 for median
  
  #Expect manually calculated value in the result
  expect_equal(result %>% filter(group == "A") %>% pull(mean), -1)
  expect_equal(result %>% filter(group == "A") %>% pull(median), -1)
 
})
```

    ## Test passed 😀
