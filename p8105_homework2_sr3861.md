P8105 Homework 2
================
Shritama Ray
10/5/2022

## Problem 1: NYC Transit Data

Read & Clean the Dataset

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
transit_df = read_csv("./Data/subway_data.csv") %>%
  janitor::clean_names() %>%
  select(line:entry, vending, ada)%>%
  mutate(entry = recode(entry, YES = TRUE, NO = FALSE)) 
```

    ## Rows: 1868 Columns: 32
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (22): Division, Line, Station Name, Route1, Route2, Route3, Route4, Rout...
    ## dbl  (8): Station Latitude, Station Longitude, Route8, Route9, Route10, Rout...
    ## lgl  (2): ADA, Free Crossover
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(transit_df, 10)
```

    ## # A tibble: 10 × 19
    ##    line     station_…¹ stati…² stati…³ route1 route2 route3 route4 route5 route6
    ##    <chr>    <chr>        <dbl>   <dbl> <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
    ##  1 4 Avenue 25th St       40.7   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  2 4 Avenue 25th St       40.7   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  3 4 Avenue 36th St       40.7   -74.0 N      R      <NA>   <NA>   <NA>   <NA>  
    ##  4 4 Avenue 36th St       40.7   -74.0 N      R      <NA>   <NA>   <NA>   <NA>  
    ##  5 4 Avenue 36th St       40.7   -74.0 N      R      <NA>   <NA>   <NA>   <NA>  
    ##  6 4 Avenue 45th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  7 4 Avenue 45th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  8 4 Avenue 45th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  9 4 Avenue 45th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ## 10 4 Avenue 53rd St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ## # … with 9 more variables: route7 <chr>, route8 <dbl>, route9 <dbl>,
    ## #   route10 <dbl>, route11 <dbl>, entrance_type <chr>, entry <lgl>,
    ## #   vending <chr>, ada <lgl>, and abbreviated variable names ¹​station_name,
    ## #   ²​station_latitude, ³​station_longitude

**Summary:** This dataset contains information on the NYC subway system.
Specifically, the dataset describes the routes and locations of the
stations, along with some other features such as ADA accessibility,
vending machines, and entrance type. So far I’ve manipulated the data to
create clean variable names, remove the variables I am not interested
in, and convert the entry values to logical operators. The resulting
dataset has **1868 rows** and **19 columns.** The data is **NOT tidy.**
The route variables are in a wide format, resulting in many NA values.

**Questions:**

1.  How many distinct stations are there?

``` r
nrow(distinct(transit_df, line, station_name))
```

    ## [1] 465

There are **465** distinct stations.

2.  How many stations are ADA compliant?

``` r
distinct(transit_df, line, station_name, ada) %>%
  filter(ada == TRUE) %>%
  nrow()
```

    ## [1] 84

**84** stations are ADA compliant

3.  What proportion of station entrances/exits without vending allow
    entrance?

``` r
filter(transit_df, vending == "NO" & entry == TRUE) %>%
  nrow()
```

    ## [1] 69

``` r
filter(transit_df, vending == "NO") %>%
  nrow()
```

    ## [1] 183

**69/183** of the entrances/exits without vending allow entry

Reformat the data so that route number and route name are distinct
variables:

``` r
  transit_tidy = 
  mutate_at(transit_df, c('route8','route9','route10','route11'), as.character) %>%
  pivot_longer(route1:route11, 
               names_to = "route_number", 
               names_prefix = "route", 
               values_to = "route_name")
```

How many distinct stations serve the A train?

``` r
nrow(
  filter(
    distinct(transit_tidy, line, station_name, route_name),route_name == "A"))
```

    ## [1] 60

**60** distinct stations serve the A train.

Of the stations, that serve the A train, how many are ADA complaint?

``` r
nrow(
  filter(
    distinct(transit_tidy, line, station_name, route_name, ada),
    route_name == "A" & ada == TRUE))
```

    ## [1] 17

**17** of the stations that serve the A train are ADA compliant.
