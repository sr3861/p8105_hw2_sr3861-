P8105 Homework 2
================
Shritama Ray
10/5/2022

## Problem 1: NYC Transit Data

*My attempt to solve without the solutions*

Read & Clean the Dataset

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

## Problem 2: Mr. Trash Wheel

Read and clean the Mr. Trash Wheel dataset:

``` r
library(readxl)
mr_trash = read_excel("./Data/trash_wheel.xlsx", sheet = "Mr. Trash Wheel", range = "A2:N549") %>%
  janitor::clean_names() %>%
   filter(dumpster != '') %>%
   mutate(sports_balls = as.integer(sports_balls), year = as.integer(year))
```

Read and clean the Professor Trash Wheel sheet:

``` r
prof_trash = read_excel("./Data/trash_wheel.xlsx", sheet = "Professor Trash Wheel", range = "A2:M96") %>%
  janitor::clean_names() %>%
   filter(dumpster != '')
```

Combine the two datasets:

``` r
#First create a new variable to track each dataset
mr_trash = mutate(mr_trash, wheel = "Mr.")
prof_trash = mutate(prof_trash, wheel = "Prof")

#combine
combined_trash = bind_rows(mr_trash, prof_trash)
```

This combined dataset has a total of **641** rows and **15** columns.
Some of the key variables include the dumpster, month/year of
collection, weight in tons, \# of homes powered, and the count of types
of trash, such as grocery bags, cigarette butts, etc.

What was the total weight of trash collected by Professor Trash Wheel?

Professor Trash Wheel collected a total of **190.12** tons of trash.

What was the total number of sports balls collected by Mr. Trash Wheel
in 2020?

Mr. Trash Wheel collected a total of **856** sports balls in 2020.

## Problem 3: FiveThirtyEight

Read & Clean pols-month.csv:

``` r
pols_month = read_csv("./data/fivethirtyeight_datasets/pols-month.csv") %>%
  janitor::clean_names() %>%
  separate(mon, into = c("year", "month", "day")) %>%
  mutate(month = month.name[as.integer(month)],
         president = recode(prez_dem, '1' = "dem", '0' = "gop")) %>%
  select(-day, -prez_dem, -prez_gop)
```

    ## Rows: 822 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Read & clean snp.csv:

``` r
snp = read_csv("./data/fivethirtyeight_datasets/snp.csv") 
```

    ## Rows: 787 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#Convert year to 4 digits
four_year = as.Date(snp$date,"%m/%d/%y")
four_year = as.Date(ifelse(four_year > Sys.Date(), format(four_year, "19%y-%m-%d"), format(four_year)))
snp[,"date"] <- four_year

#Clean data
snp = 
  separate(snp, date, into = c("year", "month", "day"))%>%
  mutate(month = month.name[as.integer(month)]) %>%
  select(year, month, close)
```

Read & clean unemployment dataset:

``` r
unemployment = read_csv("./data/fivethirtyeight_datasets/unemployment.csv") %>%
  pivot_longer(Jan:Dec,
               names_to = "month",
               values_to = "percent") %>%
  mutate(year = as.character(Year), 
         month = month.name[match(pull(.,month),month.abb)]) %>%
  arrange(year, month)
```

    ## Rows: 68 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#Note: I removed the janitor::clean_names() statement since it made the months lower case
```

Merge the Datasets:

``` r
merged_538 = left_join(pols_month, snp, by = c("year","month")) %>%
              left_join(unemployment, by = c("year","month"))
```

**Summary:** These datasets contain various information about politics
and economics. The pols_month dataset contains info on national
politicians and their party at various points of time. The snp dataset
contains the closing values of the S&P stock market index at various
points of time. The unemployment dataset has the monthly national
unemployment rate over several years. The merged dataset contains
**822** rows and **12** columns. The data contains information over the
years **1947** to **2015.** It contains information on the number of
politicians from each party, the S&P closing value, and unemployment
rate at each month over this period. This dataset can be used to analyze
associations between political landscapes and economic success.
