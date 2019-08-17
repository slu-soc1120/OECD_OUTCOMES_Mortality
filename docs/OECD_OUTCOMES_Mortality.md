OECD Life Expectancy and Infant Mortality
================
Christopher Prener, Ph.D.
(August 17, 2019)

## Introduction

This notebook creates the plots for Lecture-01 - Course Introduction for
my sections of SOC 1120.

## Packages

This notebook requires the following packages:

``` r
# tidyverse packages
library(dplyr)          # data wrangling
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)        # create plots
```

    ## Registered S3 methods overwritten by 'ggplot2':
    ##   method         from 
    ##   [.quosures     rlang
    ##   c.quosures     rlang
    ##   print.quosures rlang

``` r
library(readr)          # tabular files

# plotting packages
library(RColorBrewer)   # color schemes
library(prener)         # export tools

# other packages
library(countrycode)    # convert country names
library(here)           # manage file paths
```

    ## here() starts at /Users/prenercg/GitHub/slu-soc1120/OECD_OUTCOMES_Mortality

## Load Data

All of the raw data for these slides were obtained from the OECD’s open
data website:

  - [GDP Per
    Capita, 2018](https://data.oecd.org/gdp/gross-domestic-product-gdp.htm)
  - [Total Health Expenditure by
    Country, 2018](https://data.oecd.org/healthres/health-spending.htm)
  - [Life Expectancy at
    Birth, 2018](https://data.oecd.org/healthstat/life-expectancy-at-birth.htm)
  - [Infant
    Mortality, 2018](https://data.oecd.org/healthstat/infant-mortality-rates.htm)

They are in separate `.csv` files that we’ll load individually:

``` r
spending <- read_csv(here("data", "DP_LIVE_11082019203859613.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   LOCATION = col_character(),
    ##   INDICATOR = col_character(),
    ##   SUBJECT = col_character(),
    ##   MEASURE = col_character(),
    ##   FREQUENCY = col_character(),
    ##   TIME = col_double(),
    ##   Value = col_double(),
    ##   `Flag Codes` = col_character()
    ## )

``` r
lifexp <- read_csv(here("data", "DP_LIVE_11082019205910721.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   LOCATION = col_character(),
    ##   INDICATOR = col_character(),
    ##   SUBJECT = col_character(),
    ##   MEASURE = col_character(),
    ##   FREQUENCY = col_character(),
    ##   TIME = col_double(),
    ##   Value = col_double(),
    ##   `Flag Codes` = col_character()
    ## )

``` r
infant <- read_csv(here("data", "DP_LIVE_11082019205945830.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   LOCATION = col_character(),
    ##   INDICATOR = col_character(),
    ##   SUBJECT = col_character(),
    ##   MEASURE = col_character(),
    ##   FREQUENCY = col_character(),
    ##   TIME = col_double(),
    ##   Value = col_double(),
    ##   `Flag Codes` = col_character()
    ## )

``` r
gdp <- read_csv(here("data", "DP_LIVE_11082019213358891.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   LOCATION = col_character(),
    ##   INDICATOR = col_character(),
    ##   SUBJECT = col_character(),
    ##   MEASURE = col_character(),
    ##   FREQUENCY = col_character(),
    ##   TIME = col_double(),
    ##   Value = col_double(),
    ##   `Flag Codes` = col_character()
    ## )

## Clean Data

First, we’ll clean the health care spending data by obtaining total
spending and retaining only the value for the latest year available:

``` r
spending <- spending %>%
  filter(SUBJECT == "TOT") %>%
  filter(TIME == 2017 | TIME == 2018) %>%
  select(LOCATION, TIME, Value) %>%
  group_by(LOCATION) %>%
  summarise(spend = last(Value), spend_year = last(TIME))
```

Similarly, we’ll take the GDP value for the latest year available:

``` r
gdp <- gdp %>%
  filter(TIME == 2017 | TIME == 2018) %>%
  select(LOCATION, TIME, Value) %>%
  group_by(LOCATION) %>%
  summarise(gdp = last(Value), gdp_year = last(TIME)) %>%
  filter(LOCATION != "OECD")
```

For infant mortality, some countries do not have data for 2017 or 2018
so we need to go back further to get the latest year available:

``` r
infant <- infant %>%
  filter(TIME == 2015 | TIME == 2016 | TIME == 2017 | TIME == 2018) %>%
  select(LOCATION, TIME, Value) %>%
  group_by(LOCATION) %>%
  summarise(infant = last(Value), infant_year = last(TIME))
```

Finally, we’ll get the latest available data for life expectancy at
birth. We only want total life expectancy, which pools men and women
together. As with the other tables, we’ll keep it only for the latest
year available:

``` r
lifexp <- lifexp %>%
  filter(SUBJECT == "TOT") %>%
  filter(TIME == 2016 | TIME == 2017 | TIME == 2018) %>%
  select(LOCATION, TIME, Value) %>%
  group_by(LOCATION) %>%
  summarise(lifexp = last(Value), lifexp_year = last(TIME))
```

## Combine Tables

Next, we’ll combine all four of our raw data tables into a single
object:

``` r
oecd <- left_join(gdp, spending, by = "LOCATION") %>%
  left_join(., lifexp, by = "LOCATION") %>%
  left_join(., infant, by = "LOCATION") %>%
  rename(loc = LOCATION)

# clean-up environment
rm(gdp, spending, lifexp, infant)
```

Finally, we’ll add full country names:

``` r
oecd <- oecd %>%
  mutate(name = countrycode(loc, origin = "iso3c", destination = "country.name")) %>%
  select(loc, name, everything())
```

### GDP and Health Expenditure

First, we’ll compare GDP and health care spending:

``` r
# create plot
plot <- ggplot(oecd, aes(x = gdp, y = spend, label = loc)) +
          geom_smooth(aes(color = "lm"), method = "lm", se = FALSE) +
          geom_point() +
          geom_label(aes(fill = loc == "USA"), size = 8) +
          scale_y_continuous(labels = scales::dollar) +
          scale_x_continuous(breaks = scales::pretty_breaks(n=5), labels = scales::dollar) +
          labs(
            title = "Health Care Spending and GDP",
            subtitle = "OECD Countries",
            caption = "Data from 2017 or 2018 depending on the country; data via OECD",
            x = "Gross Domestic Product per Capita (USD)",
            y = "Health Care Spending per Capita (USD)"
          ) +
          scale_fill_brewer(palette = "Set2") +
          cp_sequoiaTheme(background = "transparent") +
          theme(legend.position = "none")

# save plot
cp_plotSave(here("results", "gdp_spend.png"), plot, preset = "lg")
```

### Health Care Spending and Life Expectancy

Next, we’ll compare health care spending to life expectancy at birth:

``` r
# create plot
plot <- ggplot(oecd, aes(x = lifexp, y = spend, label = loc)) +
              geom_smooth(aes(color = "lm"), method = "lm", se = FALSE) +
              geom_point() +
              geom_label(aes(fill = loc == "USA"), size = 8) +
              scale_y_continuous(labels = scales::dollar) +
              scale_x_continuous(breaks = scales::pretty_breaks(n=8)) +
              labs(
                title = "Health Care Spending and Life Expectancy",
                subtitle = "OECD Countries",
                caption = "Data from 2016-2018 depending on the country; data via OECD",
                x = "Life Expectancy at Birth (years)",
                y = "Health Care Spending per Capita (USD)"
              ) +
              scale_fill_brewer(palette = "Set2") + 
              cp_sequoiaTheme(background = "transparent") +
              theme(legend.position = "")

# save plot
cp_plotSave(here("results", "spend_life.png"), plot, preset = "lg")
```

### Health Care Spending and Infant Mortality

Next, we’ll compare health care spending to infant mortality rates:

``` r
# create plot
plot <- ggplot(oecd, aes(x = infant, y = spend, label = loc)) +
              geom_smooth(aes(color = "lm"), method = "lm", se = FALSE) +
              geom_point() +
              geom_label(aes(fill = loc == "USA"), size = 8) +
              scale_y_continuous(labels = scales::dollar) +
              scale_x_continuous(breaks = scales::pretty_breaks(n=5)) +
              labs(
                title = "Health Care Spending and Infant Mortality",
                subtitle = "OECD Countries",
                caption = "Data from 2015-2018 depending on the country; data via OECD",
                x = "Deaths per 1,000 Live Births",
                y = "Health Care Spending per Capita (USD)"
              ) +
              scale_fill_brewer(palette = "Set2") +
              cp_sequoiaTheme(background = "transparent") +
              theme(legend.position = "")

# save plot
cp_plotSave(here("results", "spend_infant.png"), plot, preset = "lg")
```

### Infant Mortality

Finally, we’ll generate a bar plot of countries based on their infant
mortality rates:

``` r
# create plot
plot <- ggplot(oecd, aes(x = reorder(name, infant), y = infant)) +
              geom_bar(stat='identity', aes(fill = loc == "USA")) +
              coord_flip() +
              labs(
                title = "Infant Mortality",
                subtitle = "OECD Countries",
                caption = "Data from 2015-2018 depending on the country; data via OECD",
                x = "",
                y = "Deaths per 1,000 Live Births"
              ) +
              scale_fill_brewer(palette = "Set2") +
              cp_sequoiaTheme(background = "transparent") +
              theme(
                legend.position = "",
                axis.text.y = element_text(size = 18)
              )

# save plot
cp_plotSave(here("results", "infant_bar.png"), plot, preset = "lg")
```
