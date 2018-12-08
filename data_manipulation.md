Data\_manipulation
================
Shan Jiang, Jieqi Tu
12/7/2018

``` r
library(readr)
library(tidyverse)
```

``` r
# Import data
cancer_raw = readr::read_csv("./Data/Cancer_Registry.csv") %>% 
  janitor::clean_names() 

dim(cancer_raw)
```

    ## [1] 3047   34

``` r
head(cancer_raw)
```

    ## # A tibble: 6 x 34
    ##   avg_ann_count avg_deaths_per_… target_death_ra… incidence_rate med_income
    ##           <dbl>            <dbl>            <dbl>          <dbl>      <dbl>
    ## 1          1397              469             165.           490.      61898
    ## 2           173               70             161.           412.      48127
    ## 3           102               50             175.           350.      49348
    ## 4           427              202             195.           430.      44243
    ## 5            57               26             144.           350.      49955
    ## 6           428              152             176            505.      52313
    ## # ... with 29 more variables: pop_est2015 <dbl>, poverty_percent <dbl>,
    ## #   study_per_cap <dbl>, binned_inc <chr>, median_age <dbl>,
    ## #   median_age_male <dbl>, median_age_female <dbl>, geography <chr>,
    ## #   avg_household_size <dbl>, percent_married <dbl>, pct_no_hs18_24 <dbl>,
    ## #   pct_hs18_24 <dbl>, pct_some_col18_24 <dbl>, pct_bach_deg18_24 <dbl>,
    ## #   pct_hs25_over <dbl>, pct_bach_deg25_over <dbl>,
    ## #   pct_employed16_over <dbl>, pct_unemployed16_over <dbl>,
    ## #   pct_private_coverage <dbl>, pct_private_coverage_alone <dbl>,
    ## #   pct_emp_priv_coverage <dbl>, pct_public_coverage <dbl>,
    ## #   pct_public_coverage_alone <dbl>, pct_white <dbl>, pct_black <dbl>,
    ## #   pct_asian <dbl>, pct_other_race <dbl>, pct_married_households <dbl>,
    ## #   birth_rate <dbl>

``` r
# Check NA values for each column
n_NA = sapply(cancer_raw[1:34], function(x) sum(length(which(is.na(x)))))
n_NA
```

    ##              avg_ann_count        avg_deaths_per_year 
    ##                          0                          0 
    ##          target_death_rate             incidence_rate 
    ##                          0                          0 
    ##                 med_income                pop_est2015 
    ##                          0                          0 
    ##            poverty_percent              study_per_cap 
    ##                          0                          0 
    ##                 binned_inc                 median_age 
    ##                          0                          0 
    ##            median_age_male          median_age_female 
    ##                          0                          0 
    ##                  geography         avg_household_size 
    ##                          0                          0 
    ##            percent_married             pct_no_hs18_24 
    ##                          0                          0 
    ##                pct_hs18_24          pct_some_col18_24 
    ##                          0                       2285 
    ##          pct_bach_deg18_24              pct_hs25_over 
    ##                          0                          0 
    ##        pct_bach_deg25_over        pct_employed16_over 
    ##                          0                        152 
    ##      pct_unemployed16_over       pct_private_coverage 
    ##                          0                          0 
    ## pct_private_coverage_alone      pct_emp_priv_coverage 
    ##                        609                          0 
    ##        pct_public_coverage  pct_public_coverage_alone 
    ##                          0                          0 
    ##                  pct_white                  pct_black 
    ##                          0                          0 
    ##                  pct_asian             pct_other_race 
    ##                          0                          0 
    ##     pct_married_households                 birth_rate 
    ##                          0                          0

``` r
# Check the percentage of NA values for each column
percentage_NA = sapply(cancer_raw[1:34], function(x) sum(length(which(is.na(x)))) / 3047)
percentage_NA
```

    ##              avg_ann_count        avg_deaths_per_year 
    ##                 0.00000000                 0.00000000 
    ##          target_death_rate             incidence_rate 
    ##                 0.00000000                 0.00000000 
    ##                 med_income                pop_est2015 
    ##                 0.00000000                 0.00000000 
    ##            poverty_percent              study_per_cap 
    ##                 0.00000000                 0.00000000 
    ##                 binned_inc                 median_age 
    ##                 0.00000000                 0.00000000 
    ##            median_age_male          median_age_female 
    ##                 0.00000000                 0.00000000 
    ##                  geography         avg_household_size 
    ##                 0.00000000                 0.00000000 
    ##            percent_married             pct_no_hs18_24 
    ##                 0.00000000                 0.00000000 
    ##                pct_hs18_24          pct_some_col18_24 
    ##                 0.00000000                 0.74991795 
    ##          pct_bach_deg18_24              pct_hs25_over 
    ##                 0.00000000                 0.00000000 
    ##        pct_bach_deg25_over        pct_employed16_over 
    ##                 0.00000000                 0.04988513 
    ##      pct_unemployed16_over       pct_private_coverage 
    ##                 0.00000000                 0.00000000 
    ## pct_private_coverage_alone      pct_emp_priv_coverage 
    ##                 0.19986872                 0.00000000 
    ##        pct_public_coverage  pct_public_coverage_alone 
    ##                 0.00000000                 0.00000000 
    ##                  pct_white                  pct_black 
    ##                 0.00000000                 0.00000000 
    ##                  pct_asian             pct_other_race 
    ##                 0.00000000                 0.00000000 
    ##     pct_married_households                 birth_rate 
    ##                 0.00000000                 0.00000000

There are in total 34 variables and 3047observations in the dataset.

  - The outcome variable in this datset is `target_death
    rate`(Continuous VARIABLE);

  - There are remaining 33 predictors avaliable for use.

### Here are descriptive statistics for `target_death rate`

we first need to look at the distributions of our outcome variable.

``` r
cancer_raw %>% 
  ggplot(aes(x = target_death_rate)) + 
    geom_histogram(aes(y = ..density..),  
                   binwidth = 2, colour = "black", fill = "white") +
    geom_density(alpha = .1, fill = "#FF6666") +
    geom_vline(aes(xintercept = mean(target_death_rate, na.rm = T)),   # Ignore NA values for mean
               color = "red", linetype = "dashed", size = 1)# Overlay with transparent density plot
```

![](data_manipulation_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Data cleaning

``` r
# Discard columns that have more than 10% NA values 
cancer_raw = 
  cancer_raw %>% 
  select(-pct_private_coverage, -pct_some_col18_24)

# Create a new dataset for type (a) variables
cancer_a_raw = 
  cancer_raw %>% 
  select(target_death_rate, avg_ann_count, avg_deaths_per_year, incidence_rate, study_per_cap)

# Create a new dataset for type (b) variables
cancer_b_raw = 
  cancer_raw %>% 
  select(-target_death_rate, -avg_ann_count, -avg_deaths_per_year, -incidence_rate, -study_per_cap)
```

``` r
cancer_b_raw =
  cancer_b_raw %>% 
  # select median age as the only predictor to reflect the population age level
  select(-median_age_male, -median_age_female) %>% 
  # select median income as the only predictor to relect the population income level
  select(-binned_inc) %>% 
  # select percentage of residents employed as the only predictor to relect the population employment level
  select(-pct_unemployed16_over) %>% 
  # select percent of county residents with private health coverage as the only predictor to relect the population health coverage level
  select(-pct_private_coverage_alone) %>% 
  # select percentage of whits as the only predictor to relect the race proportion
  select(-pct_black, -pct_asian)
```

### Here are descriptive statistics for predictors

### Variable selection

We are only interested in working with a subset of the most relevant
variables.

  - For combination of *race*:we combined the `pct_white`, `pct_black`,
    `pct_asian`, `pct_other_race`.

<!-- end list -->

``` r
## categorical variable 


##
```

## Multiple Linear Regression
