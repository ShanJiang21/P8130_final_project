Data\_manipulation
================
Shan Jiang
12/7/2018

``` r
library(readr)
library(tidyverse)
```

``` r
cancer_raw = readr::read_csv("./Data/Cancer_Registry.csv") %>% 
  janitor::clean_names() 

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
                   binwidth = 2, colour = "black", fill="white") +
    geom_density(alpha = .1, fill = "#FF6666") +
    geom_vline(aes(xintercept = mean(target_death_rate, na.rm = T)),   # Ignore NA values for mean
               color = "red", linetype = "dashed", size = 1)# Overlay with transparent density plot
```

![](data_manipulation_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

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
