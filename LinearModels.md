Linear Models
================
Jieqi Tu (jt3098)
12/11/2018

## Import and tidy data

``` r
# Import data
cancer_raw = readr::read_csv("./Data/Cancer_Registry.csv") %>% 
  janitor::clean_names() 
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   binnedInc = col_character(),
    ##   Geography = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
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
percentage_NA %>% data.frame()
```

    ##                                     .
    ## avg_ann_count              0.00000000
    ## avg_deaths_per_year        0.00000000
    ## target_death_rate          0.00000000
    ## incidence_rate             0.00000000
    ## med_income                 0.00000000
    ## pop_est2015                0.00000000
    ## poverty_percent            0.00000000
    ## study_per_cap              0.00000000
    ## binned_inc                 0.00000000
    ## median_age                 0.00000000
    ## median_age_male            0.00000000
    ## median_age_female          0.00000000
    ## geography                  0.00000000
    ## avg_household_size         0.00000000
    ## percent_married            0.00000000
    ## pct_no_hs18_24             0.00000000
    ## pct_hs18_24                0.00000000
    ## pct_some_col18_24          0.74991795
    ## pct_bach_deg18_24          0.00000000
    ## pct_hs25_over              0.00000000
    ## pct_bach_deg25_over        0.00000000
    ## pct_employed16_over        0.04988513
    ## pct_unemployed16_over      0.00000000
    ## pct_private_coverage       0.00000000
    ## pct_private_coverage_alone 0.19986872
    ## pct_emp_priv_coverage      0.00000000
    ## pct_public_coverage        0.00000000
    ## pct_public_coverage_alone  0.00000000
    ## pct_white                  0.00000000
    ## pct_black                  0.00000000
    ## pct_asian                  0.00000000
    ## pct_other_race             0.00000000
    ## pct_married_households     0.00000000
    ## birth_rate                 0.00000000

``` r
#Pulling quartiles for study_per_cap categorical manipulation
study.quart <- with(cancer_raw, study_per_cap[study_per_cap > 0]) %>%
  quantile(., probs = c(0.25, 0.5, 0.75))

#Variable Manipulation
cancer.df <- cancer_raw %>%    #Remove Rows with > 20% missing 
  dplyr::select(-pct_some_col18_24) %>%  #Remove for too many missing
  mutate(
    pct_non_white = pct_black + pct_asian + pct_other_race, #Creating white, non-white percentages variables
    state = str_split_fixed(geography, ", ", 2)[ ,2] %>% as.factor(), #pulling state variable and casting as factor, possible region?
    binned_inc_lb = str_split_fixed(binned_inc, ", ", 2)[ ,1] %>% parse_number(), #pulling numeric lower bound
    binned_inc_ub = str_split_fixed(binned_inc, ", ", 2)[ ,2] %>% parse_number(), #pulling numeric upper bound
    binned_inc_point = (binned_inc_lb + binned_inc_ub)/2, #computing point estimate from ub,lb (intervals symmetric)
    study_quantile = ifelse(study_per_cap == 0, "None", 
                           ifelse(study_per_cap > 0 & study_per_cap <= study.quart[1], "Low", 
                                  ifelse(study_per_cap > study.quart[1] & study_per_cap <= study.quart[2], "Moderate", 
                                         ifelse(study_per_cap > study.quart[2] & study_per_cap <= study.quart[3], "High", 
                                                "Very High")))),
    study_quantile = as.factor(study_quantile) %>% fct_relevel(., "None", "Low", "Moderate", "High", "Very High"),
    avg_deaths_yr_pop = avg_deaths_per_year/pop_est2015,  #incorporate two vars into one (multicollinearity)
    avg_ann_count_pop = avg_ann_count/pop_est2015 #incorporate two vars into one (multicollinearity)
  ) %>%
  dplyr::select(-c(binned_inc, geography, study_per_cap))

# Variable Manipulation: Add region classification 
table(cancer.df$state)  
```

    ## 
    ##              Alabama               Alaska              Arizona 
    ##                   63                   18                   15 
    ##             Arkansas           California             Colorado 
    ##                   75                   57                   60 
    ##          Connecticut             Delaware District of Columbia 
    ##                    8                    3                    1 
    ##              Florida              Georgia               Hawaii 
    ##                   66                  155                    4 
    ##                Idaho             Illinois              Indiana 
    ##                   42                  102                   92 
    ##                 Iowa               Kansas             Kentucky 
    ##                   99                  102                  120 
    ##            Louisiana                Maine             Maryland 
    ##                   64                   16                   24 
    ##        Massachusetts             Michigan            Minnesota 
    ##                   14                   83                   87 
    ##          Mississippi             Missouri              Montana 
    ##                   82                  115                   48 
    ##             Nebraska               Nevada        New Hampshire 
    ##                   80                   17                   10 
    ##           New Jersey           New Mexico             New York 
    ##                   21                   32                   62 
    ##       North Carolina         North Dakota                 Ohio 
    ##                   99                   51                   87 
    ##             Oklahoma               Oregon         Pennsylvania 
    ##                   77                   36                   67 
    ##         Rhode Island       South Carolina         South Dakota 
    ##                    5                   46                   59 
    ##            Tennessee                Texas                 Utah 
    ##                   95                  233                   27 
    ##              Vermont             Virginia           Washington 
    ##                   14                  125                   39 
    ##        West Virginia            Wisconsin              Wyoming 
    ##                   55                   72                   23

``` r
# create a dataframe containing all the 4-level states names
NE.name <- c("Connecticut","Maine","Massachusetts","New Hampshire",
             "Rhode Island","Vermont","New Jersey","New York",
             "Pennsylvania")
MW.name <- c("Indiana","Illinois","Michigan","Ohio","Wisconsin",
             "Iowa","Kansas","Minnesota","Missouri","Nebraska",
             "North Dakota","South Dakota")
S.name <- c("Delaware","District of Columbia","Florida","Georgia",
            "Maryland","North Carolina","South Carolina","Virginia",
            "West Virginia","Alabama","Kentucky","Mississippi",
            "Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
W.name <- c("Arizona","Colorado","Idaho","New Mexico","Montana",
            "Utah","Nevada","Wyoming","Alaska","California",
            "Hawaii","Oregon","Washington")

region.list <- list(
  Northeast = NE.name,
  Midwest = MW.name,
  South = S.name,
  West = W.name)

#create new region variable identical to 'region'
cancer.df$region <- sapply(cancer.df$state, 
                 function(x) names(region.list)[grep(x,region.list)])     
#relevelnumber of countries by region (original variable) 

cancer.clean = cancer.df %>% as.data.frame()

cancer.clean = 
  cancer.clean %>% 
  select(pct_white, pct_hs25_over, pct_employed16_over, pct_private_coverage, pct_public_coverage, target_death_rate, avg_ann_count_pop, avg_household_size, avg_deaths_yr_pop, med_income, median_age_male, median_age_female, region, study_quantile) %>% 
  mutate(median_age_all_gender = (median_age_male + median_age_female)/2) %>% 
  mutate(south = ifelse(region == "South", 1, 0)) %>% 
  select(-median_age_female, -median_age_male, -region) %>% 
  mutate(study_quantile = as.factor(study_quantile))
# Select variables that we are interested in
```

## Variable Selection

Automatic procedures: (1) Backward:

``` r
mult.fit = lm(target_death_rate ~ ., data = cancer.clean)
summary(mult.fit)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ ., data = cancer.clean)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -66.095  -8.386   0.111   8.327  87.977 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              2.355e+02  7.125e+00  33.058  < 2e-16 ***
    ## pct_white               -7.243e-02  2.085e-02  -3.473 0.000521 ***
    ## pct_hs25_over            6.885e-01  4.930e-02  13.963  < 2e-16 ***
    ## pct_employed16_over     -5.342e-01  5.820e-02  -9.180  < 2e-16 ***
    ## pct_private_coverage    -3.801e-01  5.471e-02  -6.948 4.56e-12 ***
    ## pct_public_coverage     -2.395e-01  9.012e-02  -2.658 0.007904 ** 
    ## avg_ann_count_pop       -1.658e+01  2.721e+00  -6.094 1.25e-09 ***
    ## avg_household_size      -1.466e-02  7.056e-01  -0.021 0.983430    
    ## avg_deaths_yr_pop        5.126e+04  8.110e+02  63.206  < 2e-16 ***
    ## med_income               7.122e-04  4.350e-05  16.373  < 2e-16 ***
    ## study_quantileLow        3.335e+00  9.795e-01   3.405 0.000671 ***
    ## study_quantileModerate   3.017e+00  9.812e-01   3.075 0.002128 ** 
    ## study_quantileHigh       2.612e+00  9.994e-01   2.613 0.009015 ** 
    ## study_quantileVery High  3.826e+00  9.952e-01   3.844 0.000123 ***
    ## median_age_all_gender   -4.108e+00  9.141e-02 -44.939  < 2e-16 ***
    ## south                    7.972e+00  6.734e-01  11.838  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 14.34 on 2879 degrees of freedom
    ##   (152 observations deleted due to missingness)
    ## Multiple R-squared:  0.7288, Adjusted R-squared:  0.7274 
    ## F-statistic: 515.9 on 15 and 2879 DF,  p-value: < 2.2e-16

``` r
# Remove the variable with the highest p-value: avg_household_size
step1 = update(mult.fit, . ~ . -avg_household_size)
summary(step1)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ pct_white + pct_hs25_over + 
    ##     pct_employed16_over + pct_private_coverage + pct_public_coverage + 
    ##     avg_ann_count_pop + avg_deaths_yr_pop + med_income + study_quantile + 
    ##     median_age_all_gender + south, data = cancer.clean)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -66.095  -8.385   0.111   8.328  87.967 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              2.355e+02  6.501e+00  36.224  < 2e-16 ***
    ## pct_white               -7.244e-02  2.084e-02  -3.475 0.000518 ***
    ## pct_hs25_over            6.884e-01  4.921e-02  13.990  < 2e-16 ***
    ## pct_employed16_over     -5.342e-01  5.809e-02  -9.196  < 2e-16 ***
    ## pct_private_coverage    -3.799e-01  5.359e-02  -7.089 1.69e-12 ***
    ## pct_public_coverage     -2.395e-01  9.004e-02  -2.660 0.007869 ** 
    ## avg_ann_count_pop       -1.658e+01  2.720e+00  -6.095 1.24e-09 ***
    ## avg_deaths_yr_pop        5.126e+04  8.080e+02  63.441  < 2e-16 ***
    ## med_income               7.121e-04  4.274e-05  16.660  < 2e-16 ***
    ## study_quantileLow        3.335e+00  9.793e-01   3.405 0.000670 ***
    ## study_quantileModerate   3.017e+00  9.810e-01   3.075 0.002124 ** 
    ## study_quantileHigh       2.612e+00  9.989e-01   2.615 0.008971 ** 
    ## study_quantileVery High  3.827e+00  9.948e-01   3.847 0.000122 ***
    ## median_age_all_gender   -4.108e+00  9.053e-02 -45.376  < 2e-16 ***
    ## south                    7.972e+00  6.732e-01  11.842  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 14.34 on 2880 degrees of freedom
    ##   (152 observations deleted due to missingness)
    ## Multiple R-squared:  0.7288, Adjusted R-squared:  0.7275 
    ## F-statistic: 552.9 on 14 and 2880 DF,  p-value: < 2.2e-16

``` r
# All variables are significant with p-value less than 0.05

# store the result of backward procedure
model_backward = step1
```

2)  Forward:

<!-- end list -->

``` r
# check p-value for each predictor in SLR model
fit1 = lm(target_death_rate ~ pct_white, data = cancer.clean)
tidy(fit1)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)  204.       2.58       79.1  0.      
    ## 2 pct_white     -0.301    0.0302     -9.95 5.80e-23

``` r
fit2 = lm(target_death_rate ~ pct_hs25_over, data = cancer.clean)
tidy(fit2)
```

    ## # A tibble: 2 x 5
    ##   term          estimate std.error statistic   p.value
    ##   <chr>            <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     123.      2.32        53.0 0.       
    ## 2 pct_hs25_over     1.60    0.0654      24.4 2.26e-120

``` r
fit3 = lm(target_death_rate ~ pct_employed16_over, data = cancer.clean)
tidy(fit3)
```

    ## # A tibble: 2 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           252.      3.07        82.3 0.       
    ## 2 pct_employed16_over    -1.36    0.0560     -24.3 4.65e-119

``` r
fit4 = lm(target_death_rate ~ pct_private_coverage, data = cancer.clean)
tidy(fit4)
```

    ## # A tibble: 2 x 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            243.      2.84        85.6 0.       
    ## 2 pct_private_coverage    -1.01    0.0436     -23.1 7.15e-109

``` r
fit5 = lm(target_death_rate ~ pct_public_coverage, data = cancer.clean)
tidy(fit5)
```

    ## # A tibble: 2 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           127.      2.18        58.3 0.       
    ## 2 pct_public_coverage     1.43    0.0586      24.4 2.32e-120

``` r
fit6 = lm(target_death_rate ~ avg_ann_count_pop, data = cancer.clean)
tidy(fit6)
```

    ## # A tibble: 2 x 5
    ##   term              estimate std.error statistic p.value
    ##   <chr>                <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)          179.      0.514    348.   0      
    ## 2 avg_ann_count_pop    -15.3     4.81      -3.19 0.00142

``` r
fit7 = lm(target_death_rate ~ avg_deaths_yr_pop, data = cancer.clean)
tidy(fit7)
```

    ## # A tibble: 2 x 5
    ##   term              estimate std.error statistic   p.value
    ##   <chr>                <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           120.      1.61      74.7 0.       
    ## 2 avg_deaths_yr_pop   25509.    680.        37.5 2.66e-253

``` r
fit8 = lm(target_death_rate ~ med_income, data = cancer.clean)
tidy(fit8)
```

    ## # A tibble: 2 x 5
    ##   term           estimate std.error statistic   p.value
    ##   <chr>             <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)  225.       1.83          123.  0.       
    ## 2 med_income    -0.000988 0.0000377     -26.2 2.04e-136

``` r
fit9 = lm(target_death_rate ~ study_quantile, data = cancer.clean)
tidy(fit9)
```

    ## # A tibble: 5 x 5
    ##   term                    estimate std.error statistic   p.value
    ##   <chr>                      <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)               181.       0.628    288.   0        
    ## 2 study_quantileLow          -6.18     1.77      -3.50 0.000479 
    ## 3 study_quantileModerate     -6.38     1.77      -3.61 0.000312 
    ## 4 study_quantileHigh         -7.64     1.77      -4.32 0.0000158
    ## 5 study_quantileVery High    -4.76     1.77      -2.69 0.00709

``` r
fit10 = lm(target_death_rate ~ median_age_all_gender, data = cancer.clean)
tidy(fit10)
```

    ## # A tibble: 2 x 5
    ##   term                   estimate std.error statistic p.value
    ##   <chr>                     <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)            180.        4.00      44.9     0    
    ## 2 median_age_all_gender   -0.0264    0.0972    -0.271   0.786

``` r
fit11 = lm(target_death_rate ~ avg_household_size, data = cancer.clean)
tidy(fit11)
```

    ## # A tibble: 2 x 5
    ##   term               estimate std.error statistic p.value
    ##   <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)          185.        2.95     62.6   0     
    ## 2 avg_household_size    -2.39      1.17     -2.04  0.0416

``` r
fit12 = lm(target_death_rate ~ south, data = cancer.clean)
tidy(fit12)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)    170.      0.640     266.  0.      
    ## 2 south           18.8     0.950      19.8 2.25e-82

``` r
# start with one predictor that has the lowest p-value: avg_deaths_yr_pop
step1 = lm(target_death_rate ~ avg_deaths_yr_pop, data = cancer.clean)
tidy(step1)
```

    ## # A tibble: 2 x 5
    ##   term              estimate std.error statistic   p.value
    ##   <chr>                <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           120.      1.61      74.7 0.       
    ## 2 avg_deaths_yr_pop   25509.    680.        37.5 2.66e-253

``` r
# enter the one with the lowest p-value in the rest
step2 = update(step1, .~. + med_income)
tidy(step2)
```

    ## # A tibble: 3 x 5
    ##   term                   estimate   std.error statistic   p.value
    ##   <chr>                     <dbl>       <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          153.         3.08           49.7 0.       
    ## 2 avg_deaths_yr_pop  21014.       758.             27.7 4.83e-151
    ## 3 med_income            -0.000474   0.0000385     -12.3 5.33e- 34

``` r
step3 = update(step2, .~. + pct_hs25_over)
tidy(step3)
```

    ## # A tibble: 4 x 5
    ##   term                   estimate   std.error statistic   p.value
    ##   <chr>                     <dbl>       <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          140.         3.83          36.7  3.99e-244
    ## 2 avg_deaths_yr_pop  19288.       817.            23.6  2.44e-113
    ## 3 med_income            -0.000409   0.0000401    -10.2  4.51e- 24
    ## 4 pct_hs25_over          0.388      0.0705         5.50 4.02e-  8

``` r
step4 = update(step3, .~. + pct_public_coverage)
tidy(step4)
```

    ## # A tibble: 5 x 5
    ##   term                     estimate   std.error statistic   p.value
    ##   <chr>                       <dbl>       <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            158.         5.56          28.4  6.23e-158
    ## 2 avg_deaths_yr_pop    21077.       909.            23.2  1.47e-109
    ## 3 med_income              -0.000562   0.0000529    -10.6  6.13e- 26
    ## 4 pct_hs25_over            0.369      0.0705         5.24 1.70e-  7
    ## 5 pct_public_coverage     -0.389      0.0882        -4.42 1.03e-  5

``` r
step5 = update(step4, .~. + pct_employed16_over)
tidy(step5)
```

    ## # A tibble: 6 x 5
    ##   term                     estimate   std.error statistic   p.value
    ##   <chr>                       <dbl>       <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            227.         7.72          29.3  6.22e-166
    ## 2 avg_deaths_yr_pop    22811.       916.            24.9  2.91e-124
    ## 3 med_income              -0.000382   0.0000542     -7.04 2.37e- 12
    ## 4 pct_hs25_over            0.351      0.0698         5.03 5.27e-  7
    ## 5 pct_public_coverage     -1.10       0.104        -10.6  1.02e- 25
    ## 6 pct_employed16_over     -1.01       0.0795       -12.7  7.79e- 36

``` r
step6 = update(step5, .~. + pct_private_coverage)
tidy(step6)
```

    ## # A tibble: 7 x 5
    ##   term                      estimate   std.error statistic   p.value
    ##   <chr>                        <dbl>       <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)             287.         7.75          37.1  2.40e-246
    ## 2 avg_deaths_yr_pop     31266.       943.            33.2  1.53e-204
    ## 3 med_income                0.000108   0.0000556      1.94 5.22e-  2
    ## 4 pct_hs25_over             0.495      0.0654         7.57 5.02e- 14
    ## 5 pct_public_coverage      -2.09       0.108        -19.5  2.70e- 79
    ## 6 pct_employed16_over      -0.712      0.0754        -9.44 7.20e- 21
    ## 7 pct_private_coverage     -1.37       0.0650       -21.0  2.35e- 91

``` r
step7 = update(step6, .~. + south)
tidy(step7)
```

    ## # A tibble: 8 x 5
    ##   term                      estimate   std.error statistic   p.value
    ##   <chr>                        <dbl>       <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)             240.         8.90          27.0  3.46e-143
    ## 2 avg_deaths_yr_pop     30017.       935.            32.1  8.25e-194
    ## 3 med_income                0.000121   0.0000546      2.22 2.63e-  2
    ## 4 pct_hs25_over             0.519      0.0643         8.07 1.04e- 15
    ## 5 pct_public_coverage      -1.69       0.113        -14.9  1.43e- 48
    ## 6 pct_employed16_over      -0.438      0.0788        -5.56 3.02e-  8
    ## 7 pct_private_coverage     -1.14       0.0677       -16.8  1.58e- 60
    ## 8 south                     9.12       0.891         10.2  3.48e- 24

``` r
step8 = update(step7, .~. + pct_white)
tidy(step8)
```

    ## # A tibble: 9 x 5
    ##   term                 estimate   std.error statistic   p.value
    ##   <chr>                   <dbl>       <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           2.41e+2   8.71          27.6  1.96e-149
    ## 2 avg_deaths_yr_pop     3.16e+4 925.            34.2  2.62e-215
    ## 3 med_income            9.44e-5   0.0000535      1.76 7.79e-  2
    ## 4 pct_hs25_over         6.49e-1   0.0640        10.2  8.04e- 24
    ## 5 pct_public_coverage  -1.64e+0   0.111        -14.8  6.08e- 48
    ## 6 pct_employed16_over  -3.57e-1   0.0774        -4.61 4.15e-  6
    ## 7 pct_private_coverage -9.35e-1   0.0686       -13.6  4.52e- 41
    ## 8 south                 7.18e+0   0.888          8.09 8.99e- 16
    ## 9 pct_white            -3.08e-1   0.0270       -11.4  2.02e- 29

``` r
step9 = update(step8, .~. + study_quantile)
tidy(step9)
```

    ## # A tibble: 13 x 5
    ##    term                    estimate   std.error statistic   p.value
    ##    <chr>                      <dbl>       <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)              2.41e+2   8.55          28.2  1.41e-154
    ##  2 avg_deaths_yr_pop        3.25e+4 914.            35.6  1.74e-230
    ##  3 med_income               9.81e-5   0.0000530      1.85 6.43e-  2
    ##  4 pct_hs25_over            7.95e-1   0.0644        12.4  3.31e- 34
    ##  5 pct_public_coverage     -1.75e+0   0.109        -16.0  2.32e- 55
    ##  6 pct_employed16_over     -4.02e-1   0.0762        -5.27 1.45e-  7
    ##  7 pct_private_coverage    -1.04e+0   0.0680       -15.2  1.90e- 50
    ##  8 south                    7.40e+0   0.880          8.41 6.12e- 17
    ##  9 pct_white               -2.84e-1   0.0267       -10.7  4.69e- 26
    ## 10 study_quantileLow        7.64e+0   1.28           5.96 2.88e-  9
    ## 11 study_quantileModerate   7.67e+0   1.28           5.98 2.50e-  9
    ## 12 study_quantileHigh       7.94e+0   1.30           6.10 1.23e-  9
    ## 13 study_quantileVery High  1.01e+1   1.29           7.79 9.56e- 15

``` r
step10 = update(step9, .~. + avg_ann_count_pop)
tidy(step10)
```

    ## # A tibble: 14 x 5
    ##    term                    estimate   std.error statistic   p.value
    ##    <chr>                      <dbl>       <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)              2.42e+2   8.51          28.5  1.82e-157
    ##  2 avg_deaths_yr_pop        3.36e+4 927.            36.2  3.58e-237
    ##  3 med_income               7.67e-5   0.0000529      1.45 1.47e-  1
    ##  4 pct_hs25_over            7.56e-1   0.0644        11.7  4.37e- 31
    ##  5 pct_public_coverage     -1.80e+0   0.109        -16.5  1.87e- 58
    ##  6 pct_employed16_over     -3.76e-1   0.0759        -4.95 7.91e-  7
    ##  7 pct_private_coverage    -1.03e+0   0.0676       -15.2  2.94e- 50
    ##  8 south                    6.81e+0   0.881          7.73 1.47e- 14
    ##  9 pct_white               -2.91e-1   0.0266       -10.9  2.42e- 27
    ## 10 study_quantileLow        7.20e+0   1.28           5.64 1.88e-  8
    ## 11 study_quantileModerate   7.15e+0   1.28           5.59 2.46e-  8
    ## 12 study_quantileHigh       7.29e+0   1.30           5.60 2.34e-  8
    ## 13 study_quantileVery High  9.46e+0   1.29           7.32 3.27e- 13
    ## 14 avg_ann_count_pop       -2.03e+1   3.56          -5.71 1.26e-  8

``` r
step11 = update(step10, .~. + avg_household_size)
tidy(step11)
```

    ## # A tibble: 15 x 5
    ##    term                    estimate   std.error statistic   p.value
    ##    <chr>                      <dbl>       <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)              2.24e+2   9.29         24.2   1.65e-117
    ##  2 avg_deaths_yr_pop        3.43e+4 937.           36.7   9.95e-242
    ##  3 med_income               3.90e-5   0.0000533     0.733 4.64e-  1
    ##  4 pct_hs25_over            7.36e-1   0.0643       11.4   1.03e- 29
    ##  5 pct_public_coverage     -1.75e+0   0.109       -16.0   2.21e- 55
    ##  6 pct_employed16_over     -3.58e-1   0.0757       -4.72  2.42e-  6
    ##  7 pct_private_coverage    -9.48e-1   0.0694      -13.7   3.43e- 41
    ##  8 south                    6.88e+0   0.878         7.84  6.12e- 15
    ##  9 pct_white               -2.89e-1   0.0265      -10.9   2.52e- 27
    ## 10 study_quantileLow        7.09e+0   1.27          5.57  2.78e-  8
    ## 11 study_quantileModerate   6.99e+0   1.27          5.49  4.42e-  8
    ## 12 study_quantileHigh       7.34e+0   1.30          5.67  1.60e-  8
    ## 13 study_quantileVery High  9.49e+0   1.29          7.37  2.25e- 13
    ## 14 avg_ann_count_pop       -2.04e+1   3.55         -5.76  9.49e-  9
    ## 15 avg_household_size       4.35e+0   0.911         4.77  1.90e-  6

``` r
step12 = update(step11, .~. + median_age_all_gender)
tidy(step12)
```

    ## # A tibble: 16 x 5
    ##    term                         estimate   std.error statistic   p.value
    ##    <chr>                           <dbl>       <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                236.         7.13        33.1    1.94e-203
    ##  2 avg_deaths_yr_pop        51260.       811.          63.2    0.       
    ##  3 med_income                   0.000712   0.0000435   16.4    1.11e- 57
    ##  4 pct_hs25_over                0.688      0.0493      14.0    6.35e- 43
    ##  5 pct_public_coverage         -0.240      0.0901      -2.66   7.90e-  3
    ##  6 pct_employed16_over         -0.534      0.0582      -9.18   8.01e- 20
    ##  7 pct_private_coverage        -0.380      0.0547      -6.95   4.56e- 12
    ##  8 south                        7.97       0.673       11.8    1.32e- 31
    ##  9 pct_white                   -0.0724     0.0209      -3.47   5.21e-  4
    ## 10 study_quantileLow            3.33       0.979        3.40   6.71e-  4
    ## 11 study_quantileModerate       3.02       0.981        3.07   2.13e-  3
    ## 12 study_quantileHigh           2.61       0.999        2.61   9.02e-  3
    ## 13 study_quantileVery High      3.83       0.995        3.84   1.23e-  4
    ## 14 avg_ann_count_pop          -16.6        2.72        -6.09   1.25e-  9
    ## 15 avg_household_size          -0.0147     0.706       -0.0208 9.83e-  1
    ## 16 median_age_all_gender       -4.11       0.0914     -44.9    0.

``` r
# store the result of forward procedure
model_forward = step12
```

3)  stepwise procedure

<!-- end list -->

``` r
# use stepwise function to choose the best subsets
step(mult.fit, direction = "backward")
```

    ## Start:  AIC=15434.64
    ## target_death_rate ~ pct_white + pct_hs25_over + pct_employed16_over + 
    ##     pct_private_coverage + pct_public_coverage + avg_ann_count_pop + 
    ##     avg_household_size + avg_deaths_yr_pop + med_income + study_quantile + 
    ##     median_age_all_gender + south
    ## 
    ##                         Df Sum of Sq     RSS   AIC
    ## - avg_household_size     1         0  591945 15433
    ## <none>                                591944 15435
    ## - pct_public_coverage    1      1453  593397 15440
    ## - pct_white              1      2481  594425 15445
    ## - study_quantile         4      5742  597687 15455
    ## - avg_ann_count_pop      1      7635  599579 15470
    ## - pct_private_coverage   1      9926  601871 15481
    ## - pct_employed16_over    1     17327  609271 15516
    ## - south                  1     28815  620760 15570
    ## - pct_hs25_over          1     40088  632033 15622
    ## - med_income             1     55118  647063 15690
    ## - median_age_all_gender  1    415225 1007170 16971
    ## - avg_deaths_yr_pop      1    821413 1413358 17952
    ## 
    ## Step:  AIC=15432.64
    ## target_death_rate ~ pct_white + pct_hs25_over + pct_employed16_over + 
    ##     pct_private_coverage + pct_public_coverage + avg_ann_count_pop + 
    ##     avg_deaths_yr_pop + med_income + study_quantile + median_age_all_gender + 
    ##     south
    ## 
    ##                         Df Sum of Sq     RSS   AIC
    ## <none>                                591945 15433
    ## - pct_public_coverage    1      1454  593398 15438
    ## - pct_white              1      2483  594427 15443
    ## - study_quantile         4      5743  597688 15453
    ## - avg_ann_count_pop      1      7636  599581 15468
    ## - pct_private_coverage   1     10330  602275 15481
    ## - pct_employed16_over    1     17381  609326 15514
    ## - south                  1     28821  620766 15568
    ## - pct_hs25_over          1     40226  632170 15621
    ## - med_income             1     57045  648990 15697
    ## - median_age_all_gender  1    423194 1015138 16992
    ## - avg_deaths_yr_pop      1    827228 1419173 17962

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ pct_white + pct_hs25_over + 
    ##     pct_employed16_over + pct_private_coverage + pct_public_coverage + 
    ##     avg_ann_count_pop + avg_deaths_yr_pop + med_income + study_quantile + 
    ##     median_age_all_gender + south, data = cancer.clean)
    ## 
    ## Coefficients:
    ##             (Intercept)                pct_white            pct_hs25_over  
    ##               2.355e+02               -7.244e-02                6.884e-01  
    ##     pct_employed16_over     pct_private_coverage      pct_public_coverage  
    ##              -5.342e-01               -3.799e-01               -2.395e-01  
    ##       avg_ann_count_pop        avg_deaths_yr_pop               med_income  
    ##              -1.658e+01                5.126e+04                7.121e-04  
    ##       study_quantileLow   study_quantileModerate       study_quantileHigh  
    ##               3.335e+00                3.017e+00                2.612e+00  
    ## study_quantileVery High    median_age_all_gender                    south  
    ##               3.827e+00               -4.108e+00                7.972e+00

``` r
# store the result of stepwise procedure
model_stepwise = lm(target_death_rate ~ pct_white + pct_hs25_over + 
    pct_employed16_over + pct_private_coverage + pct_public_coverage + 
    avg_ann_count_pop + avg_deaths_yr_pop + med_income + study_quantile + 
    median_age_all_gender + south, data = cancer.clean)
```

Test-based procedures:

``` r
# generate test results and plot
criteria_results <- (leaps::regsubsets(data = cancer.clean, target_death_rate ~.) %>% summary())
tibble(
  n_pred = c(1:8),
  "Cp" = criteria_results$cp,
  "RSQ" = criteria_results$rsq,
  "RSS" = criteria_results$rss,
  "BIC" = criteria_results$bic
) %>% gather(key = "Statistics", value = "value", 2:5) %>%
  ggplot(aes(x = n_pred, y = value)) +
  geom_point() +
  geom_line() +
  facet_grid(Statistics ~ ., scales = "free_y") +
  theme_bw()
```

![](LinearModels_files/figure-gfm/test-based%20procedures-1.png)<!-- -->
