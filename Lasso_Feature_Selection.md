Lasso\_Feature\_Selection
================
Quinton Neville
December 12, 2018

Load, clean, manipulate, and tidy the data
==========================================

``` r
# Import data
cancer_raw = readr::read_csv("./Data/Cancer_Registry.csv") %>% 
  janitor::clean_names() 

#Pulling quartiles for study_per_cap categorical manipulation
study.quart <- with(cancer_raw, study_per_cap[study_per_cap > 0]) %>%
  quantile(., probs = c(0.25, 0.5, 0.75))

#To add a variable for region, state has too many factors for CV and data is not large enough for 51 fct var to be useful
#From data(state) built in R, going to left_join with cancer.df
#District of Columbia is missing, so I'm just going to impute it with the same factor as Maryland, 2
data(state)
state.df <- cbind(state.name, state.region) %>% as.tibble() %>% rename(state = state.name, region = state.region)

#Variable Manipulation
cancer.df <- cancer_raw %>%    #Remove Rows with > 20% missing 
  dplyr::select(-c(pct_some_col18_24, pct_employed16_over, pct_private_coverage_alone)) %>%  #Remove missing vars
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
  left_join(., state.df) %>%
  mutate(region = ifelse(is.na(region), 2, region),
         region = as.factor(region)) %>%
  dplyr::select(-c(binned_inc, geography, state, study_per_cap))

######TO avoid singular matrixes for our model fits, we need remove highly correlated and/or direct linear combos
###### Also lets make the response index 1, makes everything easier

cancer.df <- cancer.df %>%
  dplyr::select(-c(avg_deaths_per_year, avg_ann_count)) %>%
  dplyr::select(target_death_rate, study_quantile, region, everything())


#mostly need to decide between white/non-white or all race % AND binned_inc_lb, ub or median income
```

Feature Selection Lasso
=======================

Inputes are data (cancer.df), response (default target\_death\_rate), lambda (penalty, default = 1, higher penalty removes more coefficients), print(logical whether or not to print the results)

``` r
lasso.feature.removal <- function(data = cancer.df, response = "target_death_rate", lambda = 1, print = TRUE) {
  
  #Snag the index for matrices
  index <- which(names(data) == response)
  
  #Response 
  Y <- as.matrix(data[, index])
  
  #Design matrix
  X <- data[ ,-index] %>%
    names() %>% 
    paste(., collapse = "+") %>%    
    paste("~ ", .) %>%
    formula() %>%
    model.matrix(.,data)
  X <- X[,-1]  
  #Fit Model
  mod.lasso <- glmnet(X,Y,alpha = 1,intercept = T,lambda = lambda, family = "gaussian")
  #Print if true
  if (isTRUE(print)) {
  coef.lasso   <- coef(mod.lasso)[,1]
  remove.index <- which(coef.lasso == 0)
  keep.index   <- which(coef.lasso != 0)
                        
    print(sprintf("Lambda = %f : Lasso method removed %i variables:", lambda, length(remove.index)))
    print(paste(names(remove.index)))
    print(sprintf("Lambda = %f : Lasso method selected %i variables:",lambda, length(keep.index)))
    print(paste(names(keep.index)))
    cat("\n")
  }
 # return(names(which(coef.lasso == 0)))
}

map(.x = c(0.01, .1, 0.5, 1, 2), ~lasso.feature.removal(cancer.df, lambda = .x))
```

    ## [1] "Lambda = 0.010000 : Lasso method removed 0 variables:"
    ## character(0)
    ## [1] "Lambda = 0.010000 : Lasso method selected 39 variables:"
    ##  [1] "(Intercept)"               "study_quantileLow"        
    ##  [3] "study_quantileModerate"    "study_quantileHigh"       
    ##  [5] "study_quantileVery High"   "region2"                  
    ##  [7] "region3"                   "region4"                  
    ##  [9] "incidence_rate"            "med_income"               
    ## [11] "pop_est2015"               "poverty_percent"          
    ## [13] "median_age"                "median_age_male"          
    ## [15] "median_age_female"         "avg_household_size"       
    ## [17] "percent_married"           "pct_no_hs18_24"           
    ## [19] "pct_hs18_24"               "pct_bach_deg18_24"        
    ## [21] "pct_hs25_over"             "pct_bach_deg25_over"      
    ## [23] "pct_unemployed16_over"     "pct_private_coverage"     
    ## [25] "pct_emp_priv_coverage"     "pct_public_coverage"      
    ## [27] "pct_public_coverage_alone" "pct_white"                
    ## [29] "pct_black"                 "pct_asian"                
    ## [31] "pct_other_race"            "pct_married_households"   
    ## [33] "birth_rate"                "pct_non_white"            
    ## [35] "binned_inc_lb"             "binned_inc_ub"            
    ## [37] "binned_inc_point"          "avg_deaths_yr_pop"        
    ## [39] "avg_ann_count_pop"        
    ## 
    ## [1] "Lambda = 0.100000 : Lasso method removed 8 variables:"
    ## [1] "region3"           "poverty_percent"   "median_age"       
    ## [4] "pct_bach_deg18_24" "pct_black"         "pct_asian"        
    ## [7] "binned_inc_lb"     "binned_inc_point" 
    ## [1] "Lambda = 0.100000 : Lasso method selected 31 variables:"
    ##  [1] "(Intercept)"               "study_quantileLow"        
    ##  [3] "study_quantileModerate"    "study_quantileHigh"       
    ##  [5] "study_quantileVery High"   "region2"                  
    ##  [7] "region4"                   "incidence_rate"           
    ##  [9] "med_income"                "pop_est2015"              
    ## [11] "median_age_male"           "median_age_female"        
    ## [13] "avg_household_size"        "percent_married"          
    ## [15] "pct_no_hs18_24"            "pct_hs18_24"              
    ## [17] "pct_hs25_over"             "pct_bach_deg25_over"      
    ## [19] "pct_unemployed16_over"     "pct_private_coverage"     
    ## [21] "pct_emp_priv_coverage"     "pct_public_coverage"      
    ## [23] "pct_public_coverage_alone" "pct_white"                
    ## [25] "pct_other_race"            "pct_married_households"   
    ## [27] "birth_rate"                "pct_non_white"            
    ## [29] "binned_inc_ub"             "avg_deaths_yr_pop"        
    ## [31] "avg_ann_count_pop"        
    ## 
    ## [1] "Lambda = 0.500000 : Lasso method removed 18 variables:"
    ##  [1] "study_quantileLow"       "study_quantileModerate" 
    ##  [3] "study_quantileHigh"      "study_quantileVery High"
    ##  [5] "region3"                 "region4"                
    ##  [7] "med_income"              "pop_est2015"            
    ##  [9] "poverty_percent"         "median_age"             
    ## [11] "pct_bach_deg18_24"       "pct_public_coverage"    
    ## [13] "pct_black"               "pct_asian"              
    ## [15] "pct_married_households"  "pct_non_white"          
    ## [17] "binned_inc_lb"           "binned_inc_point"       
    ## [1] "Lambda = 0.500000 : Lasso method selected 21 variables:"
    ##  [1] "(Intercept)"               "region2"                  
    ##  [3] "incidence_rate"            "median_age_male"          
    ##  [5] "median_age_female"         "avg_household_size"       
    ##  [7] "percent_married"           "pct_no_hs18_24"           
    ##  [9] "pct_hs18_24"               "pct_hs25_over"            
    ## [11] "pct_bach_deg25_over"       "pct_unemployed16_over"    
    ## [13] "pct_private_coverage"      "pct_emp_priv_coverage"    
    ## [15] "pct_public_coverage_alone" "pct_white"                
    ## [17] "pct_other_race"            "birth_rate"               
    ## [19] "binned_inc_ub"             "avg_deaths_yr_pop"        
    ## [21] "avg_ann_count_pop"        
    ## 
    ## [1] "Lambda = 1.000000 : Lasso method removed 22 variables:"
    ##  [1] "study_quantileLow"       "study_quantileModerate" 
    ##  [3] "study_quantileHigh"      "study_quantileVery High"
    ##  [5] "region3"                 "region4"                
    ##  [7] "med_income"              "pop_est2015"            
    ##  [9] "poverty_percent"         "median_age"             
    ## [11] "avg_household_size"      "pct_no_hs18_24"         
    ## [13] "pct_bach_deg18_24"       "pct_private_coverage"   
    ## [15] "pct_public_coverage"     "pct_white"              
    ## [17] "pct_black"               "pct_asian"              
    ## [19] "pct_married_households"  "pct_non_white"          
    ## [21] "binned_inc_lb"           "binned_inc_point"       
    ## [1] "Lambda = 1.000000 : Lasso method selected 17 variables:"
    ##  [1] "(Intercept)"               "region2"                  
    ##  [3] "incidence_rate"            "median_age_male"          
    ##  [5] "median_age_female"         "percent_married"          
    ##  [7] "pct_hs18_24"               "pct_hs25_over"            
    ##  [9] "pct_bach_deg25_over"       "pct_unemployed16_over"    
    ## [11] "pct_emp_priv_coverage"     "pct_public_coverage_alone"
    ## [13] "pct_other_race"            "birth_rate"               
    ## [15] "binned_inc_ub"             "avg_deaths_yr_pop"        
    ## [17] "avg_ann_count_pop"        
    ## 
    ## [1] "Lambda = 2.000000 : Lasso method removed 27 variables:"
    ##  [1] "study_quantileLow"       "study_quantileModerate" 
    ##  [3] "study_quantileHigh"      "study_quantileVery High"
    ##  [5] "region3"                 "region4"                
    ##  [7] "med_income"              "pop_est2015"            
    ##  [9] "poverty_percent"         "median_age"             
    ## [11] "avg_household_size"      "pct_no_hs18_24"         
    ## [13] "pct_bach_deg18_24"       "pct_private_coverage"   
    ## [15] "pct_emp_priv_coverage"   "pct_public_coverage"    
    ## [17] "pct_white"               "pct_black"              
    ## [19] "pct_asian"               "pct_other_race"         
    ## [21] "pct_married_households"  "birth_rate"             
    ## [23] "pct_non_white"           "binned_inc_lb"          
    ## [25] "binned_inc_ub"           "binned_inc_point"       
    ## [27] "avg_ann_count_pop"      
    ## [1] "Lambda = 2.000000 : Lasso method selected 12 variables:"
    ##  [1] "(Intercept)"               "region2"                  
    ##  [3] "incidence_rate"            "median_age_male"          
    ##  [5] "median_age_female"         "percent_married"          
    ##  [7] "pct_hs18_24"               "pct_hs25_over"            
    ##  [9] "pct_bach_deg25_over"       "pct_unemployed16_over"    
    ## [11] "pct_public_coverage_alone" "avg_deaths_yr_pop"

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL
    ## 
    ## [[3]]
    ## NULL
    ## 
    ## [[4]]
    ## NULL
    ## 
    ## [[5]]
    ## NULL
