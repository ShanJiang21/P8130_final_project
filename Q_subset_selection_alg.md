Forward, Backward Subset Selection - MSE
================
Quinton Neville
12/8/2018

Load, clean, manipulate, and tidy the data
==========================================

``` r
# Import data
cancer_raw = readr::read_csv("./Data/Cancer_Registry.csv") %>% 
  janitor::clean_names() 

#dim(cancer_raw)
#head(cancer_raw)

# Check NA values for each column
#n_NA = sapply(cancer_raw[1:34], function(x) sum(length(which(is.na(x)))))
#n_NA

# Check the percentage of NA values for each column
#percentage_NA = sapply(cancer_raw[1:34], function(x) sum(length(which(is.na(x)))) / 3047)
#percentage_NA %>% data.frame()

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

######TO avoid singular matrixes for our model fits, we need remove highly correlated and/or direct linear combos

cancer.df <- cancer.df %>%
  dplyr::select(-c(avg_deaths_yr_pop, avg_ann_count_pop, pct_non_white, binned_inc_point))
```

Imputing Values with less than 20% missing (two variables)
==========================================================

-   pct\_employed16\_over ~ 4%
-   pct\_private\_coverage\_alone ~ 20%

``` r
#Impute those missing less than 20%
#1. pct_employed16_over
#2. pct_private_coverage_alone

#Set up appropriate test and train for pct_employed16_over (removing other missing % variable and response (target death))
train.df <- cancer.df %>% dplyr::select(-c(pct_private_coverage_alone, target_death_rate)) %>% filter(!is.na(pct_employed16_over))
test.df <- cancer.df %>% dplyr::select(-c(pct_private_coverage_alone, target_death_rate)) %>% filter(is.na(pct_employed16_over))

#Function for imputation (after correct test, train set up), charstring must literally be the character of impute variable i.e. "var1"
impute.lasso <- function(train.df, test.df, charstring){

  if ((charstring %in% names(train.df))) {
    
#pull variable index
index <- which(names(train.df) == charstring)
  
#Set up Matrices
#Create Design Matrix Train
X <- train.df[ ,-index] %>%
  names() %>% 
  paste("~ ", paste(., collapse = "+")) %>%
  formula() %>%
  model.matrix(.,train.df)
  
#Create Design Matrix Test
X1 <- test.df[, -index] %>%
  names() %>% 
  paste("~ ", paste(., collapse = "+")) %>%
  formula() %>%
  model.matrix(., test.df)

#Remove Intercept  
X <- X[,-1]
X1 <- X1[,-1]

#Create Response vector (as matrix)
Y <- train.df[, index] %>% as.matrix()

#Optimize lambda
lambda.grid <- 10^seq(-3,1,length = 100)

#CV n = 10
cv.lasso <- cv.glmnet(X, Y, alpha = 1, intercept = TRUE, lambda = lambda.grid, family = "gaussian")

#Grab optimal lambda
opt.lambda.lasso <- cv.lasso$lambda.min

#Run model
unemploy.lasso <- glmnet(X, Y, alpha = 1, intercept = TRUE, lambda = opt.lambda.lasso, family = "gaussian")

#Return predictions
predict(unemploy.lasso, newx = X1)
  }else{
    stop("Error: Incorrect variable name")
  }
}

#Impute employed16_over_preds (first since it has less missing data ~4%)
employed16_over_preds <- impute.lasso(train.df = train.df, test.df, "pct_employed16_over")

#Set up appropriate test and train
train.df <- cancer.df %>% dplyr::select(-c(pct_employed16_over, target_death_rate)) %>% filter(!is.na(pct_private_coverage_alone))
test.df <- cancer.df %>% dplyr::select(-c(pct_employed16_over, target_death_rate)) %>% filter(is.na(pct_private_coverage_alone))

#Impute pct_private_coverage_alone (second since it has more missing data ~20%)
pct_private_coverage_alone_preds <- impute.lasso(train.df = train.df, test.df, "pct_private_coverage_alone")

#Replace Imputed values
cancer.df <- cancer.df %>%
  mutate(imp_pct_employed16_over = ifelse(is.na(pct_employed16_over),
                                          employed16_over_preds, pct_employed16_over),
         imp_pct_private_coverage_alone = ifelse(is.na(pct_private_coverage_alone),
                                          pct_private_coverage_alone_preds, pct_private_coverage_alone)
        )

#Looks good, so we will replace imputed variables in our final data set
cancer.df <- cancer.df %>%
  dplyr::select(-c(pct_employed16_over, pct_private_coverage_alone))
```

See generally what the optimal number of preds in an lm() ought to be (CP, adj*R*<sup>2</sup>, BIC (similar to AIC)).

``` r
#Summary of models for each size (one model per size)
#Set max number of predictors
nvmax <- ncol(cancer.df) - 2
reg.subsets <- cancer.df %>% 
  dplyr::select(-c(state)) %>%
  regsubsets(target_death_rate ~ ., data = ., really.big = FALSE, nvmax = nvmax)
rs <- summary(reg.subsets)

# Plots of Cp and Adj-R2 as functions of parameters
r.df <- tibble(
  preds = 1:nvmax,
  cp = rs$cp,
  adjr2 = rs$adjr2,
  bic = rs$bic,
  step = 1:nvmax
)

cp.plot <- r.df %>% ggplot(aes(x = preds, y = cp, size = cp)) +
  geom_point(colour = "purple") +
  geom_line(alpha = 0.5, colour = "purple", size = 1.25) +
 # geom_point(aes(x = preds, step),color = "black",size = 0.5) +
  geom_line(aes(x = preds, step), size = 1, color = "red") +
  labs(
    x = "Number of Preds",
    y = "CP Criterion",
    title = "Optimal Number of Preds, CP"
  ) + 
  #scale_y_continuous(breaks = seq(0, 1000, 100))
  ylim(c(0, 300)) + 
  theme(legend.position = "none")

adjr2.plot <- r.df %>% ggplot(aes(x = preds, y = adjr2, size = 1 - adjr2)) +
  geom_point(colour = "purple") +
  geom_line(alpha = 0.5, colour = "purple", size = 1.25) +
  labs(
    x = "Number of Preds",
    y = "Adjusted R^2",
    title = "Optimal Number of Preds, Adj.R^2"
  ) + theme(legend.position = "none")

bic.plot <- r.df %>% ggplot(aes(x = preds, y = bic, size = bic)) +
  geom_point(colour = "purple") +
  geom_line(alpha = 0.5, colour = "purple", size = 1.25) +
  labs(
    x = "Number of Preds",
    y = "Bayesian Information Criterion",
    title = "Optimal Number of Preds, BIC"
  ) + theme(legend.position = "none")

(cp.plot + adjr2.plot) / bic.plot
```

<img src="Q_subset_selection_alg_files/figure-markdown_github/unnamed-chunk-4-1.png" width="90%" style="display: block; margin: auto;" />

Based on the plots above, *C**P* criterion in the upper left with *p* ≤ *C**P* constraint in red, that somewhere between a 20-30 predictor model ought to be optimal. With respect to adjusted *R*<sup>2</sup>, it appears as though we reach a converging maximum starting around 20 predictors through about 25, where additional predictors have diminishing marginal return. Lastly, BIC is more conservative (penalizing more strongly for more predictors) and seems to suggest between a 15-20 predictor model (closer to 20). This may inform our subset selection criterion.

K-fold cross validation funciton for subset selecion (MSE, AIC, R^2 criterion)
==============================================================================

``` r
sampleSize <- nrow(cancer.df)

mseCV <- function(data.df, kfolds = 10){
  folds <- sample(1:kfolds, sampleSize, rep = T)
  mse <- rep(0, kfolds)
  for (k in 1:kfolds) {
    train.df <- data.df[folds != k,]
    test.df <- data.df[folds == k,]
    
    lm.mod <- lm(target_death_rate ~ ., data = train.df)
    preds <- predict(lm.mod, newdata = test.df)
    mse[k] <- with(test.df,mean((target_death_rate - preds)^2))
  }
  mean(mse)
}

aicCV <- function(data.df, kfolds = 10){
  folds <- sample(1:kfolds, sampleSize, rep = T)
  aic <- rep(0, kfolds)
  for (k in 1:kfolds) {
    train.df <- data.df[folds != k,]
    test.df <- data.df[folds == k,]
    
    lm.mod <- lm(target_death_rate ~ ., data = train.df)
    aic[k] <- AIC(lm.mod)
  }
  mean(aic)
}

r2CV <- function(data.df, kfolds = 10){
  folds <- sample(1:kfolds, sampleSize, rep = T)
  r2 <- rep(0, kfolds)
  for (k in 1:kfolds) {
    train.df <- data.df[folds != k,]
    test.df <- data.df[folds == k,]
    
    lm.mod <- lm(target_death_rate ~ ., data = train.df)
    r2[k] <- summary(lm.mod)$adj.r.squared
  }
  mean(r2)
}
```

Forward Subset Selection with k-Fold CV and MSE criterion
=========================================================

Set up data for subset selection algorithm
==========================================

``` r
#Reorder the data so the response comes first, necessary for indexing
sub.cancer.df <- cancer.df %>%
  dplyr::select(target_death_rate, everything()) %>%
  dplyr::select(-state) # need to take out state for CV to run, too many levels (51) will be fine for full model, test later
```

Function to run the subset algorithm, it will output the variable selection process so you visualize how it works. Comment out the set seed to get true variability in the subsets, only leave it in for reproducibility.

-   The function `f.subset.mse(sub.cancer.df, maxPreds = 10, nfolds = 5)` takes in a data frame, max number of predictors you want the algorithm to run through (can't be larger than the number of predictors in the data frame), and the number of folds for the cross validation.

##### Note mustreorder data frame so response is the first column of the data frame (see above code chunk)

##### Also have to take out state variable, too many factor levels and CV process can fail (re add later to see if it's useful)

-   The way the algorithm works is

1.  Starts with an empty (null) set of current predictors in the data frame input
2.  Starts with a full set of available predictors
3.  Loops through all available preds and adds one at a time, splits the data into kfolds test/train (CV), fits lm() with current preds, stores MSE
4.  Selects the predictor whose CV MSE was smallest
5.  Adds 'best' predictor to current predictors, removes it from available predictors
6.  Stores that predictor set as 'best' and records the indices and CV MSE in a matrix
7.  Repeats 3.- 5. until you have met the maximum number of predictors you specified
8.  Returns a list with all 'best' predictor sets at each iteration, matrix of results, and prespecified max preds
9.  Last few lines of code output the optimal (minimum MSE) predictor set

*notes - CV is set to 5 for speed, changing to 10 will take ~1.5 times as long to run* *notes - the print() lines within the function are so you can visualize how the algorithm is working, they can be commented out to reduce clutter when you want to run the selection function iteratively (avoids reams of output)*

``` r
#Forward Subset#
#set.seed(4) #Comment this out if you want to really get a different subset 

f.subset <- function(sub.cancer.df, maxPreds = 10, nfolds = 5, criterion = "mse") {
#Selection
#number of possible predictors
numPreds <- ncol(sub.cancer.df) - 1
## Allthe predictors (their indices).
allPreds <- 1:ncol(sub.cancer.df)
allPreds <- allPreds[-1]

#current set of preds (starts empty), and available
currPreds <- c()
availPreds <- setdiff(allPreds,currPreds)
#Record the min errors
minError <- c()
#The maximimum size of our predictor set
#maxPreds <- 30
#Initalize pred list and mse result matrix
pred.list <- list()
result.mat <- matrix(nrow = ncol(cancer.df) - 1, ncol = 2)
i <- 1
#Forward selection loop
while (length(currPreds) < maxPreds) {
  ##add predictor which decreases MSE (as determined by CV or
  ##Bootstrapping)
  ## The MSEs computed as we add each of the available predictors
  allError <- c()
  for (id in availPreds) {
    data.df <- sub.cancer.df[,c(currPreds,id,1)]
    if (criterion == "mse") {
      error <- mseCV(data.df, nfolds)
    } else if (criterion == "aic") {
      error <- aicCV(data.df, nfolds)
    } else {
      stop("Wrong criterion input")
    }
  #  error <- mseCV(data.df, nfolds)
  #  error <- aicCV(data.df, nfolds)
   # error <- r2CV(data.df, nfolds)
    allError <- c(allError,error)
  }
  ##Find the min
  id <- which.min(allError)
  ##get the best predictor and MSW
  bestPred <- availPreds[id]
  bestError <- min(allError)
  ##Add these into the collection
  currPreds <- c(currPreds,bestPred)
  minError <- c(minError,bestError)
  availPreds <- setdiff(allPreds,currPreds)
  ## Print stuff out for debugging and attention-grabbing
  print(sprintf("Iteration: %i Predictor Added: %s %s Value: %s",i, names(sub.cancer.df[,bestPred]), criterion, bestError))
  print(currPreds)
  result.mat[i,] <- c(bestPred,bestError)         #You can also comment out this print output, it's just to watch the algorithm work
  pred.list[[i]] <- currPreds
  i <- i + 1
    }
  return(list(pred.list = pred.list, result.mat = result.mat, maxPreds = maxPreds))
}

#Run Subset, call function, output is a list with predictor sets, reslut matrix and maxPreds
f.mse.list <- f.subset(sub.cancer.df, maxPreds = ncol(sub.cancer.df) - 1, nfolds = 5, criterion = "mse")
```

    ## [1] "Iteration: 1 Predictor Added: pct_bach_deg25_over mse Value: 589.934684811318"
    ## [1] 17
    ## [1] "Iteration: 2 Predictor Added: incidence_rate mse Value: 446.618777509689"
    ## [1] 17  4
    ## [1] "Iteration: 3 Predictor Added: poverty_percent mse Value: 413.764923613172"
    ## [1] 17  4  7
    ## [1] "Iteration: 4 Predictor Added: pct_other_race mse Value: 406.890114470969"
    ## [1] 17  4  7 26
    ## [1] "Iteration: 5 Predictor Added: pct_hs18_24 mse Value: 399.161874303024"
    ## [1] 17  4  7 26 14
    ## [1] "Iteration: 6 Predictor Added: pct_married_households mse Value: 394.877537506145"
    ## [1] 17  4  7 26 14 27
    ## [1] "Iteration: 7 Predictor Added: pct_public_coverage_alone mse Value: 392.927305608925"
    ## [1] 17  4  7 26 14 27 22
    ## [1] "Iteration: 8 Predictor Added: pct_emp_priv_coverage mse Value: 390.128365227532"
    ## [1] 17  4  7 26 14 27 22 20
    ## [1] "Iteration: 9 Predictor Added: pct_public_coverage mse Value: 388.280753743216"
    ## [1] 17  4  7 26 14 27 22 20 21
    ## [1] "Iteration: 10 Predictor Added: pct_bach_deg18_24 mse Value: 387.627269327759"
    ##  [1] 17  4  7 26 14 27 22 20 21 15
    ## [1] "Iteration: 11 Predictor Added: binned_inc_lb mse Value: 386.247939085129"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29
    ## [1] "Iteration: 12 Predictor Added: imp_pct_employed16_over mse Value: 384.849104136682"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32
    ## [1] "Iteration: 13 Predictor Added: pct_white mse Value: 385.011361318223"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23
    ## [1] "Iteration: 14 Predictor Added: percent_married mse Value: 384.377778533005"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12
    ## [1] "Iteration: 15 Predictor Added: birth_rate mse Value: 380.807625094985"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12 28
    ## [1] "Iteration: 16 Predictor Added: pct_asian mse Value: 380.941018491511"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12 28 25
    ## [1] "Iteration: 17 Predictor Added: pct_unemployed16_over mse Value: 380.941956164078"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12 28 25 18
    ## [1] "Iteration: 18 Predictor Added: avg_household_size mse Value: 381.888027487971"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12 28 25 18 11
    ## [1] "Iteration: 19 Predictor Added: median_age_male mse Value: 381.502590037561"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12 28 25 18 11  9
    ## [1] "Iteration: 20 Predictor Added: avg_ann_count mse Value: 379.6289278169"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12 28 25 18 11  9  2
    ## [1] "Iteration: 21 Predictor Added: pct_hs25_over mse Value: 379.172923863588"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12 28 25 18 11  9  2 16
    ## [1] "Iteration: 22 Predictor Added: pct_private_coverage mse Value: 376.96261979945"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12 28 25 18 11  9  2 16 19
    ## [1] "Iteration: 23 Predictor Added: avg_deaths_per_year mse Value: 376.032943188057"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12 28 25 18 11  9  2 16 19  3
    ## [1] "Iteration: 24 Predictor Added: binned_inc_ub mse Value: 375.888255151594"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12 28 25 18 11  9  2 16 19  3
    ## [24] 30
    ## [1] "Iteration: 25 Predictor Added: pop_est2015 mse Value: 375.189831926439"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12 28 25 18 11  9  2 16 19  3
    ## [24] 30  6
    ## [1] "Iteration: 26 Predictor Added: pct_black mse Value: 374.20115206162"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12 28 25 18 11  9  2 16 19  3
    ## [24] 30  6 24
    ## [1] "Iteration: 27 Predictor Added: median_age_female mse Value: 375.457390503731"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12 28 25 18 11  9  2 16 19  3
    ## [24] 30  6 24 10
    ## [1] "Iteration: 28 Predictor Added: imp_pct_private_coverage_alone mse Value: 375.302650335913"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12 28 25 18 11  9  2 16 19  3
    ## [24] 30  6 24 10 33
    ## [1] "Iteration: 29 Predictor Added: pct_no_hs18_24 mse Value: 375.977923479583"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12 28 25 18 11  9  2 16 19  3
    ## [24] 30  6 24 10 33 13
    ## [1] "Iteration: 30 Predictor Added: med_income mse Value: 375.96780510746"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12 28 25 18 11  9  2 16 19  3
    ## [24] 30  6 24 10 33 13  5
    ## [1] "Iteration: 31 Predictor Added: median_age mse Value: 378.625464631937"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12 28 25 18 11  9  2 16 19  3
    ## [24] 30  6 24 10 33 13  5  8
    ## [1] "Iteration: 32 Predictor Added: study_quantile mse Value: 380.645159458295"
    ##  [1] 17  4  7 26 14 27 22 20 21 15 29 32 23 12 28 25 18 11  9  2 16 19  3
    ## [24] 30  6 24 10 33 13  5  8 31

``` r
f.aic.list <- f.subset(sub.cancer.df, maxPreds = ncol(sub.cancer.df) - 1, nfolds = 5, criterion = "aic")
```

    ## [1] "Iteration: 1 Predictor Added: pct_bach_deg25_over aic Value: 22468.4838924968"
    ## [1] 17
    ## [1] "Iteration: 2 Predictor Added: incidence_rate aic Value: 21790.6327481605"
    ## [1] 17  4
    ## [1] "Iteration: 3 Predictor Added: poverty_percent aic Value: 21602.0367670333"
    ## [1] 17  4  7
    ## [1] "Iteration: 4 Predictor Added: pct_hs18_24 aic Value: 21555.8898100139"
    ## [1] 17  4  7 14
    ## [1] "Iteration: 5 Predictor Added: pct_other_race aic Value: 21513.8642782757"
    ## [1] 17  4  7 14 26
    ## [1] "Iteration: 6 Predictor Added: pct_married_households aic Value: 21489.5921411629"
    ## [1] 17  4  7 14 26 27
    ## [1] "Iteration: 7 Predictor Added: median_age_female aic Value: 21476.8415653098"
    ## [1] 17  4  7 14 26 27 10
    ## [1] "Iteration: 8 Predictor Added: birth_rate aic Value: 21457.2585900219"
    ## [1] 17  4  7 14 26 27 10 28
    ## [1] "Iteration: 9 Predictor Added: pct_private_coverage aic Value: 21442.308324412"
    ## [1] 17  4  7 14 26 27 10 28 19
    ## [1] "Iteration: 10 Predictor Added: pct_emp_priv_coverage aic Value: 21430.5751528592"
    ##  [1] 17  4  7 14 26 27 10 28 19 20
    ## [1] "Iteration: 11 Predictor Added: binned_inc_lb aic Value: 21414.3284115128"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29
    ## [1] "Iteration: 12 Predictor Added: binned_inc_ub aic Value: 21402.2647058233"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30
    ## [1] "Iteration: 13 Predictor Added: percent_married aic Value: 21395.3811134002"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12
    ## [1] "Iteration: 14 Predictor Added: imp_pct_employed16_over aic Value: 21382.0764478408"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32
    ## [1] "Iteration: 15 Predictor Added: pct_hs25_over aic Value: 21372.1534184535"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32 16
    ## [1] "Iteration: 16 Predictor Added: pct_white aic Value: 21369.1966602435"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32 16 23
    ## [1] "Iteration: 17 Predictor Added: pct_no_hs18_24 aic Value: 21366.0411411527"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32 16 23 13
    ## [1] "Iteration: 18 Predictor Added: avg_ann_count aic Value: 21363.2084878454"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32 16 23 13  2
    ## [1] "Iteration: 19 Predictor Added: avg_deaths_per_year aic Value: 21350.7788872973"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32 16 23 13  2  3
    ## [1] "Iteration: 20 Predictor Added: pop_est2015 aic Value: 21344.9716330239"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32 16 23 13  2  3  6
    ## [1] "Iteration: 21 Predictor Added: median_age_male aic Value: 21340.3406814342"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32 16 23 13  2  3  6  9
    ## [1] "Iteration: 22 Predictor Added: pct_black aic Value: 21340.5255719602"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32 16 23 13  2  3  6  9 24
    ## [1] "Iteration: 23 Predictor Added: pct_asian aic Value: 21341.0923574567"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32 16 23 13  2  3  6  9 24 25
    ## [1] "Iteration: 24 Predictor Added: med_income aic Value: 21340.1487219155"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32 16 23 13  2  3  6  9 24 25
    ## [24]  5
    ## [1] "Iteration: 25 Predictor Added: avg_household_size aic Value: 21340.5139509611"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32 16 23 13  2  3  6  9 24 25
    ## [24]  5 11
    ## [1] "Iteration: 26 Predictor Added: imp_pct_private_coverage_alone aic Value: 21340.7097649758"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32 16 23 13  2  3  6  9 24 25
    ## [24]  5 11 33
    ## [1] "Iteration: 27 Predictor Added: pct_unemployed16_over aic Value: 21342.4448927351"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32 16 23 13  2  3  6  9 24 25
    ## [24]  5 11 33 18
    ## [1] "Iteration: 28 Predictor Added: pct_bach_deg18_24 aic Value: 21343.1107539465"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32 16 23 13  2  3  6  9 24 25
    ## [24]  5 11 33 18 15
    ## [1] "Iteration: 29 Predictor Added: median_age aic Value: 21344.6816544553"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32 16 23 13  2  3  6  9 24 25
    ## [24]  5 11 33 18 15  8
    ## [1] "Iteration: 30 Predictor Added: pct_public_coverage_alone aic Value: 21344.8333988179"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32 16 23 13  2  3  6  9 24 25
    ## [24]  5 11 33 18 15  8 22
    ## [1] "Iteration: 31 Predictor Added: pct_public_coverage aic Value: 21346.7344929233"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32 16 23 13  2  3  6  9 24 25
    ## [24]  5 11 33 18 15  8 22 21
    ## [1] "Iteration: 32 Predictor Added: study_quantile aic Value: 21353.089713947"
    ##  [1] 17  4  7 14 26 27 10 28 19 20 29 30 12 32 16 23 13  2  3  6  9 24 25
    ## [24]  5 11 33 18 15  8 22 21 31

``` r
#Show the 'best' final selection with minimal MSE
present.fs.result <- function(f.result.list, criterion) {
lm.fs.preds <- with(f.result.list, pred.list[[which.min(result.mat[,2])]]) #Pick out indices from best model
fs.mse <- with(f.result.list, result.mat[which.min(result.mat[,2]), 2])
print(sprintf("The best predictor set of %s predictors, out of a max of %s, (%s = %s)",
              length(lm.fs.preds), f.result.list$maxPreds, criterion, round(fs.mse, 3)))
print(names(sub.cancer.df[,c(lm.fs.preds)]))
}

present.fs.result(f.mse.list, "MSE")
```

    ## [1] "The best predictor set of 26 predictors, out of a max of 32, (MSE = 374.201)"
    ##  [1] "pct_bach_deg25_over"       "incidence_rate"           
    ##  [3] "poverty_percent"           "pct_other_race"           
    ##  [5] "pct_hs18_24"               "pct_married_households"   
    ##  [7] "pct_public_coverage_alone" "pct_emp_priv_coverage"    
    ##  [9] "pct_public_coverage"       "pct_bach_deg18_24"        
    ## [11] "binned_inc_lb"             "imp_pct_employed16_over"  
    ## [13] "pct_white"                 "percent_married"          
    ## [15] "birth_rate"                "pct_asian"                
    ## [17] "pct_unemployed16_over"     "avg_household_size"       
    ## [19] "median_age_male"           "avg_ann_count"            
    ## [21] "pct_hs25_over"             "pct_private_coverage"     
    ## [23] "avg_deaths_per_year"       "binned_inc_ub"            
    ## [25] "pop_est2015"               "pct_black"

``` r
present.fs.result(f.aic.list, "AIC")
```

    ## [1] "The best predictor set of 24 predictors, out of a max of 32, (AIC = 21340.149)"
    ##  [1] "pct_bach_deg25_over"     "incidence_rate"         
    ##  [3] "poverty_percent"         "pct_hs18_24"            
    ##  [5] "pct_other_race"          "pct_married_households" 
    ##  [7] "median_age_female"       "birth_rate"             
    ##  [9] "pct_private_coverage"    "pct_emp_priv_coverage"  
    ## [11] "binned_inc_lb"           "binned_inc_ub"          
    ## [13] "percent_married"         "imp_pct_employed16_over"
    ## [15] "pct_hs25_over"           "pct_white"              
    ## [17] "pct_no_hs18_24"          "avg_ann_count"          
    ## [19] "avg_deaths_per_year"     "pop_est2015"            
    ## [21] "median_age_male"         "pct_black"              
    ## [23] "pct_asian"               "med_income"

``` r
#fs.lm <- lm(target_death_rate ~ ., data = cancer.df[,c(lm.fs.preds,1)])
```

If you want to repeat the process to get a feel for what you think might actually be the best of the 'best' subset selections (they very slightly from iteration to iteration by cross validation). Pick the number of iterations `len` and let the chunk run, you can see the process work (reccomend commenting out the print() calls in the functions above to reduce all the extra output), then at the end it will print all the subsets selected and you can sift through which variables you want for a final subset model. See which ones get selected most often basically.

``` r
#Repeat if you want to
#Number of repetitions
len <- 10
f.result.list <- list()

#Repeat Subset Selection algorithm
for (i in 1:len) {
  f.result.list[[i]] <- f.subset(sub.cancer.df, maxPreds = 10, nfolds = 5, criterion = "mse")
}

#View the results
for (i in 1:len) {
present.fs.result(f.result.list[[i]], "MSE")
}

#See which vars seem to be selected most often, make a subjective call what subset you like best.

#fs.lm <- lm(target_death_rate ~ ., data = cancer.df[,c(lm.fs.preds,1)])
```

Repeat the process for Backwards Subset
=======================================

NOTE this will take much longer to run
--------------------------------------

##### Again must reorder data frame so response is the first column of the data frame (see above code chunk)

##### Again state variable removed, too many factor levels and CV process can fail (re add later to see if it's useful)

-   The way the algorithm works is

1.  Starts with an exhaustive set of current predictors in the data frame input (all preds in the data frame)
2.  Starts with a empty set of predictors removed
3.  Loops through all current preds and removes one at a time, splits the data into kfolds test/train (CV), fits lm() with current preds, stores MSE
4.  Selects the set whose CV MSE was smallest
5.  The optimal set had one of the predictors removed, selects that predictor as 'worst; removes it from current predictors
6.  Stores that predictor set as worst and records the indices and CV MSE in a matrix for the model without that pred
7.  Repeats 3.- 5. until you have met the minimum number of predictors you specified
8.  Returns a list with all 'best' predictor sets at each iteration, matrix of results, and prespecified max preds
9.  Last few lines of code output the optimal (minimum MSE) predictor set

*notes - CV is set to 5 for speed, changing to 10 will take ~1.5 times as long to run* *notes - the print() lines within the function are so you can visualize how the algorithm is working, they can be commented out to reduce clutter when you want to run the selection function iteratively (avoids reams of output)*

``` r
sub.cancer.df <- cancer.df %>%
  dplyr::select(target_death_rate, everything()) %>%
  dplyr::select(-state)
  #dplyr::select(-c(state, avg_deaths_yr_pop, avg_ann_count_pop, pct_non_white, binned_inc_point))
#For backwards to work well we need remove vars we created that are combos of other vars,
#This will work better when we have a smaller non-interdependent data set with reduced variables
```

``` r
#Backward Selection
#set.seed(44)  #Set seed for reproducibility
###############################################

maxPreds <- ncol(sub.cancer.df - 1)

b.subset <- function(sub.cancer.df, minPreds = 1, nfolds = 5, criterion = "mse"){

## Allthe predictors (their indices).
allPreds <- 1:ncol(sub.cancer.df)
allPreds <- allPreds[-1]

#current set of preds (starts empty), and available
currPreds <- allPreds
availPreds <- allPreds
#Record the min errors
minError <- c()
#The minimum size of our predictor set minPreds

pred.list <- list()
result.mat <- matrix(nrow = ncol(sub.cancer.df) - 1, ncol = 2)
i <- 1
#Forward selection loop
while (length(currPreds) >= minPreds) {
  ##add predictor which decreases MSE (as determined by CV)
  ## The MSEs computed as we add each of the available predictors
  allError <- c()
  for (id in availPreds) {
    data.df <- sub.cancer.df[,c(currPreds[-id],1)]
    if (criterion == "mse") {
      error <- mseCV(data.df, nfolds)
    } else if (criterion == "aic") {
      error <- aicCV(data.df, nfolds)
    } else {
      stop("Wrong criterion input")
    }
    allError <- c(allError,error)
  }
  ##Find the min
  id <- which.min(allError)
  ##get the worst predictor and MSE
  worstPred <- availPreds[id]
  bestError <- min(allError)
  ##Add these into the collection
  currPreds <- currPreds[-id]
  minError <- c(minError, bestError)
  availPreds <- currPreds
  ## Print stuff out for debugging and attention-grabbing
  print(sprintf("Predictor Removed: %s  %s Value: %s",names(sub.cancer.df[,worstPred]), criterion, bestError))
  print(currPreds)
  result.mat[i,] <- c(worstPred,bestError)
  pred.list[[i]] <- currPreds
  
  i <- i + 1
  }
  list(pred.list = pred.list, result.mat = result.mat)
}

mse.result.list <- b.subset(sub.cancer.df, minPreds = 10, nfolds = 5, criterion = "mse")
```

    ## [1] "Predictor Removed: pop_est2015  mse Value: 374.853796096619"
    ##  [1]  2  3  4  5  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    ## [24] 26 27 28 29 30 31 32 33
    ## [1] "Predictor Removed: median_age_male  mse Value: 374.869886904723"
    ##  [1]  2  3  4  5  7  8 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
    ## [24] 27 28 29 30 31 32 33
    ## [1] "Predictor Removed: med_income  mse Value: 375.117420268297"
    ##  [1]  2  3  4  7  8 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
    ## [24] 28 29 30 31 32 33
    ## [1] "Predictor Removed: pct_bach_deg25_over  mse Value: 373.871732380756"
    ##  [1]  2  3  4  7  8 10 11 12 13 14 15 16 18 19 20 21 22 23 24 25 26 27 28
    ## [24] 29 30 31 32 33
    ## [1] "Predictor Removed: pct_no_hs18_24  mse Value: 383.937033613864"
    ##  [1]  2  3  4  7  8 10 11 12 14 15 16 18 19 20 21 22 23 24 25 26 27 28 29
    ## [24] 30 31 32 33
    ## [1] "Predictor Removed: binned_inc_lb  mse Value: 384.059620549874"
    ##  [1]  2  3  4  7  8 10 11 12 14 15 16 18 19 20 21 22 23 24 25 26 27 28 30
    ## [24] 31 32 33
    ## [1] "Predictor Removed: percent_married  mse Value: 385.371231554689"
    ##  [1]  2  3  4  7  8 10 11 14 15 16 18 19 20 21 22 23 24 25 26 27 28 30 31
    ## [24] 32 33
    ## [1] "Predictor Removed: binned_inc_ub  mse Value: 392.061098746136"
    ##  [1]  2  3  4  7  8 10 11 14 15 16 18 19 20 21 22 23 24 25 26 27 28 31 32
    ## [24] 33
    ## [1] "Predictor Removed: imp_pct_employed16_over  mse Value: 392.017354443507"
    ##  [1]  2  3  4  7  8 10 11 14 15 16 18 19 20 21 22 23 24 25 26 27 28 31 33
    ## [1] "Predictor Removed: pct_hs25_over  mse Value: 392.148231580073"
    ##  [1]  2  3  4  7  8 10 11 14 15 18 19 20 21 22 23 24 25 26 27 28 31 33
    ## [1] "Predictor Removed: pct_married_households  mse Value: 408.492653003669"
    ##  [1]  2  3  4  7  8 10 11 14 15 18 19 20 21 22 23 24 25 26 28 31 33
    ## [1] "Predictor Removed: pct_black  mse Value: 408.1930994256"
    ##  [1]  2  3  4  7  8 10 11 14 15 18 19 20 21 22 23 25 26 28 31 33
    ## [1] "Predictor Removed: pct_other_race  mse Value: 408.593943752519"
    ##  [1]  2  3  4  7  8 10 11 14 15 18 19 20 21 22 23 25 28 31 33
    ## [1] "Predictor Removed: pct_hs18_24  mse Value: 417.534556742602"
    ##  [1]  2  3  4  7  8 10 11 15 18 19 20 21 22 23 25 28 31 33
    ## [1] "Predictor Removed: imp_pct_private_coverage_alone  mse Value: 427.767450682878"
    ##  [1]  2  3  4  7  8 10 11 15 18 19 20 21 22 23 25 28 31
    ## [1] "Predictor Removed: pct_private_coverage  mse Value: 427.116913135402"
    ##  [1]  2  3  4  7  8 10 11 15 18 20 21 22 23 25 28 31
    ## [1] "Predictor Removed: avg_household_size  mse Value: 431.835997729585"
    ##  [1]  2  3  4  7  8 10 15 18 20 21 22 23 25 28 31
    ## [1] "Predictor Removed: pct_unemployed16_over  mse Value: 430.67755369942"
    ##  [1]  2  3  4  7  8 10 15 20 21 22 23 25 28 31
    ## [1] "Predictor Removed: pct_public_coverage_alone  mse Value: 432.311105912218"
    ##  [1]  2  3  4  7  8 10 15 20 21 23 25 28 31
    ## [1] "Predictor Removed: median_age_female  mse Value: 433.851028104769"
    ##  [1]  2  3  4  7  8 15 20 21 23 25 28 31
    ## [1] "Predictor Removed: pct_public_coverage  mse Value: 435.685374809128"
    ##  [1]  2  3  4  7  8 15 20 23 25 28 31
    ## [1] "Predictor Removed: pct_emp_priv_coverage  mse Value: 443.521065185735"
    ##  [1]  2  3  4  7  8 15 23 25 28 31
    ## [1] "Predictor Removed: birth_rate  mse Value: 442.38087846204"
    ## [1]  2  3  4  7  8 15 23 25 31

``` r
aic.result.list <- b.subset(sub.cancer.df, minPreds = 10, nfolds = 5, criterion = "aic")
```

    ## [1] "Predictor Removed: binned_inc_ub  aic Value: 21345.801956885"
    ##  [1]  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
    ## [24] 25 26 27 28 29 31 32 33
    ## [1] "Predictor Removed: binned_inc_lb  aic Value: 21351.1182457793"
    ##  [1]  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
    ## [24] 25 26 27 28 31 32 33
    ## [1] "Predictor Removed: median_age_female  aic Value: 21371.1494668134"
    ##  [1]  2  3  4  5  6  7  8  9 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    ## [24] 26 27 28 31 32 33
    ## [1] "Predictor Removed: pct_married_households  aic Value: 21369.8533423642"
    ##  [1]  2  3  4  5  6  7  8  9 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    ## [24] 26 28 31 32 33
    ## [1] "Predictor Removed: pct_other_race  aic Value: 21412.5716556949"
    ##  [1]  2  3  4  5  6  7  8  9 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    ## [24] 28 31 32 33
    ## [1] "Predictor Removed: poverty_percent  aic Value: 21457.6464982638"
    ##  [1]  2  3  4  5  6  8  9 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 28
    ## [24] 31 32 33
    ## [1] "Predictor Removed: pct_black  aic Value: 21465.9291572823"
    ##  [1]  2  3  4  5  6  8  9 11 12 13 14 15 16 17 18 19 20 21 22 23 25 28 31
    ## [24] 32 33
    ## [1] "Predictor Removed: pct_white  aic Value: 21465.5128620098"
    ##  [1]  2  3  4  5  6  8  9 11 12 13 14 15 16 17 18 19 20 21 22 25 28 31 32
    ## [24] 33
    ## [1] "Predictor Removed: pct_public_coverage_alone  aic Value: 21473.0457948714"
    ##  [1]  2  3  4  5  6  8  9 11 12 13 14 15 16 17 18 19 20 21 25 28 31 32 33
    ## [1] "Predictor Removed: percent_married  aic Value: 21473.7174563066"
    ##  [1]  2  3  4  5  6  8  9 11 13 14 15 16 17 18 19 20 21 25 28 31 32 33
    ## [1] "Predictor Removed: pct_emp_priv_coverage  aic Value: 21472.3199933764"
    ##  [1]  2  3  4  5  6  8  9 11 13 14 15 16 17 18 19 21 25 28 31 32 33
    ## [1] "Predictor Removed: pct_private_coverage  aic Value: 21478.0257195945"
    ##  [1]  2  3  4  5  6  8  9 11 13 14 15 16 17 18 21 25 28 31 32 33
    ## [1] "Predictor Removed: avg_household_size  aic Value: 21507.2370005074"
    ##  [1]  2  3  4  5  6  8  9 13 14 15 16 17 18 21 25 28 31 32 33
    ## [1] "Predictor Removed: pct_bach_deg25_over  aic Value: 21504.8763028369"
    ##  [1]  2  3  4  5  6  8  9 13 14 15 16 18 21 25 28 31 32 33
    ## [1] "Predictor Removed: pct_unemployed16_over  aic Value: 21537.9022658737"
    ##  [1]  2  3  4  5  6  8  9 13 14 15 16 21 25 28 31 32 33
    ## [1] "Predictor Removed: pct_asian  aic Value: 21562.9789597308"
    ##  [1]  2  3  4  5  6  8  9 13 14 15 16 21 28 31 32 33
    ## [1] "Predictor Removed: pct_hs25_over  aic Value: 21564.3231204381"
    ##  [1]  2  3  4  5  6  8  9 13 14 15 21 28 31 32 33
    ## [1] "Predictor Removed: pop_est2015  aic Value: 21648.4548859685"
    ##  [1]  2  3  4  5  8  9 13 14 15 21 28 31 32 33
    ## [1] "Predictor Removed: pct_public_coverage  aic Value: 21658.4983277122"
    ##  [1]  2  3  4  5  8  9 13 14 15 28 31 32 33
    ## [1] "Predictor Removed: pct_no_hs18_24  aic Value: 21684.6339571827"
    ##  [1]  2  3  4  5  8  9 14 15 28 31 32 33
    ## [1] "Predictor Removed: med_income  aic Value: 21683.3458072713"
    ##  [1]  2  3  4  8  9 14 15 28 31 32 33
    ## [1] "Predictor Removed: incidence_rate  aic Value: 21803.566693602"
    ##  [1]  2  3  8  9 14 15 28 31 32 33
    ## [1] "Predictor Removed: median_age  aic Value: 22557.408308988"
    ## [1]  2  3  9 14 15 28 31 32 33

``` r
present.bs.result <- function(result.list, criterion) {
lm.bs.preds <- with(result.list, pred.list[[which.min(result.mat[,2])]])
lm.bs.mse <- with(result.list, result.mat[which.min(result.mat[,2]), 2])
print(sprintf("The best predictor set of %s predictors, out of a max of %s, (%s = %s)",
              length(lm.bs.preds), maxPreds, criterion, round(lm.bs.mse, 3)))
names(sub.cancer.df[,c(lm.bs.preds)])
}

present.bs.result(mse.result.list, "MSE")
```

    ## [1] "The best predictor set of 28 predictors, out of a max of 33, (MSE = 373.872)"

    ##  [1] "avg_ann_count"                  "avg_deaths_per_year"           
    ##  [3] "incidence_rate"                 "poverty_percent"               
    ##  [5] "median_age"                     "median_age_female"             
    ##  [7] "avg_household_size"             "percent_married"               
    ##  [9] "pct_no_hs18_24"                 "pct_hs18_24"                   
    ## [11] "pct_bach_deg18_24"              "pct_hs25_over"                 
    ## [13] "pct_unemployed16_over"          "pct_private_coverage"          
    ## [15] "pct_emp_priv_coverage"          "pct_public_coverage"           
    ## [17] "pct_public_coverage_alone"      "pct_white"                     
    ## [19] "pct_black"                      "pct_asian"                     
    ## [21] "pct_other_race"                 "pct_married_households"        
    ## [23] "birth_rate"                     "binned_inc_lb"                 
    ## [25] "binned_inc_ub"                  "study_quantile"                
    ## [27] "imp_pct_employed16_over"        "imp_pct_private_coverage_alone"

``` r
present.bs.result(aic.result.list, "AIC")
```

    ## [1] "The best predictor set of 31 predictors, out of a max of 33, (AIC = 21345.802)"

    ##  [1] "avg_ann_count"                  "avg_deaths_per_year"           
    ##  [3] "incidence_rate"                 "med_income"                    
    ##  [5] "pop_est2015"                    "poverty_percent"               
    ##  [7] "median_age"                     "median_age_male"               
    ##  [9] "median_age_female"              "avg_household_size"            
    ## [11] "percent_married"                "pct_no_hs18_24"                
    ## [13] "pct_hs18_24"                    "pct_bach_deg18_24"             
    ## [15] "pct_hs25_over"                  "pct_bach_deg25_over"           
    ## [17] "pct_unemployed16_over"          "pct_private_coverage"          
    ## [19] "pct_emp_priv_coverage"          "pct_public_coverage"           
    ## [21] "pct_public_coverage_alone"      "pct_white"                     
    ## [23] "pct_black"                      "pct_asian"                     
    ## [25] "pct_other_race"                 "pct_married_households"        
    ## [27] "birth_rate"                     "binned_inc_lb"                 
    ## [29] "study_quantile"                 "imp_pct_employed16_over"       
    ## [31] "imp_pct_private_coverage_alone"

``` r
#bs.lm <- lm(life_exp ~ ., data = sub.cancer.df[,c(lm.bs.preds,1)])
```

In general, backwards selection selects larger models than forawrds. I would suggest only using forward selection in this case to choose a smaller model. Or stepwise based on some other criterion.

Repeat to get a good feel for which preds get selected most often (note this will take a long time if the original p-set you are using is large, wouldnt use the full set for these iterations). This can be useful to get a feel for what you think might actually be the best of the 'best' subset selections (they very slightly from iteration to iteration by cross validation). Pick the number of iterations `len` and let the chunk run, you can see the process work (reccomend commenting out the print() calls in the functions above to reduce all the extra output), then at the end it will print all the subsets selected and you can sift through which variables you want for a final subset model. See which ones get selected most often basically.

*This will take a long time to run*

``` r
#Repeat if you want to
#Number of repetitions
len <- 10
b.result.list <- list()

#Repeat Subset Selection algorithm
for (i in 1:len) {
  b.result.list[[i]] <- b.subset(sub.cancer.df, minPreds = 30, nfolds = 5, "mse")
}

#View the results
for (i in 1:len) {
present.bs.result(b.result.list[[i]], "MSE")
}

#See which vars seem to be selected most often, make a subjective call what subset you like best.

#fs.lm <- lm(target_death_rate ~ ., data = cancer.df[,c(lm.fs.preds,1)])
```
