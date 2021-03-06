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
#percentage_NA = sapply(cancer_raw[1:34], function(x) sum(length(which(is.na(x)))) / nrow(cancer_raw))
#percentage_NA %>% data.frame()

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
  left_join(., state.df) %>%
  mutate(region = ifelse(is.na(region), 2, region),
         region = as.factor(region)) %>%
  dplyr::select(-c(binned_inc, geography, state, study_per_cap))

######TO avoid singular matrixes for our model fits, we need remove highly correlated and/or direct linear combos
###### Also lets make the response index 1, makes everything easier

cancer.df <- cancer.df %>%
  dplyr::select(-c(avg_deaths_per_year, avg_ann_count, pct_black, pct_asian, pct_other_race)) %>%
  dplyr::select(target_death_rate, study_quantile, region, everything())


#mostly need to decide between white/non-white or all race % AND binned_inc_lb, ub or median income
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
X <- train.df[ , -index] %>%
  names() %>% 
  paste(., collapse = "+") %>%    
  paste("~ ", .) %>%
  formula() %>%
  model.matrix(.,train.df)
  
#Create Design Matrix Test
X1 <- test.df[, -index] %>%
  names() %>% 
  paste(., collapse = "+") %>%    
  paste("~ ", .) %>%
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
nvmax <- ncol(cancer.df) - 1
reg.subsets <- cancer.df %>% 
  dplyr::select(-binned_inc_point) %>% #Remove linear dependency (average of lower, upper bound)
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

#Grab max r^2 pred number
max.r2 <- with(r.df, which.max(adjr2))

#Grab min bic pred number
min.bic <- with(r.df, which.min(bic))

cp.plot <- r.df %>% ggplot(aes(x = preds, y = cp, size = cp)) +
  geom_point(colour = "purple") +
  geom_line(alpha = 0.5, colour = "purple", size = 1.25) +
 # geom_point(aes(x = preds, step),color = "black",size = 0.5) +
  geom_line(aes(x = preds, step), size = 1, linetype = 2, color = "red") +
  labs(
    x = "Number of Preds",
    y = "CP Criterion",
    title = "Optimal Number of Preds, CP"
  ) + 
  ylim(c(0, 300)) + 
  theme(legend.position = "none") 

adjr2.plot <- r.df %>% ggplot(aes(x = preds, y = adjr2, size = 1 - adjr2)) +
  geom_point(colour = "purple") +
  geom_line(alpha = 0.5, colour = "purple", size = 1.25) +
  geom_vline(xintercept = 18, size = 1, linetype = 2, color = "red") +
  labs(
    x = "Number of Preds",
    y = "Adjusted R^2",
    title = "Optimal Number of Preds, Adj.R^2"
  ) + theme(legend.position = "none")

bic.plot <- r.df %>% ggplot(aes(x = preds, y = bic, size = bic)) +
  geom_point(colour = "purple") +
  geom_line(alpha = 0.5, colour = "purple", size = 1.25) +
  geom_vline(xintercept = min.bic, size = 1, linetype = 2, color = "red") +
  labs(
    x = "Number of Preds",
    y = "Bayesian Information Criterion",
    title = "Optimal Number of Preds, BIC"
  ) + theme(legend.position = "none")

(cp.plot + adjr2.plot) / bic.plot
```

    ## Warning: Removed 8 rows containing missing values (geom_point).

    ## Warning: Removed 8 rows containing missing values (geom_path).

<img src="Q_subset_selection_alg_files/figure-markdown_github/unnamed-chunk-5-1.png" width="90%" style="display: block; margin: auto;" />

Based on the plots above, *C**P* criterion in the upper left with *p* ≤ *C**P* constraint in red, that somewhere around a 25-27 predictor model ought to be optimal. With respect to adjusted *R*<sup>2</sup>, it appears as though we reach a converging maximum starting around 20 and negligible increase after, where additional predictors have diminishing marginal return. Lastly, BIC is more conservative (penalizing more strongly for more predictors) and seems to suggest between a 15-20 predictor model (closer to 20). This may inform our subset selection criterion.

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

#Example Function for Feature Selection
#map(.x = c(0.01, .1, 0.5, 1, 2), ~lasso.feature.removal(cancer.df, lambda = .x))
```

K-fold cross validation funciton for subset selecion (MSE, AIC, BIC criterion)
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

bicCV <- function(data.df, kfolds = 10){
  folds <- sample(1:kfolds, sampleSize, rep = T)
  bic <- rep(0, kfolds)
  for (k in 1:kfolds) {
    train.df <- data.df[folds != k,]
    test.df <- data.df[folds == k,]
    
    lm.mod <- lm(target_death_rate ~ ., data = train.df)
    bic[k] <- BIC(lm.mod)
  }
  mean(bic)
}
```

1. Linear Models
================

Scale continuous predictors

``` r
cancer.df <- bind_cols(
    cancer.df %>%
    dplyr::select(c(target_death_rate:region)),
    cancer.df %>%
    dplyr::select(-c(target_death_rate:region)) %>%
    scale() %>% as.tibble()
)
```

Forward Subset Selection with k-Fold CV and MSE criterion
---------------------------------------------------------

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
#set.seed(4) #Comment this out if you want to really get a different subset each time

#Function
#inputs:
#full data frame (whatever that ends up being)
#maximum number you want to look at subsets up to (no larger than p)
#Number of folds for the cross validation step, 10 is a little slow but better for final paper
#criterion to minimize -  either "mse", or "aic". Must put those in just as they appear with "

f.subset <- function(sub.cancer.df, maxPreds = 10, nfolds = 5, criterion = "mse") {

#number of possible predictors
## Allthe predictors (their indices).
allPreds <- 1:ncol(sub.cancer.df)
allPreds <- allPreds[-1] #substract the response

#current set of preds (starts empty), and available
currPreds <- c()
availPreds <- setdiff(allPreds,currPreds)
#Record the min errors
minError <- c()
#Initalize pred list and mse result matrix (just for storage)
pred.list <- list()
result.mat <- matrix(nrow = ncol(cancer.df) - 1, ncol = 2)
#Initalize iteration
i <- 1
#Forward selection loop
while (length(currPreds) < maxPreds) {
  ##add predictor which decreases MSE (as determined by CV or
  ##Bootstrapping)
  ## The MSE/AIC computed as we add each of the available predictors
  allError <- c()
  for (id in availPreds) {
    data.df <- sub.cancer.df[,c(currPreds,id,1)] #Only the preds we want in the df
    if (criterion == "mse") {
      error <- mseCV(data.df, nfolds)  #cross validate mse for the model with the added pred
    } else if (criterion == "aic") {
      error <- aicCV(data.df, nfolds) #same but with AIC
    } else if (criterion == "bic") {
      error <- bicCV(data.df, nfolds) #same bic
    } else {
      print("Invalid criterion")
      stop("Wrong criterion input")
    }
    allError <- c(allError,error)  #Collect all the errors for every variable added
  }
  ##Find the min
  id <- which.min(allError)
  ##get the best predictor and lowest MSE/AIC
  bestPred <- availPreds[id]
  bestError <- min(allError)
  ##Add these into the collection
  currPreds <- c(currPreds,bestPred)
  minError <- c(minError,bestError)
  ##Take out the pred you just added from consideration
  availPreds <- setdiff(allPreds,currPreds)
  ## Print stuff out for debugging and attention-grabbing
  #print(sprintf("Iteration: %i Predictor Added: %s %s Value: %s",i, names(sub.cancer.df[,bestPred]), criterion, bestError))
  #print(currPreds)
  result.mat[i,] <- c(bestPred,bestError)         #You can also comment out all print() later, it's just to watch the algorithm work
  pred.list[[i]] <- currPreds
  i <- i + 1
    }
  return(list(pred.list = pred.list, result.mat = result.mat, maxPreds = maxPreds)) #returns list of preds, results, and max num preds
}

#Run Subset, call function, output is a list with predictor sets, reslut matrix and maxPreds
f.mse.list <- f.subset(cancer.df, maxPreds = ncol(cancer.df) - 1, nfolds = 5, criterion = "mse")
f.aic.list <- f.subset(cancer.df, maxPreds = ncol(cancer.df) - 1, nfolds = 5, criterion = "aic")
f.bic.list <- f.subset(cancer.df, maxPreds = ncol(cancer.df) - 1, nfolds = 5, criterion = "bic")

#Show the 'best' final selection with minimal MSE (takes in a list object from the previous function, with criterion (MSE/AIC))
present.fs.result <- function(f.result.list, criterion) {
lm.fs.preds <- with(f.result.list, pred.list[[which.min(result.mat[,2])]]) #Pick out indices from best model
fs.mse <- with(f.result.list, result.mat[which.min(result.mat[,2]), 2])
print(sprintf("The best predictor set of %s predictors, out of a max of %s, (%s = %s)",
              length(lm.fs.preds), f.result.list$maxPreds, criterion, round(fs.mse, 3)))
print(names(cancer.df[,c(lm.fs.preds)]))
}

#Show the final results and subset selections, with CV criterion stats
present.fs.result(f.mse.list, "MSE")
```

    ## [1] "The best predictor set of 26 predictors, out of a max of 32, (MSE = 156.823)"
    ##  [1] "avg_deaths_yr_pop"         "median_age_female"        
    ##  [3] "region"                    "incidence_rate"           
    ##  [5] "pct_unemployed16_over"     "pct_hs18_24"              
    ##  [7] "pct_emp_priv_coverage"     "pct_private_coverage"     
    ##  [9] "binned_inc_point"          "median_age_male"          
    ## [11] "avg_ann_count_pop"         "pct_hs25_over"            
    ## [13] "pct_no_hs18_24"            "pct_white"                
    ## [15] "percent_married"           "binned_inc_lb"            
    ## [17] "pct_non_white"             "study_quantile"           
    ## [19] "med_income"                "imp_pct_employed16_over"  
    ## [21] "pct_public_coverage"       "pct_public_coverage_alone"
    ## [23] "birth_rate"                "pct_bach_deg25_over"      
    ## [25] "pop_est2015"               "pct_bach_deg18_24"

``` r
present.fs.result(f.aic.list, "AIC")
```

    ## [1] "The best predictor set of 24 predictors, out of a max of 32, (AIC = 19214.213)"
    ##  [1] "avg_deaths_yr_pop"              "median_age_female"             
    ##  [3] "region"                         "incidence_rate"                
    ##  [5] "pct_unemployed16_over"          "pct_hs18_24"                   
    ##  [7] "pct_emp_priv_coverage"          "pct_private_coverage"          
    ##  [9] "binned_inc_point"               "median_age_male"               
    ## [11] "pct_hs25_over"                  "avg_ann_count_pop"             
    ## [13] "pct_public_coverage"            "pct_public_coverage_alone"     
    ## [15] "imp_pct_employed16_over"        "pct_bach_deg25_over"           
    ## [17] "birth_rate"                     "percent_married"               
    ## [19] "med_income"                     "pct_non_white"                 
    ## [21] "pct_white"                      "imp_pct_private_coverage_alone"
    ## [23] "pop_est2015"                    "pct_married_households"

``` r
present.fs.result(f.bic.list, "BIC")
```

    ## [1] "The best predictor set of 22 predictors, out of a max of 32, (BIC = 19365.293)"
    ##  [1] "avg_deaths_yr_pop"         "median_age_female"        
    ##  [3] "region"                    "incidence_rate"           
    ##  [5] "pct_unemployed16_over"     "pct_hs18_24"              
    ##  [7] "pct_emp_priv_coverage"     "pct_private_coverage"     
    ##  [9] "binned_inc_ub"             "median_age_male"          
    ## [11] "pct_hs25_over"             "avg_ann_count_pop"        
    ## [13] "pct_public_coverage"       "pct_public_coverage_alone"
    ## [15] "imp_pct_employed16_over"   "med_income"               
    ## [17] "birth_rate"                "percent_married"          
    ## [19] "pct_bach_deg25_over"       "pct_non_white"            
    ## [21] "pct_white"                 "pct_no_hs18_24"

``` r
#Pull the indices for the selected models, for later comparison
fs.mse.preds <- with(f.mse.list, pred.list[[which.min(result.mat[,2])]])
fs.aic.preds <- with(f.aic.list, pred.list[[which.min(result.mat[,2])]])
fs.bic.preds <- with(f.bic.list,  pred.list[[which.min(result.mat[,2])]])
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
#Backward Selection
#set.seed(4444)  #Set seed for reproducibility
###############################################

#Num all possible preds
maxPreds <- ncol(cancer.df - 1)

#Function
#Inputs:
#full data set (whatever that ends up being, ideally non-singular)
#minimum number of preds you want the algorithm to reduce subsets to
#Number of folds for CV step
#Criterion to minimize: "mse" or "aic"

b.subset <- function(sub.cancer.df, minPreds = 1, nfolds = 5, criterion = "mse"){

## Allthe predictors (their indices).
allPreds <- 1:ncol(sub.cancer.df)
allPreds <- allPreds[-1] #take out response

#current set of preds (starts full), and available
currPreds <- allPreds
availPreds <- allPreds
#Record the min errors
minError <- c()
#Set up storage objects
pred.list <- list()
result.mat <- matrix(nrow = ncol(sub.cancer.df) - 1, ncol = 2)
#initialize iteration
i <- 1
#Forward selection loop
while (length(currPreds) >= minPreds) {
  ##remove predictor which has CV lm with lowest MSE/AIC (as determined by CV)
  ## The MSE/AIC computed as we remove of the available predictors
  allError <- c()
  for (id in availPreds) {
    data.df <- sub.cancer.df[,c(currPreds[-id],1)]  #Take data frame without the predictor were removing
    if (criterion == "mse") {
      error <- mseCV(data.df, nfolds)  #calculate CV mse
    } else if (criterion == "aic") {
      error <- aicCV(data.df, nfolds)  #calculate CV aic
    } else if (criterion == "bic") {
      error <- bicCV(data.df, nfolds)  # CV bic
    } else {
      print("Invalid criterion")
      stop("Wrong criterion input")
    }
    allError <- c(allError,error)  #Collect all our criterions (error is misleading, can be mse or aic)
  }
  ##Find the min
  id <- which.min(allError)
  ##get the worst predictor and MSE
  worstPred <- availPreds[id]
  bestError <- min(allError)
  ##Remove these from the collection
  currPreds <- currPreds[-id]
  minError <- c(minError, bestError)
  availPreds <- currPreds
  ## Print stuff out for debugging and attention-grabbing
  #print(sprintf("Predictor Removed: %s  %s Value: %s",names(sub.cancer.df[,worstPred]), criterion, bestError))
  #print(currPreds)
  result.mat[i,] <- c(worstPred,bestError)   #All print() can be commented out at any time, just to visualize alg. process
  pred.list[[i]] <- currPreds
  
  i <- i + 1
  }
  list(pred.list = pred.list, result.mat = result.mat)
}

#Call functions, output is a list of preds and a result matrix of criterion and predictor removed
b.mse.list <- b.subset(cancer.df, minPreds = 10, nfolds = 5, criterion = "mse")
b.aic.list <- b.subset(cancer.df, minPreds = 10, nfolds = 5, criterion = "aic")
b.bic.list <- b.subset(cancer.df, minPreds = 10, nfolds = 5, criterion = "bic")

#Visualize function takes in a list object from the last function
present.bs.result <- function(result.list, criterion) {
lm.bs.preds <- with(result.list, pred.list[[which.min(result.mat[,2])]])
lm.bs.mse <- with(result.list, result.mat[which.min(result.mat[,2]), 2])
print(sprintf("The best predictor set of %s predictors, out of a max of %s, (%s = %s)",
              length(lm.bs.preds), maxPreds, criterion, round(lm.bs.mse, 3)))
names(cancer.df[,c(lm.bs.preds)])
}

#Present the final subsets which were selected
present.bs.result(b.mse.list, "MSE")
```

    ## [1] "The best predictor set of 31 predictors, out of a max of 33, (MSE = 157.084)"

    ##  [1] "study_quantile"                 "region"                        
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
    ## [23] "pct_married_households"         "birth_rate"                    
    ## [25] "pct_non_white"                  "binned_inc_lb"                 
    ## [27] "binned_inc_ub"                  "binned_inc_point"              
    ## [29] "avg_deaths_yr_pop"              "avg_ann_count_pop"             
    ## [31] "imp_pct_private_coverage_alone"

``` r
present.bs.result(b.aic.list, "AIC")
```

    ## [1] "The best predictor set of 31 predictors, out of a max of 33, (AIC = 19221.184)"

    ##  [1] "study_quantile"                 "region"                        
    ##  [3] "incidence_rate"                 "med_income"                    
    ##  [5] "pop_est2015"                    "poverty_percent"               
    ##  [7] "median_age"                     "median_age_male"               
    ##  [9] "avg_household_size"             "percent_married"               
    ## [11] "pct_no_hs18_24"                 "pct_hs18_24"                   
    ## [13] "pct_bach_deg18_24"              "pct_hs25_over"                 
    ## [15] "pct_bach_deg25_over"            "pct_unemployed16_over"         
    ## [17] "pct_private_coverage"           "pct_emp_priv_coverage"         
    ## [19] "pct_public_coverage"            "pct_public_coverage_alone"     
    ## [21] "pct_white"                      "pct_married_households"        
    ## [23] "birth_rate"                     "pct_non_white"                 
    ## [25] "binned_inc_lb"                  "binned_inc_ub"                 
    ## [27] "binned_inc_point"               "avg_deaths_yr_pop"             
    ## [29] "avg_ann_count_pop"              "imp_pct_employed16_over"       
    ## [31] "imp_pct_private_coverage_alone"

``` r
present.bs.result(b.bic.list, "BIC")
```

    ## [1] "The best predictor set of 31 predictors, out of a max of 33, (BIC = 19435.908)"

    ##  [1] "study_quantile"                 "region"                        
    ##  [3] "incidence_rate"                 "med_income"                    
    ##  [5] "pop_est2015"                    "poverty_percent"               
    ##  [7] "median_age"                     "median_age_male"               
    ##  [9] "median_age_female"              "avg_household_size"            
    ## [11] "percent_married"                "pct_no_hs18_24"                
    ## [13] "pct_hs18_24"                    "pct_bach_deg18_24"             
    ## [15] "pct_hs25_over"                  "pct_bach_deg25_over"           
    ## [17] "pct_private_coverage"           "pct_emp_priv_coverage"         
    ## [19] "pct_public_coverage"            "pct_public_coverage_alone"     
    ## [21] "pct_white"                      "pct_married_households"        
    ## [23] "birth_rate"                     "pct_non_white"                 
    ## [25] "binned_inc_lb"                  "binned_inc_ub"                 
    ## [27] "binned_inc_point"               "avg_deaths_yr_pop"             
    ## [29] "avg_ann_count_pop"              "imp_pct_employed16_over"       
    ## [31] "imp_pct_private_coverage_alone"

``` r
#Pull the indices for the selected models, for later comparison
bs.mse.preds <- with(b.mse.list, pred.list[[which.min(result.mat[,2])]])
bs.aic.preds <- with(b.aic.list, pred.list[[which.min(result.mat[,2])]])
bs.bic.preds <- with(b.bic.list,  pred.list[[which.min(result.mat[,2])]])
```

In general, backwards selection selects larger models than forawrds. I would suggest only using forward selection in this case to choose a smaller model. Or stepwise based on some other criterion.

Repeat to get a good feel for which preds get selected most often (note this will take a long time if the original p-set you are using is large, wouldnt use the full set for these iterations). This can be useful to get a feel for what you think might actually be the best of the 'best' subset selections (they very slightly from iteration to iteration by cross validation). Pick the number of iterations `len` and let the chunk run, you can see the process work (reccomend commenting out the print() calls in the functions above to reduce all the extra output), then at the end it will print all the subsets selected and you can sift through which variables you want for a final subset model. See which ones get selected most often basically.

Ridge and Lasso Functions for Comparison
========================================

``` r
#Initialize Sample Size
sampleSize <- nrow(cancer.df)

#Initialize lambda grids
lambda.grid.lasso <- 10^seq(-1,0.5,length = 100)
lambda.grid.ridge <- 10^seq(-1,0.5,length = 100)


#Lambda optimization Ridge
ridge.opt <- function(data, grid = lambda.grid, response = "target_death_rate"){
    #Snag the index for the matrices  
  index <- which(names(data) == response)
  
  #Response 
  Y <- as.matrix(data[ ,index])
  
  #Design matrix
  X <- data[ ,-index] %>%
    names() %>% 
    paste(., collapse = "+") %>%    
    paste("~ ", .) %>%
    formula() %>%
    model.matrix(., data)
  X <- X[,-1]
  
  #Lambda optimize
  cv.ridge <- cv.glmnet(X,Y,alpha = 0,intercept = T, lambda = grid, family = "gaussian")
  cv.ridge$lambda.min
}

#lambda.opt.ridge <- ridge.opt(cancer.df, grid = lambda.grid)

#Lambda optimization Lasso
lasso.opt <- function(data, grid = lambda.grid, response = "target_death_rate"){  
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
  
  #Optimize Lambda
  cv.lasso <- cv.glmnet(X,Y,alpha = 1,intercept = T,lambda = grid, family = "gaussian")
  cv.lasso$lambda.min
}

#lambda.opt.lasso <- lasso.opt(cancer.df, grid = lambda.grid.lasso)

ridge <- function(data, response = "target_death_rate") {

  #Snag the index for the matrices  
  index <- which(names(data) == response)
  
  #Response 
  Y <- as.matrix(data[ ,index])
  
  #Design matrix
  X <- data[ ,-index] %>%
    names() %>% 
    paste(., collapse = "+") %>%    
    paste("~ ", .) %>%
    formula() %>%
    model.matrix(., data)
  X <- X[,-1]
  
  #Lambda optimize
  cv.ridge <- cv.glmnet(X,Y,alpha = 0,intercept = T, lambda = lambda.grid.ridge, family = "gaussian")
  lambda.opt.ridge <- cv.ridge$lambda.min
  
  #Return model
  return(glmnet(X,Y,alpha = 0,intercept = T,lambda = lambda.opt.ridge, family = "gaussian"))
}

lasso <- function(data, response = "target_death_rate", print = FALSE) {
  
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
  
  #Optimize Lambda
  cv.lasso <- cv.glmnet(X,Y,alpha = 1,intercept = T,lambda = lambda.grid.lasso, family = "gaussian")
  lambda.opt.lasso <- cv.lasso$lambda.min
  mod.lasso <- glmnet(X,Y,alpha = 1,intercept = T,lambda = lambda.opt.lasso, family = "gaussian")
  #Print if true
  if (isTRUE(print)) {
  coef.lasso <- coef(mod.lasso)[,1]
    print("The Lasso method selected the variables:")
    print(paste(names(which(coef.lasso != 0))))
  }
  return(mod.lasso)
}

glmnet.predict <- function(model, data, response = "target_death_rate") {
  #Snag the index for matrices
  index <- which(names(data) == response)
  #Design matrix
  X1 <- data[ ,-index] %>%
    names() %>% 
    paste(., collapse = "+") %>%    
    paste("~ ", .) %>%
    formula() %>%
    model.matrix(., data)
  X1 <- X1[,-1] 
  return(predict(model, newx = X1, type = "response"))
}

glmnet.rmse <- function(model, data, response = "target_death_rate") {
  #Snag the index for matrices
  index <- which(names(data) == response)
  #Design matrix
  X1 <- data[ ,-index] %>%
    names() %>% 
    paste(., collapse = "+") %>%    
    paste("~ ", .) %>%
    formula() %>%
    model.matrix(., data)
  X1 <- X1[,-1] 
  preds <- predict(model, newx = X1, type = "response")
  return(with(data, sqrt(mean((target_death_rate - preds)^2))))
}

glmnet.mse <- function(model, data, response = "target_death_rate") {
  #Snag the index for matrices
  index <- which(names(data) == response)
  #Design matrix
  X1 <- data[ ,-index] %>%
    names() %>% 
    paste(., collapse = "+") %>%    
    paste("~ ", .) %>%
    formula() %>%
    model.matrix(., data)
  X1 <- X1[,-1] 
  preds <- predict(model, newx = X1, type = "response")
  return(with(data, mean((target_death_rate - preds)^2)))
}

lassoCV <- function(data.df = cancer.df, kfolds = 10, response = "target_death_rate"){
  index <- which(names(data.df) == response) #Grab response index for matrices
  folds <- sample(1:kfolds, sampleSize, rep = T) #Randomly sample indeices for kfolds
  error <- rep(0, kfolds) #initialize error vector
  for (k in 1:kfolds) {
    train.df <- data.df[folds != k,] #Split into test train
    test.df <- data.df[folds == k,]
    X1 <- test.df[ , -index] %>%
          names() %>% 
          paste(., collapse = "+") %>%    
          paste("~ ", .) %>%
          formula() %>%
          model.matrix(.,test.df)
    X1 <- X1[,-1]
     
    lasso.mod <- lasso(train.df, response)
       preds <- predict(lasso.mod, newx = X1, type = "response")
    error[k] <- with(test.df, mean((target_death_rate - preds)^2))
  }
  mean(error) #return kfold mean mse
}

ridgeCV <- function(data.df, kfolds = 10, response){
  index <- which(names(data.df) == response) #Grab response index for matrices
  folds <- sample(1:kfolds, sampleSize, rep = T) #Randomly sample indeices for kfolds
  error <- rep(0, kfolds) #initialize error vector
  for (k in 1:kfolds) {
    train.df <- data.df[folds != k,] #Split into test train
     test.df <- data.df[folds == k,]
    #Design test matrix
  X1 <- test.df[ , -index] %>%
  names() %>% 
  paste(., collapse = "+") %>%    
  paste("~ ", .) %>%
  formula() %>%
  model.matrix(.,test.df)
  #Remove Intercept
   X1 <- X1[ ,-1] 
  #Fit model, predict on kth fold, record mse
   ridge.mod <- ridge(train.df, response)
       preds <- predict(ridge.mod, newx = X1, type = "response")
    error[k] <- with(test.df, mean((target_death_rate - preds)^2))
  }
  mean(error) #return kfold mean mse
}

#Test
#lasso1 <- cancer.df %>% lasso(., print = TRUE)

#coef(lasso1)

#test - lasso, ridge - all good
#ridge(data = cancer.df)
#lasso(data = cancer.df, print = TRUE)

#test - CV - all good
lassoCV(cancer.df, 10, "target_death_rate")
```

    ## [1] 157.6378

``` r
ridgeCV(cancer.df, 10, "target_death_rate")
```

    ## [1] 157.9534

Model Comparison with Iterative 10-Fold CV
==========================================

Takes a bit to run.

``` r
#Number of CV iterations
nrep <- 100

#make sure target_death_rate is the first index in the data set (just way easier that way) District of columbia is a problem
#State is a problem, need to make a region variable
library(modelr)
cv.df <- cancer.df %>%
  mutate(median_age_all_gender = (median_age_male + median_age_female)/2) %>% 
  mutate(south = ifelse(region == "South", 1, 0)) %>%
         crossv_mc(., nrep) %>%
  mutate(train = map(train, as.tibble),
         test = map(test, as.tibble)) %>%
  mutate(fs.mse.lm = map(train, ~lm(target_death_rate ~ ., data = .x[, c(fs.mse.preds, 1)])),
         fs.aic.lm = map(train, ~lm(target_death_rate ~ ., data = .x[, c(fs.aic.preds, 1)])),
         fs.bic.lm = map(train, ~lm(target_death_rate ~ ., data = .x[, c(fs.bic.preds, 1)])),
         bs.mse.lm = map(train, ~lm(target_death_rate ~ ., data = .x[, c(bs.mse.preds, 1)])),
         bs.aic.lm = map(train, ~lm(target_death_rate ~ ., data = .x[, c(bs.aic.preds, 1)])),
         bs.bic.lm = map(train, ~lm(target_death_rate ~ ., data = .x[, c(bs.bic.preds, 1)])),
         lasso.lm  = map(train, ~lasso(data = .x)),
         ridge.lm  = map(train, ~ridge(data = .x)),
         auto.back  = map(train, ~lm(target_death_rate ~ pct_white + pct_hs25_over + 
          imp_pct_employed16_over + pct_private_coverage + pct_public_coverage + 
          avg_ann_count_pop + avg_deaths_yr_pop + med_income + study_quantile + 
          median_age_all_gender + south, data = .x)),
         auto.front  = map(train, ~lm(target_death_rate ~ pct_white + pct_hs25_over +
          imp_pct_employed16_over + pct_private_coverage + pct_public_coverage + 
          avg_ann_count_pop + avg_deaths_yr_pop + med_income + study_quantile +
          median_age_all_gender + south + avg_household_size, data = .x))) %>%
  mutate(fs.mse.mse = map2_dbl(fs.mse.lm, test, ~rmse(model = .x, data = .y)),
         fs.aic.mse = map2_dbl(fs.aic.lm, test, ~rmse(model = .x, data = .y)),
         fs.bic.mse = map2_dbl(fs.bic.lm, test, ~rmse(model = .x, data = .y)),
         bs.mse.mse = map2_dbl(bs.mse.lm, test, ~rmse(model = .x, data = .y)),
         bs.aic.mse = map2_dbl(bs.aic.lm, test, ~rmse(model = .x, data = .y)),
         bs.bic.mse = map2_dbl(bs.bic.lm, test, ~rmse(model = .x, data = .y)),
         lasso.mse  = map2_dbl( lasso.lm, test, ~glmnet.rmse(model = .x, data = .y)),
         ridge.mse  = map2_dbl( lasso.lm, test, ~glmnet.rmse(model = .x, data = .y)),
         auto.front.mse = map2_dbl(auto.back, test, ~rmse(model = .x, data = .y)),
         auto.back.mse = map2_dbl(auto.front, test, ~rmse(model = .x, data = .y)))
```

Visualize Model Comparison
==========================

``` r
#Violin box plots
violin.mse <- cv.df %>% 
  dplyr::select(ends_with(".mse")) %>% 
  gather(key = model, value = mse) %>% 
  mutate(model = str_replace(model, "mse", ""),
         model = fct_reorder(model, mse, .desc = FALSE, .fun = mean)) %>% 
  ggplot(aes(x = model, y = mse)) + 
  geom_violin(aes(fill = model), trim = FALSE, alpha = 0.3) + 
  geom_boxplot(width = 0.25) +
  labs(
    y = "CV Root Mean Sqaure Error",
    x = "Model",
    title = sprintf("Model RMSE Comparison by 10-Fold CV: %i Iterations", nrep)
  ) +
  viridis::scale_fill_viridis(
    option = "magma",
    name = "MSE",
    begin = 1,
    end = 0,
    discrete = TRUE) 

density.mse <- cv.df %>%
   dplyr::select(ends_with(".mse")) %>% 
  gather(key = model, value = mse) %>% 
  mutate(model = str_replace(model, ".mse", ""),
         model = fct_reorder(model, mse, .desc = FALSE, .fun = mean)) %>% 
  ggplot(aes(x = mse, colour = model, fill = model)) + 
  geom_density(position = "stack", alpha = 0.3) + 
  #geom_boxplot(width = 0.25) +
  labs(
    y = "Density",
    x = "CV Root Mean Square Error",
    title = sprintf("Model RMSE Comparison by 10-Fold CV: %i Iterations", nrep)
  ) +
  viridis::scale_fill_viridis(
    option = "magma",
    name = "MSE",
    begin = 1,
    end = 0,
    discrete = TRUE) +
    viridis::scale_colour_viridis(
    option = "magma",
    name = "MSE",
    begin = 1,
    end = 0,
    discrete = TRUE)
```

<img src="Q_subset_selection_alg_files/figure-markdown_github/unnamed-chunk-15-1.png" width="90%" style="display: block; margin: auto;" /><img src="Q_subset_selection_alg_files/figure-markdown_github/unnamed-chunk-15-2.png" width="90%" style="display: block; margin: auto;" />

Aggregated Ensemble of Linear Models
------------------------------------

The alternative stepwise, p-value, and `library(leaps)` selected models consistently underperform at validation, and will be removed from consideration. Additonially, backwards `lm` selection by any criterion appears to be performing poorly after multiple 100 fold observations, and likewise will be removed from consideration. We will now create an ensemble linear model averaging the predictions of the remaining forward selected linear models to form our final `target_death_rate` predictions.

After further investigation, we think that removing the forward selected mse model will also be advantagous for our ensemble.

Additionally, since forward AIC has so been so dominant, and forward BIC seems to more often be slightly better than lasso, and ridge always being slightly worse; we implement the following weighting scheme `w = c(1.1, 1.05, 0.95, 0.9)`; corresponding to AIC, BIC, Lasso, and Ridge respectively.

``` r
#Set up ensemble Functions
lm.ensemble <- function(data) {
     list(
         fs.aic.lm = lm(target_death_rate ~ ., data = data[, c(fs.aic.preds, 1)]),
         fs.bic.lm = lm(target_death_rate ~ ., data = data[, c(fs.bic.preds, 1)]),
         lasso.lm  = lasso(data = data),
         ridge.lm  = ridge(data = data)
     )
}




predict.lm.ensemble <- function(model.list, data) {
  pred.mat <- matrix(ncol = length(model.list), nrow = nrow(data))
  for (i in 1:length(model.list)) {
     ifelse(i <= 2, pred.mat[ ,i] <- predict(model.list[[i]], newdata = data, type = "response"),
                    pred.mat[ ,i] <- glmnet.predict(model.list[[i]], data = data))
  }
  apply(pred.mat, 1, weighted.mean, w = c(1.5, 1, 0.8, 0.7)/4)
}

rmse.lm.ensemble <- function(model.list, data) {
  with(data, sqrt(mean((target_death_rate - predict.lm.ensemble(model.list, data))^2)))
}

mse.lm.ensemble <- function(model.list, data) {
  with(data, mean((target_death_rate - predict.lm.ensemble(model.list, data))^2))
}

#rmse.lm.ensemble(lm.ensemble(cancer.df), cancer.df)

#Re-grid lambdas
lambda.grid.lasso <- 10^seq(-1.25, 0.5,length = 200)
lambda.grid.ridge <- 10^seq(-1.25, 0.5,length = 200)

nrep <- 100
cv.df <- cancer.df %>%
         crossv_mc(., nrep) %>%
  mutate(train = map(train, as.tibble),
         test = map(test, as.tibble)) %>%
  mutate(fs.aic.lm = map(train, ~lm(target_death_rate ~ ., data = .x[, c(fs.aic.preds, 1)])),
         fs.bic.lm = map(train, ~lm(target_death_rate ~ ., data = .x[, c(fs.bic.preds, 1)])),
         lasso.lm  = map(train, ~lasso(data = .x)),
         ridge.lm  = map(train, ~ridge(data = .x)),
         ensemble.lm = map(train, ~lm.ensemble(data = .x))) %>%
  mutate(fs.aic.mse = map2_dbl(fs.aic.lm, test, ~rmse(model = .x, data = .y)),
         fs.bic.mse = map2_dbl(fs.bic.lm, test, ~rmse(model = .x, data = .y)),
         lasso.mse  = map2_dbl(lasso.lm, test, ~glmnet.rmse(model = .x, data = .y)),
         ridge.mse  = map2_dbl(lasso.lm, test, ~glmnet.rmse(model = .x, data = .y)),
         ensemble.mse = map2_dbl(ensemble.lm, test, ~rmse.lm.ensemble(model.list = .x, data = .y)))
```

Visualize Model Comparison
==========================

``` r
#Violin box plots
violin.mse <- cv.df %>% 
  dplyr::select(ends_with(".mse")) %>% 
  gather(key = model, value = mse) %>% 
  mutate(model = str_replace(model, "mse", ""),
         model = fct_reorder(model, mse, .desc = FALSE, .fun = mean)) %>% 
  ggplot(aes(x = model, y = mse)) + 
  geom_violin(aes(fill = model), trim = FALSE, alpha = 0.3) + 
  geom_boxplot(width = 0.25) +
  labs(
    y = "CV Root Mean Sqaure Error",
    x = "Model",
    title = sprintf("Model RMSE Comparison by 10-Fold CV: %i Iterations", nrep)
  ) +
  viridis::scale_fill_viridis(
    option = "magma",
    name = "MSE",
    begin = 1,
    end = 0,
    discrete = TRUE) 

density.mse <- cv.df %>%
   dplyr::select(ends_with(".mse")) %>% 
  gather(key = model, value = mse) %>% 
  mutate(model = str_replace(model, ".mse", ""),
         model = fct_reorder(model, mse, .desc = FALSE, .fun = mean)) %>% 
  ggplot(aes(x = mse, colour = model, fill = model)) + 
  geom_density(position = "stack", alpha = 0.3) + 
  #geom_boxplot(width = 0.25) +
  labs(
    y = "Density",
    x = "CV Root Mean Square Error",
    title = sprintf("Model RMSE Comparison by 10-Fold CV: %i Iterations", nrep)
  ) +
  viridis::scale_fill_viridis(
    option = "magma",
    name = "MSE",
    begin = 1,
    end = 0,
    discrete = TRUE) +
    viridis::scale_colour_viridis(
    option = "magma",
    name = "MSE",
    begin = 1,
    end = 0,
    discrete = TRUE)
```

<img src="Q_subset_selection_alg_files/figure-markdown_github/unnamed-chunk-18-1.png" width="90%" style="display: block; margin: auto;" /><img src="Q_subset_selection_alg_files/figure-markdown_github/unnamed-chunk-18-2.png" width="90%" style="display: block; margin: auto;" />

Final Test, LOOCV with PRESS Criterion
======================================

\*no clue how long this is going to take to run

``` r
#Re-grid lambdas
lambda.grid.lasso <- 10^seq(-1.25, 0.5,length = 100)
lambda.grid.ridge <- 10^seq(-1.25, 0.5,length = 100)

a <- Sys.time()
#LOOCV
#nrep <- 1
loocv.df <- cancer.df %>%
         crossv_kfold(., k = nrow(cancer.df)) %>%
  mutate(train = map(train, as.tibble),
         test = map(test, as.tibble)) %>%
  mutate(fs.aic.lm = map(train, ~lm(target_death_rate ~ ., data = .x[, c(fs.aic.preds, 1)])),
         fs.bic.lm = map(train, ~lm(target_death_rate ~ ., data = .x[, c(fs.bic.preds, 1)])),
         lasso.lm  = map(train, ~lasso(data = .x)),
         ridge.lm  = map(train, ~ridge(data = .x)),
         ensemble.lm = map(train, ~lm.ensemble(data = .x))) %>%
  mutate(fs.aic.mse = map2_dbl(fs.aic.lm, test, ~mse(model = .x, data = .y)),
         fs.bic.mse = map2_dbl(fs.bic.lm, test, ~mse(model = .x, data = .y)),
         lasso.mse  = map2_dbl(lasso.lm, test, ~glmnet.mse(model = .x, data = .y)),
         ridge.mse  = map2_dbl(lasso.lm, test, ~glmnet.mse(model = .x, data = .y)),
         ensemble.mse = map2_dbl(ensemble.lm, test, ~mse.lm.ensemble(model.list = .x, data = .y)))
b <- Sys.time()
(c <- b - a)

loocv.df$fs.aic.mse

unscaled.cancer.df <- bind_cols(
    cancer.df %>%
    dplyr::select(c(target_death_rate:region)),
    cancer.df %>%
    dplyr::select(-c(target_death_rate:region)) %>%
    DMwR::unscale(vals = ., norm.data = .) %>% as.tibble()
)


#Unscale for Diagnostic Assumptions
unscaled.cancer.df <- cancer.df

#
for(i in 1:29) {
  unscaled.cancer.df[ ,i + 3] <- unscaled.cancer.df[ ,i + 3]*attr(cancer.df$incidence_rate, "scaled:scale")[i] + attr(cancer.df$incidence_rate, "scaled:center")[i]
}


#Unscale diagnostic plots
#par( mfrow = c(2,2))
plot(lm(target_death_rate ~ ., data = unscaled.cancer.df[, c(fs.aic.preds, 1)]))

#Scaled Diagnostic, unappreciably different
plot(lm(target_death_rate ~ ., data = cancer.df[, c(fs.aic.preds, 1)]))
```
