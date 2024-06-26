## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Script name: 05B_validateCV.R
##
## Purpose of script: To validate the determList through cross-validation.
##
## Author: Cal Chengqi Fang
##
## Date Created: 2024-06-07
##
## Copyright (c) Cal Chengqi Fang, 2024
## Email: cal.cf@uchicago.edu
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Notes:
##   
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

## set working directory for Mac and PC
setwd("/Users/atchoo/Documents/GitHub/Name-Method-Project/")     # Cal's working directory (mac)
# setwd("C:/Users/")     # Cal's working directory (PC)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls())
options(scipen=6, digits=4)         # I prefer to view outputs in non-scientific notation
memory.limit(30000000)                  # this is needed on some PCs to increase memory allowance, but has no impact on macs.

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

## load up the packages we will need:  (uncomment as required)
require(tidyverse)
require(data.table)
require(Matrix)
require(caret)        # For the confusion matrix estimation function
require(foreach)      # For the parallel computing 
require(doParallel)   # For the parallel computing
require(parallel)     # For the parallel computing

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ----------------------------------------------- STEP 1 ----------------------------------------------- 
# Load the cleaned data
load("data/interm/cleaned.Rdata")


# ----------------------------------------------- STEP 2 ----------------------------------------------- 
# Filter out the surnames of interest
cntr <- cntr %>% 
  filter(surname %in% surnames)
ntv <- ntv %>% 
  filter(surname %in% surnames)

# Expand the data
cntr <- cntr %>%
  uncount(freq) 
cntr <- cntr %>%
  mutate(id = 1:nrow(cntr))

# Define number of folds
k <- 4

# Create the folds
set.seed(20240607)
folds <- createFolds(cntr$id, k=k, list=TRUE)


# ----------------------------------------------- STEP 3 ----------------------------------------------- 
# Make a function for EM algorithm
run_EM <- function(M, N, Y, Y0, critv){
  P <- matrix(1 / (M * N), nrow=N, ncol=M)
  P_new <- matrix(NA, nrow=N, ncol=M)
  
  for (i in 1:N) {
    for (j in 1:M) {
      P_new[i, j] <- Y[i, j] + Y0[j] * P[i, j] / sum(P[, j])
    }
  }
  
  P_new <- P_new / sum(P_new)
  diff <- max(abs(P_new - P))
  
  P <- P_new
  
  while (diff > critv) {
    for (i in 1:N) {
      for (j in 1:M) {
        P_new[i,j] <- Y[i,j] + Y0[j] * P[i, j] / sum(P[, j])
      }
    }
    
    P_new <- P_new / sum(P_new)
    diff <- max(abs(P_new - P))
    
    print(diff)
    
    P <- P_new
  }
  
  return(P_new)
}

# Make a function for cross-validation
run_fold <- function(train_data, test_data, supplement=ntv) {
  # Drop the surnames where only the other category has non-NA values
  keep <- train_data %>% 
    filter(new_code != "Other") %>% 
    select(surname) %>% 
    distinct()
  
  train_data <- train_data %>% 
    filter(surname %in% keep$surname)
  
  # Collapse the train sets back to summary level to run EM
  train_data <- train_data %>% 
    group_by(new_code, surname) %>% 
    summarise(freq = n(), .groups = 'drop')
  
  # Prepare the matrix
  surnames <- sort(unique(train_data$surname))
  M <- length(surnames)        
  
  train_data$surname <- factor(train_data$surname, levels=surnames)
  ntv$surname <- factor(ntv$surname, levels=surnames)
  
  # Create vector of country codes 
  countrynames <- sort(unique(train_data$new_code))
  N <- length(countrynames)
  
  train_data$new_code <- factor(train_data$new_code, levels=countrynames)
  
  # Create US born column sparse matrix
  ntvmat <- sparseMatrix(
    i = as.integer(ntv$surname[!is.na(ntv$surname)]),
    j = rep(1, sum(!is.na(ntv$surname))),
    x = as.integer(ntv$freq[!is.na(ntv$surname)]),
    dims = c(M,1))
  rownames(ntvmat) <- surnames
  colnames(ntvmat) <- "US"
  
  # Create M by N sparse matrix
  train_data <- train_data %>%
    filter(!is.na(surname))
  trainmat <- sparseMatrix(
    i = as.integer(train_data$surname),
    j = as.integer(train_data$new_code),
    x = as.integer(train_data$freq),
    dims = c(M, N))
  rownames(trainmat) <- surnames
  colnames(trainmat) <- countrynames
  
  # Rotate all matrices needed for further analysis
  trainmat <- t(trainmat)
  ntvmat <- t(ntvmat)
  
  # Run EM on train data
  result_train <- run_EM(M, N, trainmat, ntvmat, 1e-4)
  
  # Create the conditional probability form result with Bayes' theorem
  probList_train <- t(result_train)/colSums(result_train)
  rownames(probList_train) <- surnames
  colnames(probList_train) <- countrynames
  
  probList_train <- as_tibble(probList_train) %>% 
    round(4) %>% 
    mutate(surname = surnames)
  
  # Create the deterministic form result
  determList_train <- probList_train %>% 
    pivot_longer(cols=AF:XK, names_to="area", values_to="probability") %>% 
    filter(probability >= 0.5) 
  
  # Attach the deterministic prediction result to the test set
  predict_result <- test_data %>% 
    select(-country) %>% 
    merge(determList_train) %>% 
    select(-probability) %>% 
    mutate(expected = factor(new_code, levels=countrynames),
           predicted = factor(area, levels=countrynames))
  
  # Evaluate the prediction
  evalute_predict <- confusionMatrix(data=predict_result$predicted, 
                                     reference=predict_result$expected)
  
  evaluate_predict <- evalute_predict[[4]] %>% 
    as_tibble() %>% 
    mutate(area = countrynames)
  
  return(evaluate_predict)
}


# ----------------------------------------------- STEP 4 ----------------------------------------------- 
# Setup parallel backend to use many processors
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Run cross-validation in parallel
cvResults <- foreach(fold=folds, 
                     .combine='bind_rows', 
                     .packages=c('tidyverse', 'Matrix', 'caret', 'data.table')) %dopar% {
                       train_data <- cntr %>% 
                         filter(!(id %in% fold))
                       test_data <- cntr %>% 
                         filter(id %in% fold)
                       run_fold(train_data, test_data, supplement=ntv)
                      }

# Stop parallel processing
stopCluster(cl)
registerDoSEQ()


# ----------------------------------------------- STEP 5 -----------------------------------------------
# Attach the fold number
cvResults <- cvResults %>% 
  mutate(fold = rep(1:4, each=20)) 

# Create a summary file
cvResultsSummary <- cvResults %>% 
  group_by(area) %>% 
  summarise(across(.cols=Sensitivity:`Balanced Accuracy`, 
                   .fns=~mean(.x, na.rm=TRUE)))

# Save the results
save(cvResults, cvResultsSummary,
     file="data/interm/validateCV.Rdata")
write_csv(cvResultsSummary, 
          file="results/validate/validateResultsCV.csv")

