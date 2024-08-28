## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Script name: 05C03_calibrate.R
##
## Purpose of script: To calibrate the north California validation results to the country-level.
##
## Author: Cal Chengqi Fang
##
## Date Created: 2024-07-16
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

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ----------------------------------------------- STEP 1 ----------------------------------------------- 
# Read in the validation results sent from Kaiser
demog <- read_excel("results/validate/validateResultsKP.xlsx", sheet=1, n_max=19)
validateKP <- read_excel("results/validate/validateResultsKP.xlsx", sheet=2, skip=1)

# Load the calibration weight
load("data/interm/acsWeight.Rdata")


# ----------------------------------------------- STEP 2 ----------------------------------------------- 
# Extract out the sample size, sensitivity and specificity for Table 4
validateKP1 <- validateKP %>% 
  select(Origin, N, Sensitivity, Specificity)


# ----------------------------------------------- STEP 3 ----------------------------------------------- 
# Merge the calibration weight for national sample
validateKPUS <- validateKP %>% 
  merge(usWeight, by.x="Origin", by.y="ETHN", all.x=TRUE) 

# Calibrate the PPV using Bayesian formula for general population and within-Asian population in national sample
validateKPUS <- validateKPUS %>% 
  mutate(`PPV (US)` = round(Sensitivity * prob1 / (Sensitivity * prob1 + (100 - Specificity) * (1 - prob1)) * 100, 2),
         `PPV (Asian, US)` = round(Sensitivity * prob2 / (Sensitivity * prob2 + (100 - `Specificity (Asian)`) * (1 - prob2)) * 100, 2),
         `Prevalence (US)` = round(prob1 * 100, 2)) %>% 
  select(Origin, `Prevalence (US)`, `PPV (US)`, `PPV (Asian, US)`)


# Merge the calibration weight for SF metropolitan area sample
validateKPSF <- validateKP %>% 
  merge(sfWeight, by.x="Origin", by.y="ETHN", all.x=TRUE) 

# Calibrate the PPV using Bayesian formula for general population and within-Asian population in national sample
validateKPSF <- validateKPSF %>% 
  mutate(`PPV (SF)` = round(Sensitivity * prob1 / (Sensitivity * prob1 + (100 - Specificity) * (1 - prob1)) * 100, 2),
         `PPV (Asian, SF)` = round(Sensitivity * prob2 / (Sensitivity * prob2 + (100 - `Specificity (Asian)`) * (1 - prob2)) * 100, 2),
         `Prevalence (SF)` = round(prob1 * 100, 2)) %>% 
  select(Origin, `Prevalence (SF)`, `PPV (SF)`, `PPV (Asian, SF)`) 

# cbind two results
validateKP2 <- validateKPUS %>% 
  merge(validateKPSF)
validateKP <- validateKP1 %>% 
  merge(validateKP2)


# ----------------------------------------------- STEP 5 -----------------------------------------------
# Save the results for making tables
save(validateKP1, validateKP2, 
     file="data/interm/validateKP.Rdata")
fwrite(validateKP, 
       file="results/validate/validateResultsKP.csv")

