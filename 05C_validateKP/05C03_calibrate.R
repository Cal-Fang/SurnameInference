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
setwd("/Users/atchoo/Documents/GitHub/SurnameInference/")     # Cal's working directory (mac)
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
validateKP <- read_excel("results/validate/validateResultsKP1.xlsx", sheet=2, skip=1)
validateKP0039 <- read_excel("results/validate/validateResultsKP2.xlsx", sheet=1)
validateKP4059 <- read_excel("results/validate/validateResultsKP2.xlsx", sheet=2)
validateKP60GE <- read_excel("results/validate/validateResultsKP2.xlsx", sheet=3)

# Load the calibration weight
load("data/interm/acsWeight.Rdata")


# ----------------------------------------------- STEP 2 ----------------------------------------------- 
# Extract out the sample size, sensitivity and specificity for Table 4
extract_ss <- function(df) {
  df %>% select(Origin, N, Sensitivity, Specificity)
}

ssKP <- extract_ss(validateKP)
ssKP0039 <- extract_ss(validateKP0039)
ssKP4059 <- extract_ss(validateKP4059)
ssKP60GE <- extract_ss(validateKP60GE)


# ----------------------------------------------- STEP 3 ----------------------------------------------- 
# Merge the weight and calibrate the PPV using Bayesian formula for the national estimates
calibrate_ppv_us <- function(df, weight_df) {
  df %>%
    merge(weight_df, by.x = "Origin", by.y = "ETHN", all.x = TRUE) %>%
    mutate(
      `PPV (US)` = round(Sensitivity * prob1 / 
                           (Sensitivity * prob1 + (100 - Specificity) * (1 - prob1)) * 100, 2),
      `PPV (Asian, US)` = round(Sensitivity * prob2 / 
                                  (Sensitivity * prob2 + (100 - `Specificity (Asian)`) * (1 - prob2)) * 100, 2),
      `Prevalence (US)` = round(prob1 * 100, 2)
    ) %>%
    select(Origin, `Prevalence (US)`, `PPV (US)`, `PPV (Asian, US)`)
}

pKPUS <- calibrate_ppv_us(validateKP, usWeight)
pKPUS0039 <- calibrate_ppv_us(validateKP0039, usWeight0039)
pKPUS4059 <- calibrate_ppv_us(validateKP4059, usWeight4059)
pKPUS60GE <- calibrate_ppv_us(validateKP60GE, usWeight60GE)

# Do the same for the SF estimates
calibrate_ppv_sf <- function(df, weight_df) {
  df %>%
    merge(weight_df, by.x = "Origin", by.y = "ETHN", all.x = TRUE) %>%
    mutate(
      `PPV (SF)` = round(Sensitivity * prob1 / 
                           (Sensitivity * prob1 + (100 - Specificity) * (1 - prob1)) * 100, 2),
      `PPV (Asian, SF)` = round(Sensitivity * prob2 / 
                                  (Sensitivity * prob2 + (100 - `Specificity (Asian)`) * (1 - prob2)) * 100, 2),
      `Prevalence (SF)` = round(prob1 * 100, 2)
    ) %>%
    select(Origin, `Prevalence (SF)`, `PPV (SF)`, `PPV (Asian, SF)`)
}

pKPSF <- calibrate_ppv_sf(validateKP, sfWeight)
pKPSF0039 <- calibrate_ppv_sf(validateKP0039, sfWeight0039)
pKPSF4059 <- calibrate_ppv_sf(validateKP4059, sfWeight4059)
pKPSF60GE <- calibrate_ppv_sf(validateKP60GE, sfWeight60GE)

# Combine results for the two geographic areas
pKP <- merge(pKPUS, pKPSF)
pKP0039 <- merge(pKPUS0039, pKPSF0039)
pKP4059 <- merge(pKPUS4059, pKPSF4059)
pKP60GE <- merge(pKPUS, pKPSF60GE)


# ----------------------------------------------- STEP 5 -----------------------------------------------
# Save the results for making tables
save(ssKP, ssKP0039, ssKP4059, ssKP60GE,
     pKP, pKP0039, pKP4059, pKP60GE,
     file="data/interm/validateKP.Rdata")

