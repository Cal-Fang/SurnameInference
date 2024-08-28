## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Script name: 05C02_calibrate.R
##
## Purpose of script: To download ACS 2018-2022 5-year sample to calculate calibration weight.
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
##   Line 45-47 and line 59-63 has been commentized as I do not want to request new data from
## ACS every time. Anyone who want to check the whole process can de-commentize these lines 
## and delete Line 64 to examine everything.
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

require(ipumsr)         # For requesting ACS race/ethnicity distribution through IPUMS
require(readxl)         # For reading the validation results

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ----------------------------------------------- STEP 1 ----------------------------------------------- 
# # Set up API keys for ipum
# ## Please register an account on IPUMS (https://usa.ipums.org/usa-action/menu) and request an API key here (https://account.ipums.org/api_keys)
# set_ipums_api_key("YOURAPIKEYGOESHERE", save=TRUE)


# ----------------------------------------------- STEP 2 ----------------------------------------------- 
# Create the data request payload to download ACS race/ethnicity distribution
usa_ext_def <- define_extract_usa(
  description = "2018-2022 5-year ACS Data",
  samples = "us2022c",
  variables = c("SEX", "RACE", "PERWT", "MET2013"),
  data_format = "csv"
)

# # Submit the extract request
# usa_ext_submitted <- submit_extract(usa_ext_def)
# 
# # Download the extract - you may need to wait for several seconds after last line of code
# filepath <- download_extract(usa_ext_submitted, download_dir="data/ACS2018_2022")
filepath <- "data/ACS2018_2022/usa_00007.xml"

# Read in the data
ddi <- read_ipums_ddi(filepath)
acsRace <- read_ipums_micro(ddi, vars=c("RACED", "SEX", "PERWT", "MET2013"))


# ----------------------------------------------- STEP 2 ----------------------------------------------- 
# Write a function to summarize the distribution
distriSummarize <- function(data){
  # Create a dummy variable for Asian
  acsRace <- data %>% 
    mutate(ASIAN = RACED >= 400 & RACED < 679)
  
  # Examine existing ethnicity choices among Asian
  racedSummary <- acsRace %>%
    filter(ASIAN) %>%
    group_by(RACED) %>%
    summarise(count = sum(PERWT)) 
  
  # Rename the ethnicity code to be consistent with KP distribution
  replace <- tibble(RACED = c(660, 400, 410, 420, 666,
                              665, 670, 500, 620, 662,
                              667, 643, 600, 610, 669,
                              663, 640),
                    ETHN = c("Cambodia", "China", "China", "China", "Indonesia", 
                             "Myanmar", "Sri Lanka", "Japan", "Korea", "Laos",
                             "Malaysia", "Nepal", "Philippines", "South Asian Subcontinent", "South Asian Subcontinent",
                             "Thailand", "Vietnam"))
  
  # Summarize the new ethnicity
  ethnSummary <- racedSummary %>% 
    merge(replace) %>% 
    group_by(ETHN) %>%
    summarise(count = sum(count))
  
  # Count the total number of Asian (including bi-ethnicity and others)
  asianSummary <- acsRace %>%
    group_by(ASIAN) %>%
    summarise(count = sum(PERWT))
  
  # Calculate the probability of each ethnicity for later Bayesian calculation
  ## prob1 denotes the proportion of the corresponding Asian ethnicity population in all-race population
  ## prob2 denotes the proportion of the corresponding Asian ethnicity population in Asian population
  ethnSummary <- ethnSummary %>% 
    mutate(prob1 = count / (sum(count) + asianSummary$count[!asianSummary$ASIAN]),
           prob2 = count / sum(count))
  
  # Rbind a row for all Asian (including bi-ethnicity and others)
  weight <- rbind(list("Overall Asian", asianSummary$count[asianSummary$ASIAN], asianSummary$count[asianSummary$ASIAN] / sum(asianSummary$count), NA),
                  ethnSummary)
  
  return(weight)
}


# ----------------------------------------------- STEP 3 ----------------------------------------------- 
# Run the function to summarize for national sample
usWeight <- distriSummarize(acsRace)

# Run the function to summairze for SF metropolitan area sample
sfRace <- acsRace %>% 
  filter(MET2013 == 41860)
sfWeight <- distriSummarize(sfRace)
                   

# ----------------------------------------------- STEP 4 ----------------------------------------------- 
# Save the numbers for calibration
save(usWeight, sfWeight,
     file="data/interm/acsWeight.Rdata")

