## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Script name: 04_modify.R
##
## Purpose of script: To create the probabilistic form and deterministic form result.
##
## Author: Cal Chengqi Fang
##
## Date Created: 2022-06-24
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
setwd("/Users/atchoo/Documents/GitHub/Name-Method-Project/")  # Cal's working directory (mac)
# setwd("C:/Users/")     # Cal's working directory (PC)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls())
options(scipen=6, digits=4)         # I prefer to view outputs in non-scientific notation
memory.limit(30000000)                  # this is needed on some PCs to increase memory allowance, but has no impact on macs.

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

## load up the packages we will need:  (uncomment as required)
require(tidyverse)
require(Matrix)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ----------------------------------------------- STEP 1 ----------------------------------------------- 
# Load in the Rdata file generated from EM script
load(file="data/interm/resultRaw.Rdata")


# ----------------------------------------------- STEP 2 ----------------------------------------------- 
# Create the conditional probability form result with Bayes' theorem
probList <- t(result4)/colSums(result4)
rownames(probList) <- surnames
colnames(probList) <- countrynames

# Attach a surname column
probList <- as_tibble(probList) %>% 
  round(4) %>% 
  mutate(surname = surnames)


# ----------------------------------------------- STEP 3 ----------------------------------------------- 
# Create the deterministic form result
determList <- probList %>% 
  pivot_longer(cols=AF:XK, names_to="area", values_to="probability") %>% 
  filter(probability >= 0.5) 


# ----------------------------------------------- STEP 4 ----------------------------------------------- 
# Examine the numbers of surname highly predictive of Asian descent
cat("The number of name highly predictive of Asian descendant:", 
    sum(probList$Other < 0.4),
    "\nThe number of name relatively predictive of Asian descendant:", 
    sum(probList$Other < 0.5))

# Further examine to make sure there is no non-Asian surname in deterministic list
nonAsian <- probList %>% 
  filter(Other >= 0.5)

lessAsian <- probList %>% 
  filter(Other >= 0.4)

inspect <- determList %>% 
  filter(surname %in% nonAsian$surname) %>% 
  group_by(area) %>% 
  summarise(count = n())

# Drop surnames with higher or equal to 50% probability of originating from non-Asia area
probList <- probList %>%
  filter(Other <= 0.5)


# ----------------------------------------------- STEP 5 ----------------------------------------------- 
# Save the results
save(probList, determList,
     file="data/interm/resultRep.Rdata")

write_csv(probList, "results/probList.csv")
write_csv(determList, "results/determList.csv")

