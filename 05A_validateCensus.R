## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Script name: 05A_validateCensus.R
##
## Purpose of script: To validate the determList with census data.
##
## Author: Cal Chengqi Fang
##
## Date Created: 2024-06-07
##
## Copyright (c) Cal Chengqi Fang, 2023
## Email: cal.cf@uchicago.edu
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Notes:
##   1. We reconfigured the Hispanic percentage into four main racial groups based on data 
## from the 2010 Census Brief, "Overview of Race and Hispanic Origin: 2010." This report 
## indicated the following racial composition: 53% White, 2.5% Black, 1.4% Native American, 
## 0.4% Asian, 36.7% some other race, and 6% two or more races.
##   2. This script primarily validates our results' ability to predict the Asian racial group.
##   3. 2010 Census surname list can be downloaded from the United States Census Bureau website:
## https://www2.census.gov/topics/genealogy/2010surnames/names.zip
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
# Load in the Rdata file of reportable results
load(file="data/interm/resultRep.Rdata")

# Read in and modify the CENSUS
census100 <- read_csv("data/namesCensus/Names_2010Census.csv") 
census100[, 2:11] <- sapply(census100[, 2:11], as.numeric)

# Load in the names dropped in the cleaning process
load("results/tests/droppedNames.Rdata")


# ----------------------------------------------- STEP 2 ----------------------------------------------- 
# Modify every name to 12 letter length
census100 <- census100 %>% 
  mutate(name = ifelse((nchar(name) <= 12), 
                       str_pad(name, 12, side="right", pad=" "), 
                       str_trunc(name, 12, ellipsis=""))) 

# Reconfigure the hispanic percentage to the four race group
census100 <- census100 %>%
  mutate(pctwhite = if_else(!is.na(pcthispanic), pctwhite + pcthispanic * 0.53, pctwhite),
         pctblack = if_else(!is.na(pcthispanic), pctblack + pcthispanic * 0.025, pctblack),
         pctapi = if_else(!is.na(pcthispanic), pctapi + pcthispanic * 0.004, pctapi),
         pctaian = if_else(!is.na(pcthispanic), pctaian + pcthispanic * 0.014, pctaian),
         pct2prace = if_else(!is.na(pcthispanic), pct2prace + pcthispanic * 0.06, pct2prace)) 

# Create a variable denoting the highest proportion
census100 <- census100 %>%
  pivot_longer(cols=pctwhite:pctaian, names_to="maxRace", values_to="maxPct") %>%
  group_by(name) %>%
  filter(maxPct == max(maxPct)) %>%
  ungroup() %>% 
  select(name, maxPct) %>% 
  distinct() %>% 
  left_join(census100) %>% 
  mutate(asianCensus = ifelse(!is.na(pct2prace) & pct2prace == maxPct, NA,
                              ifelse(pctapi == maxPct, TRUE, FALSE))) %>% 
  distinct(name, asianCensus) 


# ----------------------------------------------- STEP 3 ----------------------------------------------- 
# Create a probability of being Asian in our result
probList <- probList %>% 
  mutate(asian = 1 - Other)

# Check the inclusion
test_df <- census100 %>%
  filter(asianCensus==TRUE) %>%
  merge(probList, by.x="name", by.y="surname", all.x=TRUE)

# Calculate the coverage rate (87.02%)
sum(!is.na(test_df$asian)) / nrow(test_df)

# Examine where the names were dropped
noton <- filter(test_df, is.na(test_df$asian))
noton1 <- noton[!(noton$name %in% fb_ntv$surname), ]
noton2 <- noton[(noton$name %in% fb_ntv$surname) & !(noton$name %in% fb_ntv2$surname), ]
noton3 <- noton[(noton$name %in% fb_ntv2$surname) & !(noton$name %in% fb_ntv3$surname), ]
noton4 <- noton[(noton$name %in% fb_ntv3$surname) & !(noton$name %in% fb_ntv4$surname), ]
noton5 <- noton[(noton$name %in% fb_ntv4$surname) & !(noton$name %in% fb_ntv5$surname), ]

cat("Missingness status:\n", nrow(noton1), "names were never in our original data.\n",
    nrow(noton2), "names were omitted because the total frequency in our data is not larger than 50.\n",
    nrow(noton3), "names were omitted because the total 19 Asian countries foreign born frequency is not larger than 10.\n",
    nrow(noton4), "names were omitted because the most foreign born were from outside Asia.\n",
    nrow(noton5), "name was omitted because the foreign born rate were smaller than 5%.")

# Remove objects to clean the memory
rm(fb_ntv, fb_ntv2, fb_ntv3, fb_ntv4, fb_ntv5,
   noton, noton1, noton2, noton3, noton4, noton5)


# ----------------------------------------------- STEP 4 ----------------------------------------------- 
# Examine the prediction of the whole list
predict <- census100 %>% 
  merge(probList, by.x="name", by.y="surname", all.x=TRUE) %>% 
  mutate(TP = asianCensus==TRUE & asian > 0.50,
         FP = asianCensus!=TRUE & asian > 0.50,
         TN = asianCensus!=TRUE & (asian < 0.50 | is.na(asian)),
         FN = asianCensus==TRUE & (asian < 0.50 | is.na(asian)))

# Name count
wholeCount <- sum(!is.na(predict$asian))
# PPV
wholePPV <- sum(predict$TP, na.rm=TRUE) / (sum(predict$TP, na.rm=TRUE) + sum(predict$FP, na.rm=TRUE))
# Sensitivity
wholeSensitivity <- sum(predict$TP, na.rm=TRUE) / (sum(predict$TP, na.rm=TRUE) + sum(predict$FN, na.rm=TRUE))
# Specificity
wholeSpecificity <- sum(predict$TN, na.rm=TRUE) / (sum(predict$TN, na.rm=TRUE) + sum(predict$FP, na.rm=TRUE))

cat("Whole list prediction with ", wholeCount, " surnames has ",
    "PPV of ", wholePPV, ", ",
    "sensitivity of ", wholeSensitivity, ", and ",
    "specificity of ", wholeSpecificity, ".",
    sep="")

# ----------------------------------------------- STEP 5 ----------------------------------------------- 
# Create a variable to denote the most probable origin
## Here we report the most probable origin among the 19 Asian area
cntr <- probList %>% 
  select(-Other) %>% 
  pivot_longer(cols=AF:XK, names_to="maxCntr", values_to="maxProp") %>%
  group_by(surname) %>%
  filter(maxProp == max(maxProp)) %>%
  ungroup() %>% 
  select(surname, maxCntr, asian)

# Repeat the process
## We have to repeat the process instead of creating maxCntr before STEP 3
## because there are surnames that fall into multiple maxCntr category.
predictSub <- census100 %>% 
  merge(cntr, by.x="name", by.y="surname", all.x=TRUE) %>% 
  mutate(TP = asianCensus==TRUE & asian > 0.50,
         FP = asianCensus!=TRUE & asian > 0.50,
         TN = asianCensus!=TRUE & (asian < 0.50 | is.na(asian)),
         FN = asianCensus==TRUE & (asian < 0.50 | is.na(asian))) %>% 
  select(name, maxCntr, 
         TP, FP, TN, FN)

# Examine the prediction of the sub-lists
predictSubSummary <- predictSub %>% 
  group_by(maxCntr) %>% 
  summarise(nameCount = n(),
            PPV = sum(TP, na.rm=TRUE) / (sum(TP, na.rm=TRUE) + sum(FP, na.rm=TRUE)),
            Sensitivity = NA,
            Specificity = NA) %>% 
  filter(!is.na(maxCntr))


# ----------------------------------------------- STEP 5 ----------------------------------------------- 
# Combine the sub-list evaluation results with the result of the whole list
predictSummary <- rbind(c("Whole List", wholeCount, wholePPV, wholeSensitivity, wholeSpecificity),
                        predictSubSummary)

# Save the results
save(predictSummary,
     file="data/interm/validateCensus.Rdata")
fwrite(predictSummary, 
       file="results/validate/validateResultsCensus.csv")
