## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Script name: 01B_clean2.R
##
## Purpose of script: To clean the surnames.
##
## Author: Cal Chengqi Fang
##
## Date Created: 2022-01-14
##
## Copyright (c) Cal Chengqi Fang, 2024
## Email: cal.cf@uchicago.edu
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Notes:
##   1. Check STEP 3 on how sum_asian is generated if you have changed any country code.
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
require(Matrix)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ----------------------------------------------- STEP 1 ----------------------------------------------- 
# # Read in the two summary documents
# cntr <- read.fwf("data/SSA/srnmcntr.txt", c(12,2,6), col.names=c("surname", "country", "freq"), 
#                  na.strings=c())    # Have to specify this because there is one country code NA
# ntv <- read.fwf("data/SSA/srnmntv.txt", c(12,8), col.names=c("surname", "freq"))
# fb <- cntr %>%             # Just to keep two data sets consistent
#   filter(freq > 2) %>%     # Actually unnecessary as cntr has already been filtered.
#   group_by(surname) %>%
#   summarise(freq = sum(freq))
# 
# save(cntr, ntv, fb,
#      file="data/interm/original.Rdata")
load(file="data/interm/original.Rdata")

# Drop all records that actually have no surname
noname <- c("NO NAME     ", "NO GIVEN NAM", "NO LAST NAME", "NO NAME GIVE")

cntr <- filter(cntr, !(surname %in% noname))
ntv <- filter(ntv, !(surname %in% noname))
fb <- filter(fb, !(surname %in% noname))

# Combine all multiple-word names into single-word names
com_name <- function(df, cntr=FALSE){
  df$surname <- gsub(" ", "", df$surname)
  df$surname <- gsub("\\s", " ", format(df$surname, width=12))
  
  if (cntr) {
    df_comb <- df %>% 
      group_by(surname, country) %>% 
      summarise(freq = sum(freq)) %>% 
      ungroup()
  } else {
    df_comb <- df %>% 
      group_by(surname) %>% 
      summarise(freq = sum(freq)) %>% 
      ungroup()
  }
  
  return(df_comb)
}

cntr <- com_name(cntr, cntr=TRUE)
ntv <- com_name(ntv)
fb <- com_name(fb)


# Combine two data
fb_ntv <- fb %>% 
  merge(ntv, by="surname", all=TRUE)


# ----------------------------------------------- STEP 2 ----------------------------------------------- 
# Filter out all surnames of which the total frequency is not bigger than 50
fb_ntv$sum_freq <- rowSums(fb_ntv[, -1], na.rm=TRUE)
fb_ntv$fb_prop <- fb_ntv$freq.x / fb_ntv$sum_freq

fb_ntv2 <- filter(fb_ntv, sum_freq>50)


# ----------------------------------------------- STEP 3 ----------------------------------------------- 
# Filter out all surnames of which the total frequency of the 19 Asian countries is smaller or equal to 10
code_replace <- read_csv("data/interm/codeReplace.csv")

cntr <- cntr %>% 
  merge(code_replace, by.x="country", by.y="code", all.x=TRUE) %>% 
  mutate(new_code = ifelse(is.na(new_code), "Other", new_code))   # This would deal with original NA and ZI codes.

asia <- cntr %>%
  filter(new_code != "Other") %>% 
  group_by(surname) %>% 
  summarise(t_asia = sum(freq)) %>% 
  filter(t_asia > 10) 

asiansurnames <- sort(asia$surname)  

fb_ntv3 <- filter(fb_ntv2, surname %in% asiansurnames)


# ----------------------------------------------- STEP 4 ----------------------------------------------- 
# Filter out the surnames of which the "Other" category is more than the sum of other countries
cntr0 <- cntr
cntr0$surname <- factor(cntr0$surname, levels=sort(unique(fb_ntv3$surname)))
cntr0$country <- factor(cntr0$new_code, levels=sort(unique(cntr0$new_code)))
cntr0 <- cntr0 %>%
  filter(!is.na(surname))
cntr0mat <- sparseMatrix(
  i = as.integer(cntr0$surname),
  j = as.integer(cntr0$country),
  x = as.integer(cntr0$freq),
  dims = c(length(fb_ntv3$surname), 20))
rownames(cntr0mat) = sort(unique(fb_ntv3$surname))
colnames(cntr0mat) = sort(unique(cntr0$new_code))

# Sum the frequency of all non-Other group
asiansum <- as.data.frame(as.matrix(cntr0mat)) %>% 
  mutate(sum_asian = apply(cntr0mat[, setdiff(unique(code_replace$new_code), "Other")], 1, sum, na.rm=TRUE),      
         surname = rownames(cntr0mat)) %>%      
  filter(sum_asian > Other)

fb_ntv4 <- filter(fb_ntv3, surname %in% asiansum$surname)


# ----------------------------------------------- STEP 5 ----------------------------------------------- 
# Filter out all surnames of which the foreign born records were not more than 5% of the total population living in America
fb_ntv5 <- filter(fb_ntv4, fb_prop>0.05)

# Recover the surnames to the 12 digits form
surnames <- gsub("\\s", " ", format(fb_ntv5$surname, width=12))


# ----------------------------------------------- STEP 6 ----------------------------------------------- 
# Save cleaned results
save(cntr, ntv,
     surnames, 
     file="data/interm/cleaned.Rdata")

# Save the drop records 
save(fb_ntv,
     fb_ntv2,
     fb_ntv3,
     fb_ntv4,
     fb_ntv5,
     file="results/tests/droppedNames.Rdata")

