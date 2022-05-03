# Clear out the history
rm(list=ls())

# Read in the packages needed
library(Matrix)
library(tidyverse)

# Set working directory
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path

# STEP 1
# Load in the Rdata file generated from EM script
load(file="data/result_1e-4.Rdata")

# STEP 2
result4_p <- result4_p[, surnames]
result4p_df <- as.data.frame(t(result4_p))
# write.csv(result4p_df2, "data/result4p_df2.csv")
probability_df <- round(result4_p, 3)

panasian_df <- result4p_df %>% 
  mutate(asian = 1 - Other) %>% 
  round(3) %>% 
  mutate(surname = rownames(result4p_df)) %>% 
  select(c("asian", "surname"))

# STEP 3
# Create the deterministic form
determine_df <- result4p_df %>%
  mutate(largest = apply(result4_p, 2, max)) %>% 
  round(3) %>%
  mutate(maxcntr_list = lapply(apply(t(result4_p), 1, function(x) which(x == max(x))), names),
         surname = rownames(result4p_df)) %>% 
  mutate(maxcntr = unlist(lapply(maxcntr_list, paste, collapse=" & "))) %>%
  select(-"maxcntr_list")

# Mutate the second largest probability
determine_df$largest2 <- apply(result4_p, 2, function(x) Rfast::nth(x, 2, descending=T)) %>% 
  round(3)
determine_df$maxcntr2 <- lapply(apply(t(result4_p), 1, function(x) which(x == Rfast::nth(x, 2, descending=T))), names)
determine_df$maxcntr2[lengths(determine_df$maxcntr2) == 19] <- NA
determine_df$maxcntr2 <- unlist(lapply(determine_df$maxcntr2, paste, collapse=" & "))

determine_df <- determine_df[, c("surname", "maxcntr", "maxcntr2")]

save(probability_df,
     panasian_df,
     determine_df,
     file="data/ReportResult.Rdata")
