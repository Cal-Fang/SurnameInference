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
result4p_df <- as.data.frame(t(result4_p))
# Generate one set that has three 
result4p_df2 <- round(result4p_df, 3)
# write.csv(result4p_df2, "data/result4p_df2.csv")

panasian_df <- result4p_df %>% 
  mutate(asian = 1 - Other,
         surname = rownames(result4)) %>% 
  select(c("asian", "surname"))
# panasian_df2 <- round(panasian_df, 3)

# STEP 3
# Create the deterministic form
determine_df <- result4p_df %>%
  mutate(largest = apply(result4_p, 2, max),
         maxcntr_list = lapply(apply(t(result4_p), 1, function(x) which(x == max(x))), names),
         surname = rownames(result4p_df)) %>%
  mutate(maxcntr = unlist(lapply(maxcntr_list, paste, collapse=" & "))) %>%
  select(-"maxcntr_list")

# Mutate the second largest probability
determine_df$largest2 <- apply(result4_p, 2, function(x) Rfast::nth(x, 2, descending=T))


determine_df <- determine_df %>% 
  filter(surname %in% fb_terr_ntv4.1$surname)


