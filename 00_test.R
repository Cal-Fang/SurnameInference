library(Matrix)

# Set working directory and read in the four datasets
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path

# Load in the data
load(file="data/AllData.Rdata")

# Create test dataset first 
# Use TH ("Thailand") for this test by the way
library(tidyverse)
test1 <- cntr %>% 
  filter(country == "TH")
sur_test <- sort(unique(test1$surname))

test2 <- us %>% 
  filter(surname %in% sur_test)
test3 <- fb %>% 
  filter(surname %in% sur_test)

test4 <- cntr %>% 
  filter((country != "TH") & 
           (surnames %in% sur_test))

# Test the convergence loop on several values
for (i in countrynames) 
{
  pi_ij <- Y_ij / phi_i * (Y_0j + sum(Y_ij)) / sum(Y_ij / phi_i)
  for (j in surnames)
  {
    Y_ij_frame = Y_ij_frame %>% 
      filter(Y_ij > 0)
    phi_i = sum(Y_ij / pi_ij) / length(Y_ij)
  }
}
  

pi_vietnam_chew <- cntr["vietnam", "Chew"] / fb_estimates["vietnam"] * (ntv["Chew"] + terr["Chew"] + sum(cntr["Chew"])) / 













# library(doParallel)
# 
# # Specify the number of cores/workers we want to use
# n_cores <- detectCores() - 1
# n_cores

cntr <- read.fwf("data/srnmcntr.txt", c(12,2,6), col.names=c("surname","country","freq"), na.strings = c())

cntr %>% 
  filter(country == "BX") %>% 
  summarize(sum = sum(freq))

cntr %>% 
  filter(country == "YQ") 


cntr %>% 
  filter(surname == "A           ") %>% 
  summarise(sum = sum(freq))

terr %>% 
  filter(surname == "ZHAO        ") %>% 
  summarise(sum = sum(freq))




