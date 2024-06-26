## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Script name: 03_EM.R
##
## Purpose of script: To run EM algorithm.
##
## Author: Cal Chengqi Fang
##
## Date Created: 2022-06-21
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
require(Matrix)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ----------------------------------------------- STEP 1 ----------------------------------------------- 
# Load in the formatted matrices
load(file="data/interm/formatted.Rdata")


# ----------------------------------------------- STEP 2 ----------------------------------------------- 
# Make a function to run EM algorithm
run_EM <- function(M, N, Y, Y0, critv) {
  P <- matrix(1/(M*N), nrow=N, ncol=M)
  P_new <- matrix(NA, nrow=N, ncol=M)
  
  for (i in 1:N) {
    for (j in 1:M) {
      P_new[i, j] <- Y[i, j] + Y0[j] * P[i, j] / sum(P[, j])
      # P_new_deno <- sum(Y) + sum(Y0) * P[i, j] / sum(P[, j])
    }
  }
  
  P_new <- P_new / sum(P_new)
  diff <- max(abs(P_new - P))
  
  P <- P_new
  
  while (diff > critv) {
    for (i in 1:N) {
      for (j in 1:M) {
        P_new[i,j] <- Y[i,j] + Y0[j] * P[i, j] / sum(P[, j])
        # P_new_deno <- sum(Y) + sum(Y0) * P[i, j] / sum(P[, j])
      }
    }
    
    P_new <- P_new / sum(P_new)
    diff <- max(abs(P_new - P))
    
    print(diff)
    
    P <- P_new
  }
  
  return(P_new)
}

# Run the algorithm
result4 <- run_EM(M, N, cntrmat, ntvmat, 1e-4)


# ----------------------------------------------- STEP 3 ----------------------------------------------- 
# Save the results
save(result4, 
     surnames, M,
     countrynames, N,
     file="data/interm/resultRaw.Rdata")

