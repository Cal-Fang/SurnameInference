# Clear out the history
rm(list=ls())

# Read in the packages needed
library(Matrix)
library(tidyverse)

# Set working directory
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path

# STEP 1
# Load in the Rdata file generated from analysis script
load(file="data/PrepData.Rdata")

# STEP 2
# Make a function
run_EM <- function(M, N, Y, Y0, critv){
  Pi <- matrix(1/(M*N), nrow = N, ncol = M)
  Pi_new <- matrix(NA, nrow = N, ncol = M)
  
  for (i in 1:N) {
    for (j in 1:M) {
      Pi_new[i, j] <- Y[i, j] + Y0[j] * Pi[i, j] / sum(Pi[, j])
      # Pi_new_deno <- sum(Y) + sum(Y0) * Pi[i, j] / sum(Pi[, j])
    }
  }
  
  Pi_new <- Pi_new / sum(Pi_new)
  diff <- max(abs(Pi_new - Pi))
  
  Pi <- Pi_new
  
  while (diff > critv) {
    for (i in 1:N) {
      for (j in 1:M) {
        Pi_new[i,j] <- Y[i,j] + Y0[j] * Pi[i, j] / sum(Pi[, j])
        # Pi_new_deno <- sum(Y) + sum(Y0) * Pi[i, j] / sum(Pi[, j])
      }
    }
    
    Pi_new <- Pi_new / sum(Pi_new)
    diff <- max(abs(Pi_new - Pi))
    
    print(diff)
    
    Pi <- Pi_new
  }
  
  return(Pi_new)
}

# STEP 3
# Run on the dataset without US oversea territories data
# result <- run_EM(M, N, cntrmat, ntvmat, 1e-5)
# write.csv(result, "data/result_noUSO.csv")

# STEP 4
# Run on the dataset with US oversea territories data
# load(file="data/AllData3.Rdata")
# result2 <- run_EM(M, N2, cntrmat_withUSO, ntvmat)
# # write.csv(result2, "data/result_withUSO.csv")
# save(result, result2,
#      surnames, M,
#      countrynames, N, countrynames2, N2,
#      file="data/results.Rdata")

# STEP 5
# Check the difference
# result2d <- result2[-21,]
# 
# result2d_p <- t(t(result2d)/colSums(result2d))
# result_p <- t(t(result)/colSums(result))
#   
# result_diff <- result_p - result2d_p
# colnames(result_diff) <- surnames
# rownames(result_diff) <- countrynames
# 
# max(result_diff[result_diff != 0])
# colnames(result_p) <- surnames
# rownames(result_p) <- countrynames
# 
# which(surnames == "NGUYEN      ")
# which(surnames == "FANG        ")


# STEP 6
# Generate new result datasets with smaller threshold
result3 <- run_EM(M, N, cntrmat, ntvmat, 1e-3)
result4 <- run_EM(M, N, cntrmat, ntvmat, 1e-4)
result5 <- run_EM(M, N, cntrmat, ntvmat, 1e-5)

# Check the probabilistic form
result3_p <- t(t(result3)/colSums(result3))
colnames(result3_p) <- surnames
rownames(result3_p) <- countrynames

result4_p <- t(t(result4)/colSums(result4))
colnames(result4_p) <- surnames
rownames(result4_p) <- countrynames

result5_p <- t(t(result5)/colSums(result5))
colnames(result5_p) <- surnames
rownames(result5_p) <- countrynames

# Save the results
save(result3, result3_p,
     surnames, M,
     countrynames, N,
     file="data/result_1e-3.Rdata")
save(result4, result4_p,
     surnames, M,
     countrynames, N,
     file="data/result_1e-4.Rdata")
save(result5, result5_p,
     surnames, M,
     countrynames, N,
     file="data/result_1e-5.Rdata")

# result4p_df <- as.data.frame(t(result4_p))
# result4p_df <- round(result4p_df, 3)
# write.csv(result4p_df, "data/result4p_df.csv")
