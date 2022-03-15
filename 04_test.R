# Read in the packages needed
library(Matrix)
library(tidyverse)

# Test EM-algorithm on a fraction of the dataset
# STEP 1
# Set working directory and load in the Rdata file generated from analysis script
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path
load(file="data/AllData2.Rdata")

# STEP 2
set.seed(1000)
index <- sample(1:M, 300, replace=FALSE)
BT <- sample(which(cntrmat["BT", ] > 0), 3, replace=FALSE)
MV <- sample(which(cntrmat["MV", ] > 0), 2, replace=FALSE)
index <- sort(c(index, BT, MV))

# STEP 3
Pi <- matrix(1/(M*N), nrow = N, ncol = 305)
Pi_new <- matrix(NA, nrow = N, ncol = 305)
Y <- cntrmat[, index]
Y0 <- ntvmat[1, index]

for (i in 1:N) {
  for (j in 1:305) {
    Pi_new[i,j] <- Y[i,j] + Y0[j] * Pi[i, j] / sum(Pi[, j])
    # Pi_new_deno <- sum(Y) + sum(Y0) * Pi[i, j] / sum(Pi[, j])
  }
}

Pi_new <- Pi_new / sum(Pi_new)
diff <- max(abs(Pi_new - Pi))

Pi <- Pi_new

while (diff > 1e-5) {
  for (i in 1:N) {
    for (j in 1:305) {
      Pi_new[i,j] <- Y[i,j] + Y0[j] * Pi[i, j] / sum(Pi[, j])
      # Pi_new_deno <- sum(Y) + sum(Y0) * Pi[i, j] / sum(Pi[, j])
    }
  }
  
  Pi_new <- Pi_new / sum(Pi_new)
  diff <- max(abs(Pi_new - Pi))
  Pi <- Pi_new
  print(diff)
}
