# Clear out the history
rm(list=ls())

# Read in the packages needed
library(Matrix)
library(tidyverse)

# STEP 1
# Set working directory and load in the Rdata file generated from analysis script
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path
load(file="data/AllData2.Rdata")

# STEP 2
# Transform the first variable of fb_estimate into a vector of Phi trigger values
phi <- fb_estimates[[1]]

# Create the empty Pi matrix
Pi <- matrix(NA, nrow = N, ncol = M)

# Form the Y matrix based on the cntrmat
Y <- cntrmat

# Transform the ntvmat into a vector of Y0
Y0 <- ntvmat[1, ]

# STEP 3
# Calculate the first round value
for (i in 1:N) {
  for(j in 1:M) {
    Pi[i,j] <- Y[i,j] / phi[i] * (Y0[j] + sum(Y[,j])) / (sum(Y[,j] / phi))
  }
}

for (i in 1:N) {
  index_i <- which(Y[i,] > 0)
  phi[i] <- mean(Y[i, index_i] / Pi[i, index_i])
}

# Create the storage variable and counter
Pi_old <- Pi
phi_old <- phi

eps <- 1e-2              # Consider how big this loop is, I think it makes sense to 
diff <- 100              # just look at 0.01 level difference

# Run the actual convergence
while (diff > eps) {
  for (i in 1:N) {
    for(j in 1:M) {
      Pi[i,j] <- Y[i,j] / phi_old[i] * (Y0[j] + sum(Y[,j])) / (sum(Y[,j] / phi_old))
    }
  }
  
  for (i in 1:N) {
    index_i <- which(Y[i,] > 0)
    phi[i] <- mean(Y[i, index_i] / Pi[i, index_i])
  }
  
  diff <- max(max(abs(phi - phi_old)), max(abs(Pi - Pi_old)))
  Pi_old <- Pi
  phi_old <- phi
}

# STEP 4
# This is taking way too much time
# I'm gonna try to recode this in python and see whether it will be quicker
write.table(phi, "data/phi.txt")
write.table(Y0, "data/Y0.txt")
write.table(Y, "data/Y.txt")
