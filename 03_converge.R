library(Matrix)

# Set working directory and read in the four datasets
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path

# Load in the data
load(file="data/AllData.Rdata")
estimates_exc <- read.csv("data/ForeignBornAmongExclusiveSurnames.csv")

# Build the convergence loop

cntrmat[1:3, 1:3]



# library(doParallel)
# 
# # we specify the number of cores/workers we want to use
# n_cores <- detectCores() - 1
# n_cores
