# Clear out the history
rm(list=ls())

# Read in the packages needed
library(Matrix)
library(tidyverse)

# Set working directory
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path

# STEP 1
# Load in the Rdata file of reportable results
load(file="data/ReportResult.Rdata")
# Read in the CENSUS
census100 <- read_csv("data/Names_2010Census.csv")

# STEP 2
# Check the inclusion