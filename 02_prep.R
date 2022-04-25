# Clear out the history
rm(list=ls())

# Read in the packages needed
library(Matrix)
library(tidyverse)

# Set working directory
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path

# STEP 1 
# Read in the after clean2 dataset
load(file="data/Clean2Data.Rdata")

# STEP 2 
# Create vector of unique surnames and update factor levels for all objects
# We would use the outcome of the clean2 script
M <- length(surnames)                                  

cntr$surname <- factor(cntr$surname, levels=surnames)
ntv$surname <- factor(ntv$surname, levels=surnames)

# Create vector of country codes 
countrynames <- sort(unique(cntr$new_code))
N <- length(countrynames)

cntr$new_code <- factor(cntr$new_code, levels=countrynames)

# STEP 3
# Create US born column sparse matrix
ntvmat <- sparseMatrix(
  i = as.integer(ntv$surname[!is.na(ntv$surname)]),
  j = rep(1, sum(!is.na(ntv$surname))),
  x = as.integer(ntv$freq[!is.na(ntv$surname)]),
  dims = c(M,1))
rownames(ntvmat) <- surnames
colnames(ntvmat) <- "US"

# Create M by N sparse matrix
cntr <- cntr %>%
 filter(!is.na(surname))
cntrmat <- sparseMatrix(
  i = as.integer(cntr$surname),
  j = as.integer(cntr$new_code),
  x = as.integer(cntr$freq),
  dims = c(M, N))
rownames(cntrmat) = surnames
colnames(cntrmat) = countrynames


# STEP 4
# Rotate all matrices needed for further analysis
cntrmat <- t(cntrmat)
ntvmat <- t(ntvmat)
# Save objects
save(cntrmat, ntvmat,
     surnames, M,
     countrynames, N, 
     file="data/PrepData.Rdata")

# Random tests
# What's the matter with "Nguyen"?
# sort(prop.table(cntrmat[which.max(rowSums(cntrmat)), ]))

# What about "Lauderdale"?
# cntrmat[grep("LAUDERDALE", surnames), ]

