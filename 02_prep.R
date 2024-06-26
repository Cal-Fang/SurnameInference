## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Script name: 02_prep.R
##
## Purpose of script: To create sparse matrices for EM algorithm.
##
## Author: Cal Chengqi Fang
##
## Date Created: 2022-06-20
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
# Read in the after clean2 dataset 
load(file="data/interm/cleaned.Rdata")


# ----------------------------------------------- STEP 2 ----------------------------------------------- 
# Create vector of unique surnames and update factor levels for all objects
# We would use the outcome of the clean2 script
M <- length(surnames)                                  

cntr$surname <- factor(cntr$surname, levels=surnames)
ntv$surname <- factor(ntv$surname, levels=surnames)

# Create vector of country codes 
countrynames <- sort(unique(cntr$new_code))
N <- length(countrynames)

cntr$new_code <- factor(cntr$new_code, levels=countrynames)


# ----------------------------------------------- STEP 3 ----------------------------------------------- 
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
rownames(cntrmat) <- surnames
colnames(cntrmat) <- countrynames

# Rotate all matrices needed for further analysis
cntrmat <- t(cntrmat)
ntvmat <- t(ntvmat)


# ----------------------------------------------- STEP 4 -----------------------------------------------
# Save objects
save(cntrmat, ntvmat,
     surnames, M,
     countrynames, N, 
     file="data/interm/formatted.Rdata")

