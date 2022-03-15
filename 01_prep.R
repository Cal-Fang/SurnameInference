# Clear out the history
rm(list=ls())

# Read in the packages needed
library(Matrix)
library(tidyverse)

# Set working directory
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path

# STEP 1 
# Read in the dataset and recode the country codes
cntr <- read.fwf("data/srnmcntr.txt", c(12,2,6), col.names=c("surname","country","freq"), na.strings = c())
# fb <- read.fwf("data/srnmfb.txt", c(12,8), col.names=c("surname","freq"))
ntv <- read.fwf("data/srnmntv.txt", c(12,8), col.names=c("surname","freq"))
terr <- read.fwf("data/srnmterr.txt", c(12,8), col.names=c("surname","freq"))

# Turns out cntr has dropped all combination of surnames and countries less than 3
# So we will need to make a new fb dataframe here
fb <- cntr %>% 
  filter(freq > 2) %>% 
  group_by(surname) %>% 
  summarise(freq = sum(freq))

# Apply the data cleaning result onto the cntr dataset to obtain the new_code variable
code_replace <- read_csv("data/code_replace.csv")

cntr <- cntr %>% 
  merge(code_replace, by.x="country", by.y="code", all.x=TRUE) %>% 
  mutate(new_code = ifelse(is.na(new_code), "Other", new_code))

# STEP 2 
# Create vector of unique surnames and update factor levels for all objects
# We would only care if at least one person with that surname was from the 15 Asian countries
asia <- cntr %>%
  filter(new_code != "Other") %>% 
  group_by(surname) %>% 
  summarise(t_asia = sum(freq)) %>% 
  filter(t_asia > 0) 
surnames <- sort(asia$surname)       
M <- length(surnames)                                  

cntr$surname <- factor(cntr$surname, levels=surnames)
fb$surname <- factor(fb$surname, levels=surnames)
ntv$surname <- factor(ntv$surname, levels=surnames)
terr$surname <- factor(terr$surname, levels=surnames)

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

# Create US terr column sparse matrix
terrmat <- sparseMatrix(
  i = as.integer(terr$surname[!is.na(terr$surname)]),
  j = rep(1, sum(!is.na(terr$surname))),
  x = as.integer(terr$freq[!is.na(terr$surname)]),
  dims = c(M,1))
rownames(terrmat) = surnames
colnames(terrmat) = "USO"

# Create foreign born column sparse matrix
fbmat <- sparseMatrix(
  i = as.integer(fb$surname[!is.na(fb$surname)]),
  j = rep(1, sum(!is.na(fb$surname))),
  x = as.integer(fb$freq[!is.na(fb$surname)]),
  dims = c(M,1))
rownames(fbmat) = surnames
colnames(fbmat) = "Foreign"

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

# Take oversea territories as a foreign county
# To prepare a matrix with territories data for experimental run
cntrmat_withUSO <- cbind(cntrmat, terrmat)
countrynames2 <- append(countrynames, "USO")
N2 <- N + 1

# Create country proportion matrix
gc()    # following command requires ~ 12GB working memory
cntrmatprop <- sweep(cntrmat, 1, fbmat[, 1] + terrmat[, 1], FUN="/")       # Just in case there were names in US territories
gc()                                                                       # identified as exclusive to the 19 asian countries 


# STEP 4
# Save objects
save(cntrmat, cntrmat_withUSO,
     cntrmatprop, terrmat, # denommat,
     fbmat, ntvmat,
     surnames, M,
     countrynames, N, countrynames2, N2,
     file="data/AllData.Rdata")

# Random tests
# What's the matter with "Nguyen"?
# sort(prop.table(cntrmat[which.max(rowSums(cntrmat)), ]))

# What about "Lauderdale"?
# cntrmat[grep("LAUDERDALE", surnames), ]

# What about "Fang"?
# cntrmatprop["FANG        ",]
