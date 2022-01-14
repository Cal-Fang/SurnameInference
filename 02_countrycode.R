library(tidyverse)
library(data.table)
library(stringr)
library(countrycode)

# Set working directory
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path

# Read in the messy countrycode .csv file
countrycode_raw <- read.csv("data/countrycode.csv") 

# Format and decide the break by taking out all code position
countrycode_raw <- countrycode_raw %>% 
  mutate(allcode = str_replace_all(countrycode_raw$Code, c("\\[" = "", "\\]" = "")),
         slen = nchar(allcode)) %>% 
  filter(slen != 1)

breakn <- which(countrycode_raw$slen == 2)
codelist <- list()

# Break down the code column and form a list
for (i in 1:(length(breakn)-1)) {
  head <- breakn[[i]]
  tail <- breakn[[(i+1)]]
  templ <- countrycode_raw$allcode[head:tail]
  codelist[[length(codelist)+1]] <- head(templ, -1)
}

# Rbind the list into a new dataframe
SSAcountrycode <- rbindlist(
  lapply(codelist, function(x) data.table(t(x))),
  fill = TRUE
)

# Rename the dataframe
colnamel <- c("code")
prefix <- c("countryname", "startdate", "enddate")
for (i in 1:((ncol(SSAcountrycode)-1)/3)) {
  suffix <- as.character(i)
  names <- paste(prefix, suffix)
  colnamel <- append(colnamel, names)
}

colnames(SSAcountrycode) <- colnamel


