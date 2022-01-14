library(tidyverse)
library(data.table)
library(stringr)
library(countrycode)
library(cgwtools)

# Set working directory
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path

# Read in the messy countrycode .csv file
countrycode_raw <- read.csv("data/countrycode_raw.csv") 

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
  names <- paste(prefix, suffix, sep="")
  colnamel <- append(colnamel, names)
}

colnames(SSAcountrycode) <- colnamel

# Create a continent column and filter out other continent
SSAcountrycode <- SSAcountrycode %>% 
  mutate(continent = countrycode(sourcevar = countryname1,
                                 origin = "country.name", destination = "continent")) %>% 
  filter(continent %in% c("Asia", NA))

nonAPI_list <- c("Antarctica", "Byelarus", "Bouvetoya", "Central African Empire", "Czechoslovakia",
               "Europa Island", "French Southern and Antarctic Lands", "French Territories of the Afars and Issas",
               "Glorioso Islands", "Clipperton Island", "Jan Mayen", "Juan de Nova Island", "Kosovo", "Spanish North Africa",
               "St. Martin", "St. Christopher-Nevis-Anguilla", "Swan Islands", "Spanish Sahara", "South Georgia and the South Sandwich Island",
               "Tromelin Island", "South-West Africa", "Berlin, West", "Yugoslavia ", "Yugoslavia")

SSAcountrycode_AAPI <- SSAcountrycode %>% 
  filter(!(countryname1 %in% nonAPI_list))

# Save the file just in case
# write.csv(SSAcountrycode_AAPI, file="data/SSAcountrycode_AAPI.csv")

# Keep the newest name
newest_AAPI <- SSAcountrycode_AAPI %>% 
  mutate(newestname = coalesce(countryname4, countryname3, countryname2, countryname1)) %>% 
  select(code, newestname)


# Read in the Rdata file from 01 script
load(file="data/AllData2.Rdata")

# Match the code to country name
setDT(fb_estimates, keep.rownames = TRUE)
fb_estimates <- fb_estimates %>% 
  merge(newest_AAPI, by.x = "rn", by.y = "code")

# Update the fb_estimates csv file
write.csv(fb_estimates, file="data/ForeignBornAmongExclusiveSurnames.csv")

resave(SSAcountrycode, file="data/AllData2.Rdata")