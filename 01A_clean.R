## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Script name: 01A_clean.R
##
## Purpose of script: To clean the country code into some simpler codes for our analysis.
##
## Author: Cal Chengqi Fang
##
## Date Created: 2022-01-12
##
## Copyright (c) Cal Chengqi Fang, 2024
## Email: cal.cf@uchicago.edu
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Notes:
##   The result of this script dropped two codes by mistake, "NA" and "ZI", so I added a mutation 
## step when merging it with cntr. But NA stands for Netherlands Antilles and ZI stands for Zimbabwe 
## so neither would affect our final result anyway.
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
require(data.table)
require(stringr)
require(countrycode)    # For country-continent information
require(cgwtools)
require(xlsx)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ----------------------------------------------- STEP 1 ----------------------------------------------- 
# Read in the messy countrycode .csv file
countrycode_raw <- read.csv("data/SSA/countrycodeRaw.csv") 

# Format and decide the break by taking out all code position
countrycode_raw <- countrycode_raw %>% 
  mutate(allcode = str_replace_all(countrycode_raw$Code, c("\\["="", "\\]"="")),
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
  fill=TRUE
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


# ----------------------------------------------- STEP 2 ----------------------------------------------- 
# Create a continent column
SSAcountrycode <- SSAcountrycode %>% 
  mutate(continent = countrycode(sourcevar=countryname1,
                                 origin="country.name", destination="continent"))

# Filter out other continents
SSAcountrycode_AsiaNA <- SSAcountrycode %>% 
  filter(continent %in% c("Asia", NA))

nonAPI_list <- c("Antarctica", "Byelarus", "Bouvetoya", "Central African Empire", "Czechoslovakia", "Europa Island", 
                 "French Southern and Antarctic Lands", "French Territories of the Afars and Issas", "Glorioso Islands", 
                 "Clipperton Island", "Jan Mayen", "Juan de Nova Island", "Kosovo", "Spanish North Africa", 
                 "St. Martin", "St. Christopher-Nevis-Anguilla", "Swan Islands", "Spanish Sahara", 
                 "South Georgia and the South Sandwich Island", "Tromelin Island", "South-West Africa", "Berlin, West", 
                 "Yugoslavia ", "Yugoslavia")

SSAcountrycode_AAPI <- SSAcountrycode_AsiaNA %>% 
  filter(!(countryname1 %in% nonAPI_list))

# Keep the newest name
newest_AAPI <- SSAcountrycode_AAPI %>% 
  mutate(newestname = coalesce(countryname4, countryname3, countryname2, countryname1)) %>% 
  select(code, newestname)


# ----------------------------------------------- STEP 3 ----------------------------------------------- 
# Create a region column
SouthAsia <- c("Afghanistan", "Bangladesh", "Bhutan", "Maldives", "Nepal", "Sri Lanka",
               "British India", "India", "Pakistan", "Sikkim")
EastAsia <- c("China, Peoples Republic of", "Taiwan", "Hong Kong", "Macau (Macao)", 
              "Mongolia", 
              "Korea", "Korea, Republic of (South Korea)", "Korea, Democratic People’s Republic of (North Korea)",
              "Japan", "Southern Ryukyu Islands")
SoutheastAsia <- c("Cambodia", "Laos", "Burma (Myanmar)", "Malaysia", "Thailand", "Brunei", 
                   "Indonesia", "Philippines", "Singapore", 
                   "Timor-Leste (East Timor)", "Portuguese Timor",
                   "Vietnam", "Vietnam, Democratic Republic of", "Vietnam, Republic of",
                   "Cocos (Keeling) Islands",
                   "Spratly Islands", "Paracel Islands")
CentralAsia <- c("Kazakhstan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Uzbekistan")
WesternAsia <- c("Bahrain", "Iran", "Iraq", "Jordan", "Kuwait", "Lebanon", "Syria", "Oman", "Qatar", "Saudi Arabia", "United Arab Emirates", 
                 "Yemen (Sanaa)", "Yemen, Republic of", "Yemen (Aden)",
                 "Turkey", "Ottoman Empire",
                 "Palestine", "West Bank", "Gaza Strip",
                 "Israel", "Israel-Syria Demilitarized Zones", "Israel-Jordan Demilitarized Zones", "Iraq-Saudi Arabia Neutral Zone ",
                 "Azerbaijan", "Armenia", "Cyprus", "Georgia")
droplist <- c("Wake Island", "Midway Island", "Palmyra Atoll", "Kingman Reef", "British Indian Ocean Territory", "Heard Island and McDonald Islands", "Bassas da India",
              "Trust Territory of the Pacific Islands", "Gilbert Islands", "Gilbert and Ellice Islands", "Central and Southern Line Islands",
              "Canton and Enderbury Islands", "Coral Sea Islands", "Ashmore and Cartier Islands", 
              "Unknown")

# Filter by region
census_Asian <- newest_AAPI %>% 
  mutate(region = as.factor(ifelse(newestname %in% SouthAsia, "SouthAsia",
                                   ifelse(newestname %in% EastAsia, "EastAsia", 
                                          ifelse(newestname %in% SoutheastAsia, "SoutheastAsia", 
                                                 ifelse(newestname %in% CentralAsia, "CentralAsia", 
                                                        ifelse(newestname %in% WesternAsia, "WesternAsia", NA))))))) %>% 
  filter(!(newestname %in% droplist)) %>% 
  filter(region %in% c("SouthAsia", "EastAsia", "SoutheastAsia"))


# ----------------------------------------------- STEP 4 ----------------------------------------------- 
# Combine codes that were different because of SSA country code revision
census_Asian <- census_Asian %>% 
  mutate(code2 = ifelse(code %in% c("YQ", "JA"), "JA",
                        ifelse(code %in% c("SK", "XB", "IN"), "IN",
                               ifelse(code %in% c("XK", "KS", "KN"), "XK",
                                      ifelse(code %in% c("PT", "TT"), "TT",
                                             ifelse(code %in% c("VN", "VS", "VM"), "VM", code))))))

# Drop some countries that had no sufficient information in SSA dataset
census_Asian <- census_Asian %>% 
  filter(!(code2 %in% c("PG", "CK", "PF", "TT")))


# ----------------------------------------------- STEP 5 ----------------------------------------------- 
# Put all revision above into the cntr dataset
cntr <- read.fwf("data/SSA/srnmcntr.txt", c(12,2,6), col.names=c("surname", "country", "freq"), na.strings=c())
cntr_census <- cntr %>% 
  merge(census_Asian, by.x="country", by.y="code") %>% 
  filter(!is.na(code2))

# Create a function to test combing codes' impact on country-exclusive surnames
TestCountryCombn <- function(code_list, df=cntr_census){
  # Make sure to read in the cntr dataset as a dataframe named as cntr
  # And merge the code2 onto the cntr and rename it as cntr_census
  
  # Generate all combinations to be tested
  res <- Map(combn, list(code_list), seq_along(code_list), simplify=FALSE)
  test_list <- unlist(res, recursive=FALSE)
  
  result <- data.frame(freqSum = double(),
                       freqPercent = double(),
                       surnameCount = double(),
                       surnamePercent = double(),
                       country_tested = list())
  
  for (test_comb in test_list){
    # Get the subset of the country(s) to be tested and the complement subset
    df_tmp <- subset(df, code2 %in% test_comb)
    tmp_surname <- unique(df_tmp$surname)
    df_rest <- subset(df, !(code2 %in% test_comb))
    rest_surname <- unique(df_rest$surname)
    
    # Get the name only in the country(s) to be tested
    unique_surname <- setdiff(tmp_surname, rest_surname)
    
    # Generate a summary for this test unit
    unique_summary <- df_tmp %>% 
      mutate(unique = ifelse(surname %in% unique_surname, TRUE, FALSE)) %>% 
      summarise(freqSum = sum(freq[unique == TRUE]),
                freqPercent = freqSum / sum(freq),
                surnameCount = n_distinct(surname[unique == TRUE]),
                surnamePercent = surnameCount / n_distinct(surname),
                country_tested = paste(test_comb, collapse=', '))
    
    result <- rbind(result, unique_summary)
  }
  
  return(result)
}

# Test on the five potential combination lists
result1 <- TestCountryCombn(c("BX", "ID", "MV"))
result2 <- TestCountryCombn(c("ID", "MY", "RP"))
result3 <- TestCountryCombn(c("CH", "HK", "TW", "MC", "SN"))
result4 <- TestCountryCombn(c("IN", "PK", "BG", "NP"))
result5 <- TestCountryCombn(c("TH", "LA"))

# # Print out the test results for future reference
# write.xlsx(result1, file="results/tests/combinationTest.xlsx", sheetName="BX_ID", row.names=FALSE)
# write.xlsx(result2, file="results/tests/combinationTest.xlsx", sheetName="ID_MY_RP", append=TRUE, row.names=FALSE)
# write.xlsx(result3, file="results/tests/combinationTest.xlsx", sheetName="CH_HK_TW_MC_SN", append=TRUE, row.names=FALSE)
# write.xlsx(result4, file="results/tests/combinationTest.xlsx", sheetName="IN_PK_BG_NP", append=TRUE, row.names=FALSE)
# write.xlsx(result5, file="results/tests/combinationTest.xlsx", sheetName="TH_LA", append=TRUE, row.names=FALSE)

# Combine some codes according to the test results above
census_Asian <- census_Asian %>% 
  mutate(code3 = ifelse(code2 %in% c("BX", "ID"), "BIA",
                        ifelse(code2 %in% c("CH", "HK", "TW", "MC"), "GCA",
                               ifelse(code2 %in% c("IN", "PK", "BG"), "SAS", code2))))
    # We use


# ----------------------------------------------- STEP 6 ----------------------------------------------- 
# Create a new dataframe for the use of replacing the code in later analysis
code_replace <- census_Asian %>% 
  merge(SSAcountrycode[,"code"], all.y=TRUE) %>% 
  mutate(new_code = ifelse(is.na(code3), "Other", code3)) %>% 
  select(c(code, new_code))

# Write out the results for later code change use
write_csv(code_replace, file="data/interm/codeReplace.csv")

