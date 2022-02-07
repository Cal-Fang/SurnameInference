# Clear out the history
rm(list = ls())

# Read in the libraries needed
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

# STEP 1
# Create a continent column and filter out confirmed other continents
SSAcountrycode <- SSAcountrycode %>% 
  mutate(continent = countrycode(sourcevar = countryname1,
                                 origin = "country.name", destination = "continent"))

SSAcountrycode_AsiaNA <- SSAcountrycode %>% 
  filter(continent %in% c("Asia", NA))

nonAPI_list <- c("Antarctica", "Byelarus", "Bouvetoya", "Central African Empire", "Czechoslovakia",
               "Europa Island", "French Southern and Antarctic Lands", "French Territories of the Afars and Issas",
               "Glorioso Islands", "Clipperton Island", "Jan Mayen", "Juan de Nova Island", "Kosovo", "Spanish North Africa",
               "St. Martin", "St. Christopher-Nevis-Anguilla", "Swan Islands", "Spanish Sahara", "South Georgia and the South Sandwich Island",
               "Tromelin Island", "South-West Africa", "Berlin, West", "Yugoslavia ", "Yugoslavia")

SSAcountrycode_AAPI <- SSAcountrycode_AsiaNA %>% 
  filter(!(countryname1 %in% nonAPI_list))

# Save the file just in case
# write.csv(SSAcountrycode_AAPI, file="data/SSAcountrycode_AAPI.csv")

# Keep the newest name
newest_AAPI <- SSAcountrycode_AAPI %>% 
  mutate(newestname = coalesce(countryname4, countryname3, countryname2, countryname1)) %>% 
  select(code, newestname)

# STEP 2
# Add region
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

census_Asian <- newest_AAPI %>% 
  mutate(region = as.factor(ifelse(newestname %in% SouthAsia, 'SouthAsia',
                                   ifelse(newestname %in% EastAsia, 'EastAsia', 
                                          ifelse(newestname %in% SoutheastAsia, 'SoutheastAsia', 
                                                 ifelse(newestname %in% CentralAsia, 'CentralAsia', 
                                                        ifelse(newestname %in% WesternAsia, 'WesternAsia', NA))))))) %>% 
  filter(!(newestname %in% droplist)) %>% 
  filter(region %in% c('SouthAsia', 'EastAsia', 'SoutheastAsia'))

# STEP 3
# Combine codes that were different because of SSA country code revision
census_Asian <- census_Asian %>% 
  mutate(code2 = ifelse(code %in% c("YQ", "JA"), "JA",
                        ifelse(code %in% c("SK", "XB", "IN"), "IN",
                               ifelse(code %in% c("XK", "KS", "KN"), "XK",
                                      ifelse(code %in% c("PT", "TT"), "TT",
                                             ifelse(code %in% c("VN", "VS", "VM"), "VM", code))))))

# STEP 4
# Drop some countries that had no sufficient information in SSA dataset
census_Asian <- census_Asian %>% 
  filter(!(code2 %in% c("PG", "CK", "PF", "TT")))

# STEP 5
# Put all revision above onto the cntr dataset
cntr <- read.fwf("data/srnmcntr.txt", c(12,2,6), col.names=c("surname","country","freq"), na.strings = c())
cntr_census <- cntr %>% 
  merge(census_Asian, by.x = "country", by.y = "code") %>% 
  filter(!is.na(code2))

# Create a function to test the impact on the exclusive names result of different ways of combining codes
TestCountryCombn <- function(code_list, df=cntr_census){
  # Make sure to read in the cntr dataset as a dataframe named as cntr
  # And merge the code2 onto the cntr and rename it as cntr_census
  
  # Generate all combinations to be tested
  res <- Map(combn, list(code_list), seq_along(code_list), simplify = FALSE)
  test_list <- unlist(res, recursive = FALSE)
  
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
    
    # Generate a summary fir this test unit
    unique_summary <- df_tmp %>% 
      mutate(unique = ifelse(surname %in% unique_surname, TRUE, FALSE)) %>% 
      summarise(freqSum = sum(freq[unique == TRUE]),
                freqPercent = freqSum / sum(freq),
                surnameCount = n_distinct(surname[unique == TRUE]),
                surnamePercent = surnameCount / n_distinct(surname),
                country_tested = paste(test_comb, collapse=', ')
                )
    
    result <- rbind(result, unique_summary)
  }
  
  return(result)
}

# Test on the five potential combination lists
# test1 <- c("BX", "ID")
# result1 <- TestCountryCombn(test1)
# 
# test2 <- c("ID", "MY", "RP")
# result2 <- TestCountryCombn(test2)
# 
# test3 <- c("CH", "HK", "TW", "MC", "SN")
# result3 <- TestCountryCombn(test3)
# 
# test4 <- c("IN", "PK", "BG", "NP")
# result4 <- TestCountryCombn(test4)
# 
# test5 <- c("TH", "LA")
# result5 <- TestCountryCombn(test5)

# # Print out the test results
# library(xlsx)
# 
# write.xlsx(result1, file="testresults.xlsx", sheetName="BX_ID", row.names=FALSE)
# write.xlsx(result2, file="testresults.xlsx", sheetName="ID_MY_RP", append=TRUE, row.names=FALSE)
# write.xlsx(result3, file="testresults.xlsx", sheetName="CH_HK_TW_MC_SN", append=TRUE, row.names=FALSE)
# write.xlsx(result4, file="testresults.xlsx", sheetName="IN_PK_BG_NP", append=TRUE, row.names=FALSE)
# write.xlsx(result5, file="testresults.xlsx", sheetName="TH_LA", append=TRUE, row.names=FALSE)

# Combine some codes according to the test results above
census_Asian <- census_Asian %>% 
  mutate(code3 = ifelse(code2 %in% c("BX", "ID"), "GDA",
                        ifelse(code2 %in% c("CH", "HK", "TW", "MC"), "GCA",
                               ifelse(code2 %in% c("IN", "PK", "BG"), "GIA", code2))))

# Create a new dataframe for the use of replacing the code in later analysis
code_replace <- census_Asian %>% 
  merge(SSAcountrycode[,"code"], all.y=TRUE) %>% 
  mutate(new_code = ifelse(is.na(code3), "Other", code3)) %>% 
  select(c(code, new_code))

write_csv(code_replace, file="data/code_replace.csv")
# Later found this file mistakenly dropped two codes, "NA" and "ZI", 
# So I added a mutation step later when merging this with cntr

