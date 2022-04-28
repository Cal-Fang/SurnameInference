# Clear out the history
rm(list=ls())

# Read in the packages needed
library(Matrix)
library(tidyverse)

# Set working directory
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path

# STEP 1
cntr <- read.fwf("data/srnmcntr.txt", c(12,2,6), col.names=c("surname","country","freq"), na.strings = c())
terr <- read.fwf("data/srnmterr.txt", c(12,8), col.names=c("surname","freq"))
ntv <- read.fwf("data/srnmntv.txt", c(12,8), col.names=c("surname","freq"))
fb <- cntr %>% 
  filter(freq > 2) %>% 
  group_by(surname) %>% 
  summarise(freq = sum(freq))

save(cntr, terr, ntv, fb,
     file="data/OriginalData.Rdata")
load(file="data/OriginalData.Rdata")

# Drop all records that actually have no surname
noname <- c("NO NAME     ", "NO GIVEN NAM", "NO LAST NAME", "NO NAME GIVE")

cntr <- filter(cntr, !(surname %in% noname))
terr <- filter(terr, !(surname %in% noname))
ntv <- filter(ntv, !(surname %in% noname))
fb <- filter(fb, !(surname %in% noname))

# Combine all multiple-word names into single-word names
com_name <- function(df, country=FALSE){
  df$surname <- gsub(" ", "", df$surname)
  df$surname <- gsub("\\s", " ", format(df$surname, width=12))
  
  if (country) {
    df_comb <- df %>% 
      group_by(surname, country) %>% 
      summarise(freq = sum(freq))
  } else {
    df_comb <- df %>% 
      group_by(surname) %>% 
      summarise(freq = sum(freq))
  }
  
  return(df_comb)
}

cntr <- com_name(cntr, TRUE)
terr <- com_name(terr)
ntv <- com_name(ntv)
fb <- com_name(fb)

fb_terr_ntv <- fb %>% 
  merge(terr, by="surname", all=TRUE) %>% 
  merge(ntv, by="surname", all=TRUE)

# STEP 2
# Filter out all surnames of which the total frequency is not bigger than 50
fb_terr_ntv$sum_freq <- rowSums(fb_terr_ntv[, -1], na.rm=TRUE)
fb_terr_ntv$fb_prop <- fb_terr_ntv$freq.x / fb_terr_ntv$sum_freq

# ggplot(fb_terr_ntv, aes(x=sum_freq)) +
#   geom_bar() +
#   xlim(1, 100)
# sum(fb_terr_ntv$sum_freq >= 10) / nrow(fb_terr_ntv)

fb_terr_ntv2 <- filter(fb_terr_ntv, sum_freq>50)

# STEP 3
# Filter out all surnames of which the total frequency of the 19 Asian countries is 
# smaller or equal to 10
code_replace <- read_csv("data/code_replace.csv")

cntr <- cntr %>% 
  merge(code_replace, by.x="country", by.y="code", all.x=TRUE) %>% 
  mutate(new_code = ifelse(is.na(new_code), "Other", new_code))

asia <- cntr %>%
  filter(new_code != "Other") %>% 
  group_by(surname) %>% 
  summarise(t_asia = sum(freq)) %>% 
  filter(t_asia > 10) 

asiansurnames <- sort(asia$surname)  

fb_terr_ntv3 <- filter(fb_terr_ntv2, surname %in% asiansurnames)

# STEP 4
# Filter out the surnames of which the "Other" category is the most frequent origin
cntr0 <- cntr
cntr0$surname <- factor(cntr0$surname, levels=sort(unique(fb_terr_ntv3$surname)))
cntr0$country <- factor(cntr0$new_code, levels=sort(unique(cntr0$new_code)))
cntr0 <- cntr0 %>%
  filter(!is.na(surname))
cntr0mat <- sparseMatrix(
  i = as.integer(cntr0$surname),
  j = as.integer(cntr0$country),
  x = as.integer(cntr0$freq),
  dims = c(length(fb_terr_ntv3$surname), 20))
rownames(cntr0mat) = sort(unique(fb_terr_ntv3$surname))
colnames(cntr0mat) = sort(unique(cntr0$new_code))

asianmax <- as.data.frame(as.matrix(cntr0mat)) %>% 
  mutate(largest = apply(cntr0mat, 1, max, na.rm = TRUE),
         surname = rownames(cntr0mat)) %>% 
  filter(!(Other == largest))

fb_terr_ntv4 <- filter(fb_terr_ntv3, surname %in% asianmax$surname)


fb_terr_ntv4.1 <- filter(fb_terr_ntv4, fb_prop>0.05)

# STEP 5
# Recover the surnames to the 12 digits form
surnames <- gsub("\\s", " ", format(fb_terr_ntv4$surname, width=12))
# Save objects
save(cntr, ntv,
     surnames, 
     file="data/Clean2Data.Rdata")


# countrynames <- unique(code_replace$new_code)
# for (i in countrynames) {
#   total <- sum(asianmax$largest == asianmax[i])
#   
#   print(i)
#   print(total)
# }
