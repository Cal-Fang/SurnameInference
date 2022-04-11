# Clear out the history
rm(list=ls())

# Read in the packages needed
library(Matrix)
library(tidyverse)

# Set working directory and load in the Rdata file generated from analysis script
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path

# STEP 1
cntr <- read.fwf("data/srnmcntr.txt", c(12,2,6), col.names=c("surname","country","freq"), na.strings = c())
terr <- read.fwf("data/srnmterr.txt", c(12,8), col.names=c("surname","freq"))
ntv <- read.fwf("data/srnmntv.txt", c(12,8), col.names=c("surname","freq"))
fb <- cntr %>% 
  filter(freq > 2) %>% 
  group_by(surname) %>% 
  summarise(freq = sum(freq))

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
fb_terr_ntv$sum_freq <- rowSums(fb_terr_ntv[,-1], na.rm=TRUE)
fb_terr_ntv$us_prop <- fb_terr_ntv$freq.x / fb_terr_ntv$sum_freq

# ggplot(fb_terr_ntv, aes(x=sum_freq)) +
#   geom_bar() +
#   xlim(1, 100)
# sum(fb_terr_ntv$sum_freq >= 10) / nrow(fb_terr_ntv)

fb_terr_ntv2 <- filter(fb_terr_ntv, sum_freq>50)


# STEP 3
# Filter out all surnames of which the total frequency of the 19 Asian countries is 0
# Apply the data cleaning result onto the cntr dataset to obtain the new_code variable
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
cntr$surname <- factor(cntr$surname, levels=sort(unique(fb_terr_ntv3$surname)))
cntr$country <- factor(cntr$new_code, levels=sort(unique(cntr$new_code)))
cntr <- cntr %>%
  filter(!is.na(surname))
cntrmat <- sparseMatrix(
  i = as.integer(cntr$surname),
  j = as.integer(cntr$country),
  x = as.integer(cntr$freq),
  dims = c(length(fb_terr_ntv3$surname), 20))
rownames(cntrmat) = sort(unique(fb_terr_ntv3$surname))
colnames(cntrmat) = sort(unique(cntr$new_code))

asianmax <- as.data.frame(as.matrix(cntrmat)) %>% 
  mutate(largest = apply(cntrmat, 1, max, na.rm = TRUE),
         surname = rownames(cntrmat)) %>% 
  filter(!(Other == largest))
fb_terr_ntv4 <- filter(fb_terr_ntv3, surname %in% asianmax$surname)

# jp <- asianmax %>% 
#   filter(largest == JA)

jp2 <- asianmax %>% 
  filter(largest == JA)

# # To help better observe the outcome 
# # Take out the country codes of all the biggest possibility value
# p3_df4 <- p3_df4 %>% 
#   mutate(maxcntr_list = lapply(apply(p3_df4[2:21], 1, function(x) which(x == max(x))), names)) %>% 
#   mutate(maxcntr = unlist(lapply(maxcntr_list, paste, collapse=" & "))) %>% 
#   select(-"maxcntr_list")
# # Make all smallest numbers 0
# p3_df4[2:21] <- t(apply(p3_df4[2:21], 1, function(x) 
#   replace(x, x == min(x), 0)))
# 
# write_csv(p3_df4, "data/p3_df4.csv")
