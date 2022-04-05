# Clear out the history
rm(list=ls())

# Read in the packages needed
library(Matrix)
library(tidyverse)

# Set working directory and load in the Rdata file generated from analysis script
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path
load(file="data/result_1e-3.Rdata")

result_p3 <- t(t(result3)/colSums(result3))
rownames(result_p3) <- countrynames

p3_df <- as.data.frame(t(result_p3)) %>% 
  mutate(largest = apply(result_p3, 2, max, na.rm = TRUE),
         surname = surnames) 

# STEP 1
hist(p3_df$Other)

# Filter out surnames whose probability of "Other" is larger than 0.5
p3_df2 <- p3_df %>% 
  filter(Other <= 0.5) 


# STEP 2
# Filter out surnames of which the most possible country of origin is "Other"
p3_df3 <- p3_df2 %>% 
  filter(largest != Other)
# SumTopThree <- function(vec) {
#   sortVec <- vec[order(vec, decreasing=TRUE)]
#   return(sum(sortVec[1:3]))
# }
# 
# p3_df$largest3deo <- apply(result_p3[-15,], 2, SumTopThree)
# 
# hist(p3_df$largest3deo)
# sum(p3_df$largest3deo >= 0.6) / nrow(p3_df)
# 
# p3_df3 <- p3_df %>% 
#   filter(largest3deo >= 0.6) %>% 
#   merge(p3_df2)


# STEP 3
cntr <- read.fwf("data/srnmcntr.txt", c(12,2,6), col.names=c("surname","country","freq"), na.strings = c())
terr <- read.fwf("data/srnmterr.txt", c(12,8), col.names=c("surname","freq"))
ntv <- read.fwf("data/srnmntv.txt", c(12,8), col.names=c("surname","freq"))
fb <- cntr %>% 
  filter(freq > 2) %>% 
  group_by(surname) %>% 
  summarise(freq = sum(freq))

fb_terr_ntv <- fb %>% 
  merge(terr, by="surname", all=TRUE) %>% 
  merge(ntv, by="surname", all=TRUE)
fb_terr_ntv$sum_freq <- rowSums(fb_terr_ntv[,-1], na.rm=TRUE)
fb_terr_ntv$us_prop <- fb_terr_ntv$freq.x / fb_terr_ntv$sum_freq

ggplot(fb_terr_ntv, aes(x=sum_freq)) +
  geom_bar() +
  xlim(1, 100)
sum(fb_terr_ntv$sum_freq >= 10) / nrow(fb_terr_ntv)

p3_df4 <- p3_df3 %>% 
  merge(fb_terr_ntv[c("surname", "sum_freq", "us_prop")], all.x=TRUE) %>% 
  filter(sum_freq >= 20) 


# To help better observe the outcome 
# Take out the country codes of all the biggest possibility value
p3_df4 <- p3_df4 %>% 
  mutate(maxcntr_list = lapply(apply(p3_df4[2:21], 1, function(x) which(x == max(x))), names)) %>% 
  mutate(maxcntr = unlist(lapply(maxcntr_list, paste, collapse=" & "))) %>% 
  select(-"maxcntr_list")
# Make all smallest numbers 0
p3_df4[2:21] <- t(apply(p3_df4[2:21], 1, function(x) 
  replace(x, x == min(x), 0)))

write_csv(p3_df4, "data/p3_df4.csv")
