library(Matrix)
library(tidyverse)

# Set working directory and read in the four datasets
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path

# Load in the data
load(file="data/AllData.Rdata")
load(file="data/AllData2.Rdata")
load(file="data/AllData3.Rdata")

# Create test dataset first 
# Use TH ("Thailand") for this test by the way
test1 <- cntr %>% 
  filter(country == "TH")

sur_test <- sort(unique(test1$surname))
cntr_test <- c("AF", "BM", "BT", "CB", "CE", "JA", "LA", "MG", "MV", "MY", "NP", "RP", "SN", "TH", "VM", "XK")    # Drop "GCA", "GDA", "GIA" for this test

test2 <- us %>% 
  filter(surname %in% sur_test)

# test4 contain all surnames appear in TH tests but recategorize all country codes not in census_Asia into "Other"
test4 <- cntr %>% 
  filter(surname %in% sur_test) %>% 
  merge(census_Asian[, c("code", "code3")], by.x="country", by.y="code", all=TRUE)
test4.1 <- test4 %>% 
  filter(is.na(code3)) %>% 
  group_by(surname) %>% 
  summarise(freq = sum(freq),
            country = "Other")
test4 <- test4 %>% 
  filter(!is.na(code3)) %>% 
  select(-country) %>% 
  rename(country = code3) %>% 
  rbind(test4.1) %>% 
  drop_na() %>% 
  filter(country %in% cntr_test)

# transform the fb_estimates just for the test
fb_estimates2 <- fb_estimates %>%
  cbind(code = rownames(fb_estimates)) %>% 
  drop_na() %>% 
  select(-c("exclusive_surnames", "count_with_exclusive_surnames"))
rownames(fb_estimates2) <- 1:nrow(fb_estimates2)

phi_df <- fb_estimates2 %>% 
  merge(census_Asian[, c("code", "code3")], all.x=TRUE) %>% 
  drop_na() %>%
  rename(country = code3,
         phi = "fb_among_exclusive_surnames") %>% 
  select(-c(code)) %>% 
  filter(country %in% cntr_test)

phi_df <- phi_df[order(phi_df$country), ]

# Make sure the Y0 has the same length
Y0_df <- test2[order(test2$surname), ]
df <- as.data.frame(sur_test)
colnames(df) <- "surname"
Y0_df <- Y0_df %>% 
  merge(df, all=TRUE)
Y0_df[is.na(Y0_df)] <- 0

# Test the convergence loop on several values
N <- length(cntr_test)
M <- length(sur_test)

Pi = matrix(NA, nrow = N, ncol = M)

phi <- phi_df$phi
Y = matrix(rnorm(N*M), nrow = N, ncol = M)
Y0 = Y0_df$freq

test4$surname <- factor(test4$surname, levels=sur_test)
test4$country <- factor(test4$country, levels=cntr_test)
Y <- sparseMatrix(
  i = as.integer(test4$surname),
  j = as.integer(test4$country),
  x = as.integer(test4$freq),
  dims = c(length(sur_test), length(cntr_test)))

for (i in 1:N) {
  for(j in 1:M) {
    Pi[i,j] = Y[i,j] / phi[i] * (Y0[j] + sum(Y[,j])) / (sum(Y[,j] / phi))
  }
}

for (i in 1:N) {
  index_i = which(Y[i,] > 0)
  phi[i] = mean(Y[i, index_i] / Pi[i, index_i])
}

Pi_old = Pi
phi_old = phi

eps = 1e-2
diff = 100

while (diff > eps) {
  for (i in 1:N) {
    for(j in 1:M) {
      Pi[i,j] = Y[i,j] / phi_old[i] * (Y0[j] + sum(Y[,j])) / (sum(Y[,j] / phi_old))
    }
  }
  
  for (i in 1:N) {
    index_i = which(Y[i,] > 0)
    phi[i] = mean(Y[i, index_i] / Pi[i, index_i])
  }
  
  diff = max(max(abs(phi - phi_old)), max(abs(Pi - Pi_old)))
  Pi_old = Pi
  phi_old = phi
}

Pi[11, ]
