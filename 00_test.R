library(Matrix)

# Set working directory and read in the four datasets
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path

# Load in the data
load(file="data/AllData2.Rdata")

# Test the convergence loop on several values
for (i in countrynames){
  pi_ij <- Y_ij / phi_i * (Y_0j + sum(Y_ij)) / sum(Y_ij / phi_i)
    for (j in surnames){
      Y_ij_frame = Y_ij_frame %>% 
        filter(Y_ij > 0)
      phi_i = sum(Y_ij / pi_ij) / length(Y_ij)
    }
}
  





# library(doParallel)
# 
# # Specify the number of cores/workers we want to use
# n_cores <- detectCores() - 1
# n_cores

cntr <- read.fwf("data/srnmcntr.txt", c(12,2,6), col.names=c("surname","country","freq"), na.strings = c())

cntr %>% 
  filter(country == "BX") %>% 
  summarize(sum = sum(freq))

cntr %>% 
  filter(country == "YQ") 

""



cntr %>% 
  filter(surname == "CUNHA       ")















