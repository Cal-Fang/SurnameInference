# Clear out the history
rm(list = ls())

# Read in the libraries needed
library(Matrix)

# Set working directory and read in the four datasets
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path

# Load in the data
load(file="data/AllData.Rdata")

cntrmatprop[is.na(cntrmatprop)] <- 0

# Create a function to get the numbers of a certain country origin
constructCountryNameList <- function(countrycode){
  counts <- cntrmat[, which(countrynames == countrycode)]
  props <- cntrmatprop[, which(countrynames == countrycode)]
  names(counts) <- names(props) <- surnames
  sortorder <- order(props, decreasing=TRUE)
  return(data.frame(
    fbcount = counts[sortorder],
    fbprop = props[sortorder],
    ntvcount = ntvmat[sortorder,1]
  ))
}

fb_estimates <- data.frame(
  fb_among_exclusive_surnames = rep(NA, N),
  exclusive_surnames = rep(NA, N),
  count_with_exclusive_surnames = rep(NA, N),
  row.names = countrynames
)

# Create a dataframe of foreign-born estmiates of exclusive names
for (i in 1:N){
  tmp <- constructCountryNameList(countrynames[i])
  tmp2 <- tmp$fbprop == 1
  tmp3 <- tmp[tmp2, ]
  total <- tmp3$fbcount + tmp3$ntvcount
  tmp4 <- tmp3$fbcount / total
  fb_estimates$fb_among_exclusive_surnames[i] <- sum(tmp4*total) / sum(total)
  fb_estimates$exclusive_surnames[i] <- length(tmp4)
  fb_estimates$count_with_exclusive_surnames[i] <- sum(total)
  # print(fb_estimates[i, ])
}

# Save the file just in case
tmp <- fb_estimates
tmp[,1] <- round(100*tmp[, 1], 1)
tmp <- tmp[order(tmp[, 3], decreasing=TRUE), ]
write.csv(tmp, file="data/ForeignBornAmongExclusiveSurnames2.csv")

# Rotate all matrices to match with the method doc
cntrmat <- t(cntrmat)
cntrmatprop <- t(cntrmatprop)
fbmat <- t(fbmat)
ntvmat <- t(ntvmat)
# terrmat <- t(terrmat)
# denommat <- t(denommat)

# Update the data file
save(cntrmat, cntrmatprop, 
     fbmat, ntvmat, us,
     surnames, M,
     countrynames, N,
     fb_estimates,
     file="data/AllData2.Rdata")
