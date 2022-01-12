library(Matrix)

# Set working directory and read in the four datasets
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path

cntr <- read.fwf("data/srnmcntr.txt", c(12,2,6), col.names=c("surname","country","freq"), na.strings = c())
fb <- read.fwf("data/srnmfb.txt", c(12,8), col.names=c("surname","freq"))
ntv <- read.fwf("data/srnmntv.txt", c(12,8), col.names=c("surname","freq"))
terr <- read.fwf("data/srnmterr.txt", c(12,8), col.names=c("surname","freq"))


# Create vector of unique surnames and update factor levels for all objects
surnames <- sort(union(fb$surname, terr$surname))      # Take out all surnames appearing in foreign born or us territory files
N_surnames <- length(surnames)

cntr$surname <- factor(cntr$surname, levels=surnames)
fb$surname <- factor(fb$surname, levels=surnames)
ntv$surname <- factor(ntv$surname, levels=surnames)
terr$surname <- factor(terr$surname, levels=surnames)

# Create vector of country codes 
countrynames <- sort(unique(cntr$country))
N_countrynames <- length(countrynames)

cntr$country <- factor(cntr$country, levels=countrynames)

# Create US born column sparse matrix
ntvmat <- sparseMatrix(
  i = rep(1,sum(!is.na(ntv$surname))),
  j = as.integer(ntv$surname[!is.na(ntv$surname)]),
  x = as.integer(ntv$freq[!is.na(ntv$surname)]),
  dims = c(1,N_surnames))
rownames(ntvmat) = "US"
colnames(ntvmat) = surnames

# Create US terr column sparse matrix
terrmat <- sparseMatrix(
  i = rep(1,nrow(terr)),
  j = as.integer(terr$surname),
  x = as.integer(terr$freq),
  dims = c(1,N_surnames))
rownames(terrmat) = "Territory"
colnames(terrmat) = surnames

# Create foreign born column sparse matrix
fbmat <- sparseMatrix(
  i = rep(1,nrow(fb)),
  j = as.integer(fb$surname),
  x = as.integer(fb$freq),
  dims = c(1,N_surnames))
rownames(fbmat) = "Foreign"
colnames(fbmat) = surnames

# Create N_surnames by N_countrynames sparse matrix
cntrmat <- sparseMatrix(
  i = as.integer(cntr$country),
  j = as.integer(cntr$surname),
  x = as.integer(cntr$freq),
  dims = c(N_countrynames,N_surnames))
rownames(cntrmat) = countrynames
colnames(cntrmat) = surnames


# What's the matter with "Nguyen"?
sort(prop.table(cntrmat[, which.max(colSums(cntrmat))]))

# What about "Lauderdale"?
cntrmat[, grep("LAUDERDALE", surnames)]

# Combine US, Terr, and Foreign matrices
denommat <- rbind(ntvmat, terrmat, fbmat)

# Create country proportion matrix
gc()    # following command requires ~ 12GB working memory
cntrmatprop <- sweep(cntrmat, 1, fbmat[1, ] + terrmat[1, ], FUN="/")
gc()

# Save objects
save(cntrmat, cntrmatprop, 
     fbmat, ntvmat, terrmat, 
     surnames, N_surnames,
     countrynames, N_countrynames,
     file="data/AllData.Rdata")
