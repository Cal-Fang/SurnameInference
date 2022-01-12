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
ntvmat2 <- sparseMatrix(
  i = as.integer(ntv$surname[!is.na(ntv$surname)]),
  j = rep(1,sum(!is.na(ntv$surname))),
  x = as.integer(ntv$freq[!is.na(ntv$surname)]),
  dims = c(N_surnames,1))
rownames(ntvmat2) = surnames
colnames(ntvmat2) = "US"

# Create US terr column sparse matrix
terrmat2 <- sparseMatrix(
  i = as.integer(terr$surname),
  j = rep(1,nrow(terr)),
  x = as.integer(terr$freq),
  dims = c(N_surnames,1))
rownames(terrmat2) = surnames
colnames(terrmat2) = "Territory"

# Create foreign born column sparse matrix
fbmat2 <- sparseMatrix(
  i = as.integer(fb$surname),
  j = rep(1,nrow(fb)),
  x = as.integer(fb$freq),
  dims = c(N_surnames,1))
rownames(fbmat2) = surnames
colnames(fbmat2) = "Foreign"

# Create N_surnames by N_countrynames sparse matrix
cntrmat2 <- sparseMatrix(
  i = as.integer(cntr$surname),
  j = as.integer(cntr$country),
  x = as.integer(cntr$freq),
  dims = c(N_surnames, N_countrynames))
rownames(cntrmat2) = surnames
colnames(cntrmat2) = countrynames

a <- fbmat2[1, ] + terrmat2[1, ] == fbmat[, 1] + terrmat[, 1]


# Create country proportion matrix
gc()    # following command requires ~ 12GB working memory
cntrmat2prop <- sweep(cntrmat2, 1, fbmat2[1, ] + terrmat2[1, ], FUN="/")
gc()

# Save objects
save(cntrmat, cntrmatprop, 
     fbmat, ntvmat, terrmat, 
     surnames, N_surnames,
     countrynames, N_countrynames,
     file="data/AllData.Rdata")
