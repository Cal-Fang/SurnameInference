library(Matrix)

# Set working directory and read in the four datasets
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path

cntr <- read.fwf("data/srnmcntr.txt", c(12,2,6), col.names=c("surname","country","freq"), na.strings = c())
# fb <- read.fwf("data/srnmfb.txt", c(12,8), col.names=c("surname","freq"))
ntv <- read.fwf("data/srnmntv.txt", c(12,8), col.names=c("surname","freq"))
terr <- read.fwf("data/srnmterr.txt", c(12,8), col.names=c("surname","freq"))

# Turns out cntr has dropped all combination of surnames and countries less than 3
# So we will need to make a new fb dataframe here
fb <- cntr %>% 
  filter(freq > 2) %>% 
  group_by(surname) %>% 
  summarise(freq = sum(freq))

# Also need to combine ntv and terr cuz it just makes more sense
us <- ntv %>% 
  merge(terr, by="surname", all=TRUE) %>% 
  mutate(freq.x = replace_na(freq.x, 0),
         freq.y = replace_na(freq.y, 0),
         freq = freq.x + freq.y) %>% 
  select(-c(freq.x, freq.y))

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
  i = as.integer(ntv$surname[!is.na(ntv$surname)]),
  j = rep(1,sum(!is.na(ntv$surname))),
  x = as.integer(ntv$freq[!is.na(ntv$surname)]),
  dims = c(N_surnames,1))
rownames(ntvmat) = surnames
colnames(ntvmat) = "US"

# Create US terr column sparse matrix
terrmat <- sparseMatrix(
  i = as.integer(terr$surname),
  j = rep(1,nrow(terr)),
  x = as.integer(terr$freq),
  dims = c(N_surnames,1))
rownames(terrmat) = surnames
colnames(terrmat) = "Territory"

# Create foreign born column sparse matrix
fbmat <- sparseMatrix(
  i = as.integer(fb$surname),
  j = rep(1,nrow(fb)),
  x = as.integer(fb$freq),
  dims = c(N_surnames,1))
rownames(fbmat) = surnames
colnames(fbmat) = "Foreign"

# Create N_surnames by N_countrynames sparse matrix
cntrmat <- sparseMatrix(
  i = as.integer(cntr$surname),
  j = as.integer(cntr$country),
  x = as.integer(cntr$freq),
  dims = c(N_surnames, N_countrynames))
rownames(cntrmat) = surnames
colnames(cntrmat) = countrynames


# What's the matter with "Nguyen"?
sort(prop.table(cntrmat[which.max(rowSums(cntrmat)), ]))

# What about "Lauderdale"?
cntrmat[grep("LAUDERDALE", surnames), ]

# Combine US, Terr, and Foreign matrices
denommat <- cbind(ntvmat,terrmat,fbmat)

# Create country proportion matrix
gc()    # following command requires ~ 12GB working memory
cntrmatprop <- sweep(cntrmat, 1, fbmat[, 1] + terrmat[, 1], FUN="/")
gc()

# Save objects
save(cntrmat, cntrmatprop, 
     fbmat, ntvmat, terrmat, denommat,
     surnames, N_surnames,
     countrynames, N_countrynames,
     file="data/AllData.Rdata")
