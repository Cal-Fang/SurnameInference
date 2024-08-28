##############################
# KPNC Surname List Validation
# Author: Rishi Parikh
##############################

#Libraries
library(dplyr)
library(haven)

#Datasets
##############################

#Code highest probability ethnicity and second highest (with >20% probability)
namelist = read.csv("~/goproject/projects/panache/data/surname_validation/probList.csv")

second_highest_value <- function(row) {
  sorted_values <- order(row, decreasing = TRUE)
  return(row[sorted_values[2]])
}

second_highest_column <- function(row) {
  sorted_values <- order(row, decreasing = TRUE)
  return(sorted_values[2])
}

names = namelist %>%
  rowwise() %>%
  mutate(Highest_p = names(.)[which.max(c_across(AF:XK))],
         Second_p = ifelse(second_highest_value(c_across(AF:XK)) > 0.2, names(.)[second_highest_column(c_across(AF:XK))], NA),
         surname = str_trim(surname))


#Surnames in this cohort are stripped of white space and in all caps
cohort = read_sas("~/goproject/projects/panache/data/surname_validation/surname_cohort_final.sas7bdat")

#Re-run with sex filters for stratified performance estimates
#cohort = filter(cohort, SEX_ADMIN == 'M')

#Exclude multi-ethnic Asian and Unknown asian from specific ethnicity denominator
cohort_known = filter(cohort, !origin %in% c('MA','UA'))

#Calculate performance for highest ethnicity
##############################

#Create empty results df
results = data.frame(matrix(ncol=7, nrow=0))

#Overall Asian performance
cohort = mutate(cohort, surname_match = if_else(surname %in% names$surname, 1, 0))

#Function- for re-weighting, separate by Asian vs Non-Asian
perf = function(df, truth, test) {
  conf_matrix <- table(df[[truth]], df[[test]], df[['any_asian']])
  TP_A <- conf_matrix[2, 2, 2]
  TP_NA <- conf_matrix[2, 2, 1]
  FP_A <- conf_matrix[1, 2, 2]
  FP_NA <- conf_matrix[1, 2, 1]
  TN_A <- conf_matrix[1, 1, 2]
  TN_NA <- conf_matrix[1, 1, 1]
  FN_A <- conf_matrix[2, 1, 2]
  FN_NA <- conf_matrix[2, 1, 1]
  
  #Overall performance
  sensitivity <- round(TP_A / (TP_A + FN_A) * 100, 2)
  specificity <- round((TN_A + TN_NA/0.1) / (TN_A + FP_A + TN_NA/0.1 + FP_NA/0.1) * 100, 2)
  PPV <- round(TP_A / (TP_A + FP_A + FP_NA/0.1) * 100, 2)
  
  #Calculate performance for disaggregation given people are aggregated as Asian
  specificity_asian <- round(TN_A / (TN_A + FP_A) * 100, 2)
  PPV_asian <- round(TP_A / (TP_A + FP_A) * 100, 2)
  N <- TP_A + FN_A
  performance = c(N, sensitivity, specificity, PPV, specificity_asian, PPV_asian)
  return(performance)
}

results = rbind(results, c('Overall', perf(cohort, 'any_asian', 'surname_match')))
colnames(results) = c('Origin', 'N', 'Sensitivity', 'Specificity', 'PPV', 'Specificity_Asian', 'PPV_Asian')

#Calculate performance for highest probability ethnicity- Sensitivity, Specificity, PPV
origins = c('AF', 'SAS', 'BIA', 'CB', 'JA', 'XK', 'LA', 'MY', 'BM', 'NP', 'GCA', 'RP', 'SN', 'CE', 'TH', 'VM')

for (orig in origins) {
  df = cohort_known %>% mutate(ind = ifelse(origin==orig, 1, 0))
  name = filter(names, Highest_p==orig)
  df = mutate(df, surname_match = if_else(surname %in% name$surname, 1, 0))
  results = rbind(results, c(orig, perf(df, 'ind', 'surname_match')))
}

write.csv(results,"~/goproject/projects/panache/data/surname_validation/Results_best_ethnicity.csv")

# Calculate performance using highest ethnicity or second highest ethnicity with >20% probability
##############################

#Create empty results df
results2 = data.frame(matrix(ncol=7, nrow=0))

#Overall Asian performance
cohort = mutate(cohort, surname_match = if_else(surname %in% names$surname, 1, 0))

results2 = rbind(results2, c('Overall', perf(cohort, 'any_asian', 'surname_match')))
colnames(results2) = c('Origin', 'N', 'Sensitivity', 'Specificity', 'PPV', 'Specificity_Asian', 'PPV_Asian')

#Calculate performance for ethnicity groups
origins = c('AF', 'SAS', 'BIA', 'CB', 'JA', 'XK', 'LA', 'MY', 'BM', 'NP', 'GCA', 'RP', 'SN', 'CE', 'TH', 'VM')

for (orig in origins) {
  df = cohort_known %>% mutate(ind=ifelse(origin==orig, 1, 0))
  name = filter(names, Highest_p==orig | Second_p==orig)
  df = mutate(df, surname_match = if_else(surname %in% name$surname, 1, 0))
  results2 = rbind(results2, c(orig, perf(df, 'ind', 'surname_match')))
}

write.csv(results2, "~/goproject/projects/panache/data/surname_validation/Results_2ndbest_ethnicity.csv")

#Unique surnames/people
# cohort_known %>%
#   filter(!surname %in% names$surname & any_asian==1) %>%
#   summarise(unique_people = n_distinct(MRN),
#             unique_surnames = n_distinct(surname))

