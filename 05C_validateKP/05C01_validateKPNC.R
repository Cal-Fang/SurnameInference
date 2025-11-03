##############################
# KPNC Surname List Validation
# Author: Rishi Parikh
##############################

#Libraries
library(dplyr)
library(haven)
library(stringr)
library(tidyr)
library(ggplot2)

#Datasets
##############################

#Code highest probability ethnicity and second highest (with >20% probability)
namelist = read.csv("~/probList.csv")

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
cohort = read_sas("~/surname_cohort_final.sas7bdat")

cohort = mutate(cohort,
                      age_c = factor(case_when(
                        age < 40 ~ 1,
                        age >= 40 & age < 60 ~ 2,
                        age >=60 ~ 3
                      ),levels = c(1,2,3), labels = c("<40","40-59","60+")))


#Re-run with sex filters for stratified performance estimates
#cohort = filter(cohort, SEX_ADMIN == 'M')
#cohort = filter(cohort, age_c == "60+")


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

write.csv(results,"~/Results_best_ethnicity.csv")

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

write.csv(results2, "~/Results_2ndbest_ethnicity.csv")

#Unique surnames/people
# cohort_known %>%
#   filter(!surname %in% names$surname & any_asian==1) %>%
#   summarise(unique_people = n_distinct(MRN),
#             unique_surnames = n_distinct(surname))


#Additional Validation - 10/27/2025
######################################################

# Analysis 1- plot probability density by ethnicity
cohort = read_sas("~/surname_cohort_final.sas7bdat")
cohort_known = filter(cohort, !origin %in% c('MA','UA','NA'))

df = cohort_known %>% left_join(names,by="surname")

origins = c('AF', 'SAS', 'BIA', 'CB', 'JA', 'XK', 'LA', 'MY', 'BM', 'NP', 'GCA', 'RP', 'SN', 'CE', 'TH', 'VM')
labels = c("Afghanistan", 
           "Bangladesh, India, Pakistan,\nSikkim, British India",
           "Indonesia",
           "Cambodia",
           "Japan,\nSouthern Ryukyu Islands",
           "Korea, Democratic People’s\nRepublic of Korea,\nRepublic of Korea",
           "Laos",
           "Malaysia",
           "Myanmar",
           "Nepal",
           "People's Republic of China,\nHong Kong, Macau (Macao),\nTaiwan",
           "Philippines",
           "Singapore",
           "Sri Lanka",
           "Thailand",
           "Vietnam, Democratic Republic\nof Vietnam, Republic\nof Vietnam")

df_long <- df %>%
  pivot_longer(cols = all_of(origins), 
               names_to = "ancestry",
               values_to = "value") %>%
  filter(ancestry == origin) %>%
  mutate(origin = factor(origin, levels = origins, labels = labels))

# Density plot faceted by the individual's origin
ggplot(df_long, aes(x = value)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  facet_wrap(~ origin, scales = "free_y", ncol = 4, nrow = 4) +
  labs(y="Density", x="Probability") + 
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=expansion(mult=c(0,0.1)))+
  theme_bw()

ggsave("~/density_plot.png",height=8,width=10,units="in",dpi=600)


#Calibration table

x = as.data.frame(prop.table(table(df$origin))) %>%
  mutate(Var1 = factor(Var1, levels=origins, labels=labels))

df_long <- df %>%
  pivot_longer(cols = all_of(origins), 
               names_to = "ancestry",
               values_to = "value") %>%
  mutate(origin = factor(origin, levels = origins, labels = labels))

y = df_long %>% 
  group_by(ancestry) %>%
  summarize(mean = mean(value,na.rm=T)) %>%
  mutate(Var1 = factor(ancestry,levels=origins,labels=labels))

table = left_join(x,y,by="Var1")
colnames(table) = c("Ethnicity","KP_freq","origin","ProbList_Freq")

write.csv(table, "~/Overall_calibration.csv")

