##########################################################################
#PS4
#DATE:26th Sept, 2023
#Author: Keshav Bhusal
#AREC 559 Advanced Applied Econometric 
#Affiliation: Department of Agriculture and Resource Economics(AREC)
##########################################################################

#Setting up the environment
library(rio)
library(ggplot2)  
library(dplyr)            
library(tidyr)  
library(readr)
library(mice) #The Multiple Imputation by Chained Equations (MICE) package, not only allows for performing imputations but includes several functions for identifying the missing data pattern(s) present in a particular dataset.
library(VIM)

# Loading the combined dataset 
early_legal <- x 

early_legal <- early_legal %>% 
  select(-case_close_dt, -act_info_cd1, -grad_date, -act_info_cd2, -act_info_cd3, -ctcpty, -ctcplace, -case_type_at_setup, -misc_amt)

#Censoring the data-sets for Early legal message sent after 29th August
early_legal <- early_legal %>% 
  filter(message_date <= "2022-08-29")

#######################################################
# Looking at the distribution of the missing value
######################################################

# Missing data patterns
missing_pattern <- md.pattern(early_legal_censored)
missing_pattern

# Number of observations per patterns for all pairs of variables
missing_obs_per_pattern <- md.pairs(early_legal_censored)
missing_obs_per_pattern

# Count missing values for each variable in your dataset
missing_count <- sapply(early_legal, function(x) sum(is.na(x)))
missing_count

# Creating the 2 distinct dataset with MISSING observation and NO MISSING Observations
missing_dataset <- early_legal_censored %>% 
  filter(is.na(age_num_days))

nonmissing_dataset <- anti_join(early_legal_censored, missing_dataset, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr"))

# Validating the presence of any missing values in the datasets
any(is.na(missing_dataset))
any(is.na(nonmissing_dataset))

# Missing Data Visualization: Margin plot of y1 and y4
marginplot(early_legal_censored[c(6, 11)], col = c("blue", "red", "orange"))

# distributions of missing variable by another specified variable
pbox(early_legal_censored, pos = 6)

######################################
#Imputation Using KNN
#####################################

check_before <- early_legal %>% 
  group_by(case_cbr_score) %>% 
  summarize(freq = n())

# Managing the outliers and NAs in CBR Score
early_legal <- early_legal %>% 
  mutate(case_cbr_score = case_when(case_cbr_score == -9999 ~ NA,
                                    case_cbr_score == -999 ~ NA,
                                    # case_cbr_score == 0 ~ NA,
                                    # case_cbr_score == 1 ~ NA,
                                    case_cbr_score == 2 ~ NA,
                                    case_cbr_score == 3 ~ NA,
                                    # case_cbr_score == 999 ~ NA,
                                    TRUE ~ case_cbr_score ))

check_after <- early_legal %>% 
  group_by(case_cbr_score) %>% 
  summarize(freq = n())

# Imputing the missing variables for variable of interest in data-sets 
early_legal_impute <- kNN(early_legal, variable = c("age_num_days", "message_DPB", "setup_cdss", "total_case_amount_due", "total_case_exposure", "total_past_due"), k = 200)

# Keeping the variables of interest 
early_legal_impute <- early_legal_impute %>% 
  select(1:33)

############################################################
#Comparing the statistics in original Vs imputed data-sets
###########################################################

  summary(early_legal$age_num_days)
  sd(early_legal$age_num_days, na.rm = T)
  
  summary(early_legal$message_DPB)
  sd(early_legal$message_DPB, na.rm = T)
  
  summary(early_legal$setup_cdss)
  sd(early_legal$setup_cdss, na.rm = T)
  
  summary(early_legal$total_case_amount_due)
  sd(early_legal$total_case_amount_due, na.rm = T)
  
  summary(early_legal$total_case_exposure)
  sd(early_legal$total_case_exposure, na.rm = T)
  
  summary(early_legal$total_past_due)
  sd(early_legal$total_past_due, na.rm = T)

# Summary statistics of variables in imputed data-sets 
  summary(early_legal_impute$age_num_days)
  sd(early_legal_impute$age_num_days, na.rm = T)
  
  summary(early_legal_impute$message_DPB)
  sd(early_legal_impute$message_DPB, na.rm = T)
  
  summary(early_legal_impute$setup_cdss)
  sd(early_legal_impute$setup_cdss, na.rm = T)
  
  summary(early_legal_impute$total_case_amount_due)
  sd(early_legal_impute$total_case_amount_due, na.rm = T)
  
  summary(early_legal_impute$total_case_exposure)
  sd(early_legal_impute$total_case_exposure, na.rm = T)
  
  summary(early_legal_impute$total_past_due)
  sd(early_legal_impute$total_past_due, na.rm = T)
  

  # Balance statistics 
  ttest1 <- t.test(early_legal$age_num_days, early_legal_impute$age_num_days)
  ttest1
  
  ttest2 <- t.test(early_legal$message_DPB, early_legal_impute$message_DPB)
  ttest2

  ttest3 <- t.test(early_legal$setup_cdss, early_legal_impute$setup_cdss)
  ttest3
  
  ttest4 <- t.test(early_legal$total_case_amount_due, early_legal_impute$total_case_amount_due)
  ttest4
  
  ttest5 <- t.test(early_legal$total_case_exposure, early_legal_impute$total_case_exposure)
  ttest5
  
  ttest6 <- t.test(early_legal$total_past_due, early_legal_impute$total_past_due)
  ttest6
  
############################################
# Calculate the standardized differences 
############################################
  
payers <- early_legal %>% 
    filter(pvcDUM == 1)
  
nonpayers <- early_legal %>% 
  filter(pvcDUM == 0)
  
mean_payers <- payers %>% 
  summarize(DPB = mean(age_num_days, na.rm = T),
            messageDPB = mean(message_DPB, na.rm = T),
            CBR = mean(case_cbr_score, na.rm = T),
            TSR = mean(tsr_max_prob, na.rm = T),
            CDSS = mean(setup_cdss, na.rm = T),
            total_case_amt_due = mean(total_case_amount_due, na.rm = T),
            total_exposure = mean(total_case_exposure, na.rm = T),
            total_past_due = mean(total_past_due, na.rm = T))

mean_nonpayers <- nonpayers %>% 
  summarize(DPB = mean(age_num_days, na.rm = T),
            messageDPB = mean(message_DPB, na.rm = T),
            CBR = mean(case_cbr_score, na.rm = T),
            TSR = mean(tsr_max_prob, na.rm = T),
            CDSS = mean(setup_cdss, na.rm = T),
            total_case_amt_due = mean(total_case_amount_due, na.rm = T),
            total_exposure = mean(total_case_exposure, na.rm = T),
            total_past_due = mean(total_past_due, na.rm = T))
  
var_payers <- payers %>% 
  summarize(DPB = var(age_num_days, na.rm = T),
            messageDPB = var(message_DPB, na.rm = T),
            CBR = var(case_cbr_score, na.rm = T),
            TSR = var(tsr_max_prob, na.rm = T),
            CDSS = var(setup_cdss, na.rm = T),
            total_case_amt_due = var(total_case_amount_due, na.rm = T),
            total_exposure = var(total_case_exposure, na.rm = T),
            total_past_due = var(total_past_due, na.rm = T))
  
var_nonpayers <- nonpayers %>% 
  summarize(DPB = var(age_num_days, na.rm = T),
            messageDPB = var(message_DPB, na.rm = T),
            CBR = var(case_cbr_score, na.rm = T),
            TSR = var(tsr_max_prob, na.rm = T),
            CDSS = var(setup_cdss, na.rm = T),
            total_case_amt_due = var(total_case_amount_due, na.rm = T),
            total_exposure = var(total_case_exposure, na.rm = T),
            total_past_due = var(total_past_due, na.rm = T))

summary_table <- bind_rows(
  mean_payers %>% mutate(Group = "Payers Mean"),
  mean_nonpayers %>% mutate(Group = "Non-Payers Mean"),
  var_payers %>% mutate(Group = "Payers Var"),
  var_nonpayers %>% mutate(Group = "Non-Payers Var")
) %>% 
  select(Group, 1:8)


print(summary_table)

d_dpb <- (summary_table[1,2] - summary_table[2,2])/sqrt(((summary_table[3,2] - summary_table[4,2])/2))
d_msgdp <- (summary_table[1,3] - summary_table[2,3])/sqrt(((summary_table[3,3] - summary_table[4,3])/2))
d_CBR <- (summary_table[1,4] - summary_table[2,4])/sqrt(((summary_table[3,4] - summary_table[4,4])/2))
d_TSR <- (summary_table[1,5] - summary_table[2,5])/sqrt(((summary_table[3,5] - summary_table[4,5])/2))
d_CDSS <- (summary_table[1,6] - summary_table[2,6])/sqrt(((summary_table[3,6] - summary_table[4,6])/2))
d_TCAD <- (summary_table[1,7] - summary_table[2,7])/sqrt(((summary_table[3,7] - summary_table[4,7])/2))
d_TE <- (summary_table[1,8] - summary_table[2,8])/sqrt(((summary_table[3,8] - summary_table[4,8])/2))
d_TPD <- (summary_table[1,9] - summary_table[2,9])/sqrt(((summary_table[3,9] - summary_table[4,9])/2))


ttest1 <- t.test(payers$total_past_due, nonpayers$total_past_due)
ttest1

