##########################################################################
#PS4
#DATE:28th Sept, 2023
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
library(VIM)
library(zoo)

#Loading the data set (Load the "early_legal_all" data set by Dr. Thompson)
check_dataset <- x

# Sub-setting just the 2 cases ("pseudo_keys") for checking the imputation
particular <- check_dataset 
#  filter(pseudo_key == 5185 | pseudo_key == 2088 | pseudo_key ==178979)  #2088 #178979

# Looking for the unique observations in variables
unique(particular$case_cbr_score)

# Managing the outliers in the variable (Converting -9999 and 0 to NAs in this case)
particular <- particular %>% 
  mutate(case_cbr_score = case_when(case_cbr_score == -9999 ~ NA,
                                  case_cbr_score == -999 ~ NA,
                                  case_cbr_score == 0 ~ NA,
                                  case_cbr_score == 1 ~ NA,
                                  case_cbr_score == 2 ~ NA,
                                  case_cbr_score == 3 ~ NA,
                                  case_cbr_score == 999 ~ NA,
                                  TRUE ~ case_cbr_score ))

# Verifying the change in outliers in the variable (No other outliers)
unique(particular$case_cbr_score)

# Creating the uniform date format across the dataset (Need to arrange the data chronologically)
particular <- particular %>% 
  mutate (act_dt = as.Date(act_dt, format = "%d%b%Y"),
          case_open_dt = as.Date(case_open_dt, format = "%d%b%Y"),
          message_date = as.Date(message_date, format = "%m/%d/%Y"),
          case_close_dt = as.Date(case_close_dt, format = "%d%b%Y"))

# Sort the data-set by pseudo_key and date (or any other appropriate ordering variable)
particular <- particular %>%
  arrange(pseudo_key, act_dt)

# Creating the duplicate variables of interest to compare original vs imputed variable
particular <- particular %>% 
  mutate(case_close_dt1 = case_close_dt) 
#    setup_cdss1 = setup_cdss,
#         total_case_amount_due1 = total_case_amount_due,
#         total_past_due1 = total_past_due) %>% 
#  select(pseudo_key, case_grp_cd, case_seq_nbr, act_dt, message_DPB, act_type_cd, setup_cdss1, setup_cdss, total_case_amount_due1, total_case_amount_due, 
#         total_case_exposure1, total_case_exposure, total_past_due1, total_past_due)

# # Assuming your dataset is named "particular"
# particular_zoo <- zoo::zoo(particular$total_case_exposure)
# 
# # Impute missing values using Last Observation Carried Forward "na.locf "
# particular_zoo <- na.locf(particular_zoo)
# 
# # Replace the original column with the imputed values
# particular$total_case_exposure <- particular_zoo

vars_to_impute <- c(
  "case_close_dt",
  "total_case_exposure",
  "setup_cdss",
  "total_case_amount_due",
  "total_case_exposure",
  "total_past_due",
  "case_cbr_score"
)


##############################
# FINAL SHOT: Modified Version
##############################

particular1 <- particular %>%
  group_by(pseudo_key, case_grp_cd, case_seq_nbr) %>%
  arrange(pseudo_key, act_dt) %>%
  mutate(across(vars_to_impute, ~ {
    if (all(is.na(.))) {
      NA
    } else {
      # Find the index of the first non-missing value within each group
      first_non_missing_idx <- which(!is.na(.))[1]
      
      # Initialize a flag to track backward imputation
      backward_imputed <- FALSE
      
      # Check if initial observation is missing
      if (is.na(.[1])) {
        # Perform backward imputation only once if not already performed
        if (!backward_imputed) {
          .[1:(first_non_missing_idx - 1)] <- na.locf(., fromLast = TRUE, na.rm = FALSE)[1:(first_non_missing_idx - 1)]
          backward_imputed <- TRUE
        }
      }
      
      # Perform forward imputation
      na.locf(., na.rm = FALSE)
    }
  })) %>%
  ungroup()

# Checking the imputation 
  any(is.na(particular1$case_close_dt))
  any(is.na(particular1$case_cbr_score))
  any(is.na(particular1$total_case_exposure))
  any(is.na(particular1$total_case_exposure))
  any(is.na(particular1$total_past_due))
  
# Looking at some rotten tomatoes in case_cbr_score: 14 unique rotten tomatoes
    ffff <- particular1 %>%
    filter(is.na(case_cbr_score)) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = TRUE)
    
# Imputing the Date    
    particular1 <- particular1 %>% 
      group_by(pseudo_key) %>%
      mutate(case_close_dt = case_when(is.na(case_close_dt) ~ max(act_dt),
                                  !is.na(case_close_dt) ~ case_close_dt,
                                  T ~ case_close_dt)) %>% 
      ungroup() 
    
    
# Exporting Dataset    
  export(particular1, "Imputed_Final.Rdata")  
  
  
  
  
  
  
  
  
  
  
  
  
  
#GARAGE GARAGE GARAGE GARAGE
    
##############################
# Final Shot needs modfn.  :) 
##############################
    
    particular1 <- particular %>%
      group_by(pseudo_key) %>%
      arrange(pseudo_key, act_dt) %>%
      mutate_at(vars_to_impute, ~ {
        # Find the index of the first non-missing value within each group
        first_non_missing_idx <- which(!is.na(.))[1]
        
        # Initialize a flag to track backward imputation
        backward_imputed <- FALSE
        
        # Check if initial observation is missing
        if (is.na(.[1])) {
          # Perform backward imputation only once if not already performed
          if (!backward_imputed) {
            .[1:(first_non_missing_idx - 1)] <- na.locf(., fromLast = TRUE, na.rm = FALSE)[1:(first_non_missing_idx - 1)]
            backward_imputed <- TRUE
          }
        }
        
        # Perform forward imputation
        na.locf(., na.rm = FALSE)
      }) %>%
      ungroup()
    
    
    export (particular1, "Imputed_EarlyLegal_Dataset.RData")
    
    ##############################
    # Imperfect
    ##############################
    
    particular1 <- particular %>%
      group_by(pseudo_key) %>%
      arrange(pseudo_key, act_dt) %>%
      mutate_at(vars_to_impute, ~ {
        # Find the index of the first non-missing value within each group
        first_non_missing_idx <- which(!is.na(.))[1]
        
        if (is.na(.[1])) {
          # Initial observation is missing, perform backward imputation
          na.locf(., fromLast = TRUE, na.rm = FALSE)
        } else {
          # Initial observation is available, perform forward imputation
          na.locf(., na.rm = FALSE)
        }
      }) %>%
      
      
      ungroup()
