##########################################################################
#PS4
#DATE:12 Oct, 2023
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

#Load the necessary datasets and creating the combined dataset 
early_legal <- combined_all50

case_open <- x

early_legal <- early_legal %>% 
  mutate(date_diff = case_close_dt - case_open_dt)

#early_legal_unique_censored <- early_legal_unique_censored %>% 
#  select(pseudo_key, case_grp_cd, case_seq_nbr, cm_type)


# Normalized Counts  
early_legal <- early_legal %>%   
  mutate(norm_MLC = as.numeric(mobile_login_count)/as.numeric(date_diff), 
         norm_WLC = as.numeric(web_login_count)/as.numeric(date_diff),
         norm_BLC_mobile = as.numeric(both_mob_login_count)/as.numeric(date_diff),
         norm_BLC_web = as.numeric(both_web_login_count)/as.numeric(date_diff),
         norm_BLC = as.numeric(both_login_count)/as.numeric(date_diff))
  
#  early_legal_diff1 <- early_legal_diff %>% 
#    left_join(early_legal_unique_censored, early_legal_diff, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr"))

#Descriptive Statistics 

discriptives <- early_legal %>% 
  group_by(both, both_mob_login_period) %>% 
  summarize(freq = n()) %>% 
  ungroup() %>% 
  mutate(total_freq = sum(freq),
         percentage = freq/total_freq * 100)


summary(early_legal_diff$mobile_login_count, na.rm = T)
summary(early_legal_diff$norm_MLC, na.rm = T)

summary(early_legal_diff$web_login_count, na.rm = T)
summary(early_legal_diff$norm_WLC, na.rm = T)

summary(early_legal_diff$both_mob_login_count, na.rm = T)
summary(early_legal_diff$norm_BLC_mobile, na.rm = T)

summary(early_legal_diff$both_web_login_count, na.rm = T)
summary(early_legal_diff$norm_BLC_web, na.rm = T)

discriptives <- early_legal %>% 
  group_by(pvcDUM, both_web_login_period) %>% 
  summarize(freq = n()) %>% 
  ungroup() %>% 
  mutate(total_freq = sum(freq),
         percentage = freq/total_freq * 100)
discriptives

summary_table <- early_legal_diff1 %>%
  group_by(cm_type) %>%
  summarize(mean_login_count = mean(norm_MLC))

summary_table

summary_table1 <- early_legal_diff %>%
  group_by(both_web_login_period) %>%
  summarize(mean_login_count = mean(norm_BLC_web))

summary_table1


summary_table(early_legal_diff1$cm_type)


