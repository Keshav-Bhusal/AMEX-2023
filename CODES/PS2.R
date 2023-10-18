##########################################################################
#PS2
#DATE:6th Sept, 2023
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

#Loading the necessary datasets 
load("~/Library/CloudStorage/Box-Box/Project AMEX/myca_mob.RData")
myca_mob <- x
load("~/Library/CloudStorage/Box-Box/Project AMEX/myca_web.RData")
myca_web <- x
load("~/Library/CloudStorage/Box-Box/Project AMEX/raw.RData")
newraw <- x

##################################################
#Early Legal Message sent to Card Members
##################################################

#Looking at the types of communication sent to the CMs
unique(newraw$act_let_cd)

#Unique Pseudo_Keys
unique_pk <- newraw %>%
  distinct(pseudo_key) %>% 
  summarize(frequency = n())
unique_pk

#Unique Triplicates
unique_tri <- newraw %>%
  distinct(pseudo_key, case_grp_cd, case_seq_nbr) %>% 
  summarize(frequency = n())
unique_tri

# Sub-setting the CMs with early legal message 
early_legal <- newraw %>%
  filter(act_let_cd %in% c("GENLTPRL", "GENLTPON"))
early_legal

# Checking for the total number of unique CMs
#By pseudo_keys: 5,188
length(unique(early_legal$pseudo_key))   

#By triplicates: 5,262
early_legal_message <- early_legal %>%
  filter(act_let_cd %in% c("GENLTPRL", "GENLTPON")) %>%
  distinct(pseudo_key, case_grp_cd, case_seq_nbr) %>% 
  summarize(Frequency = n())
early_legal_message

# Sub-setting the unique CMs (by "pseudo_keys" and by identifier triplicates) with early legal message 
#By "pseudo_keys"
early_legal_PK <- newraw %>%
  filter(act_let_cd %in% c("GENLTPRL", "GENLTPON"))%>%
  distinct(pseudo_key)
  early_legal_PK

#By identifier triplicates
early_legal_TRI <- newraw %>%
  filter(act_let_cd %in% c("GENLTPRL", "GENLTPON"))%>%
  distinct(pseudo_key, case_grp_cd, case_seq_nbr) 
  early_legal_TRI
  
#Merging the unique early legal cases with the newraw datasets by triplicates for total early legal observations 
  result_mg_PK <- merge(newraw, early_legal_PK, by = "pseudo_key")
  total_EL <- merge(newraw, early_legal_TRI, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr"))
                            
#Cross-checking the total number of unique card members by pseudo_key: 5,188
length(unique(total_EL$pseudo_key))     

##Cross-checking for the total number of unique customers by triplicate (pseudo_key, case_grp_cd, case_seq_nbr): 5,262
early_legal_message <- total_EL %>%
  filter(act_let_cd %in% c("GENLTPRL", "GENLTPON")) %>%
  distinct(pseudo_key, case_grp_cd, case_seq_nbr) %>% 
  summarize(Frequency = n())
early_legal_message

#########################################################
#*****CMs with early legal message sent to collection***#
#########################################################

#Checking the number of unique keys in "portfo_w_lvl_cd" (What portfolio was the case in when this action took place?)
# Overall Data set: 725
unique(newraw$portfo_w_lvl_cd)   
length(unique(newraw$portfo_w_lvl_cd))

# Data set with just Early Legal Cases: 228
unique(total_EL$portfo_w_lvl_cd)   
length(unique(total_EL$portfo_w_lvl_cd))

# Sub-setting the CMs with early legal message eventually sent to collection
collection_data <- total_EL %>%
  filter(portfo_w_lvl_cd == "051")
collection_data

##Total Number of unique CMs by triplicates (pseudo_key, case_grp_cd, case_seq_nbr) sent to collections
# Overall Dataset: 4,193
collection_overall <- newraw %>%
  filter(portfo_w_lvl_cd == "051") %>%
  distinct(pseudo_key, case_grp_cd, case_seq_nbr) %>% 
  summarize(Frequency = n())
collection_overall  

# Early Legal Dataset: 4,186
el_to_collection <- total_EL %>%
  filter(portfo_w_lvl_cd == "051") %>%
  distinct(pseudo_key, case_grp_cd, case_seq_nbr) %>% 
  summarize(Frequency = n())
el_to_collection  

#########################################################################
#*****Number of Days Past Bill(DPB) the early legal message were sent***#
#########################################################################

# Create summary statistics for each data frame
summary_total_EL <- summary(total_EL$age_num_days)
summary_total_EL
summary_newraw <- summary(newraw$age_num_days)
summary_newraw

# Combined summary statistics (Overall Vs Early Legal)
combined_summary <- rbind(summary_total_EL, summary_newraw) 
colnames(combined_summary) <- c("total_EL", "newraw")
combined_summary

# Tabular Representation
tabular_DPB_NR <- total_EL %>% 
  group_by(age_num_days) %>% 
  summarize(Frequency = n()) %>% 
  ungroup() %>% 
  mutate(Total_Frequency = sum(Frequency),
         Percentage = (Frequency / Total_Frequency) * 100,
         Cumulative_Percentage = cumsum(Percentage)) %>% 
  select(age_num_days, Frequency, Percentage, Cumulative_Percentage)

tabular_DPB_NR

# Graphical Representation
# Comparative plot for OVERALL VS EARLY LEGAL 
plot(total_EL$age_num_days, type = "l", col = "red", 
     main = "Distribution of Days Past Billing (DPB): OVERALL Vs EARLY LEGAL",
     xlab = "Days Past Bill",      
     ylab = "Frequency")
lines(newraw$age_num_days, type = "l", col = "blue")
legend("topright", legend = c("Early Legal", "Overall"), col = c("red", "blue"), lty = 1)

# Histogram for Days Past Billing(DPB): OVERALL
hist(newraw$age_num_days, 
     col = "purple",             
     main = "Distribution of Days Past Billing (DPB): OVERALL", 
     xlab = "Days Past Billing(DPB)",      
     ylab = "Frequency")    

# Histogram for Days Past Billing(DPB):EARLY LEGAL
hist(total_EL$age_num_days, 
     col = "green",             
     main = "Distribution of Days Past Billing (DPB): EARLY LEGAL", 
     xlab = "Days Past Billing(DPB)",      
     ylab = "Frequency")    

#Trying with creating the bins
breaks <- c(0, 30, 50, 70, 90, 273) 
total_EL$age_bin <- cut(total_EL$age_num_days, breaks, labels = c("0-30 days", "30-50 days", "50-70 days", "70-90 days", "90+ days"))
hist(total_EL$age_num_days, 
     breaks = breaks,
     col = "purple",             
     main = "Distribution of Days Past Billing (DPB): OVERALL", 
     xlab = "Days Past Billing (DPB)",      
     ylab = "Frequency",
     xlim = c(0, 100))  

#########################################################
#*****Right and Left Censoring***#
#########################################################

str(newraw)

#Earliest possible date for the case to begin
max(total_EL$case_open_dt, na.rm = TRUE)
min(total_EL$case_open_dt, na.rm = TRUE)

max(total_EL$case_close_dt, na.rm = TRUE)
min(total_EL$case_close_dt, na.rm = TRUE)

#Check for left censoring
#Total observation counts for left censoring: 0
earliest_date_in_data <- min(total_EL$case_open_dt)
left_censored_cases <- total_EL %>%
  filter(case_open_dt < earliest_date_in_data)
left_censored_cases

#Check for right censoring
right_censored_cases <- total_EL %>%
  filter(act_type_cd == "EN" & !(pseudo_key %in% total_EL$`pseudo_key`[total_EL$act_type_cd == "RI"]))
right_censored_cases

# Total observation counts for right censoring: 1,478
# Unique triplicates counts for right censoring: 955
counts <- right_censored_cases %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr) %>% 
  summarize(unique_CMs = n())
counts


close_date <- as.Date("2022-09-26")
right_censored_cases <- total_EL %>%
  filter(act_type_cd == "EN",
         act_dt > close_date)
right_censored_cases