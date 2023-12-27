##########################################################################
#PS3
#DATE:10th Sept, 2023
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

#Load the necessary datasets and creating the combined dataset 
load("~/Library/CloudStorage/Box-Box/Project AMEX/myca_mob.RData")
myca_mob <- x
load("~/Library/CloudStorage/Box-Box/Project AMEX/myca_web.RData")
myca_web <- x


early_legal_all <- x


####################################################
#Early Legal:
#Consumer Vs Small Business
####################################################

#a. Unique Card members: 5,188 / Unique Cases (triplicates): 5,262
early_legal <- early_legal_all %>%
  filter(act_let_cd %in% c("GENLTPRL", "GENLTPON")) %>%
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T)
early_legal

## Creating the filter for the card member types (CUST Only, COMP Only and Both)
early_legal_all <- x %>%
  mutate(cm_type = case_when(
    case_grp_cd == "CUST" & !(pseudo_key %in% early_legal_all$pseudo_key[early_legal_all$case_grp_cd == "COMP"]) ~ "cust_only",
    case_grp_cd == "COMP" & !(pseudo_key %in% early_legal_all$pseudo_key[early_legal_all$case_grp_cd == "CUST"]) ~ "comp_only",
    case_grp_cd == "COMP" & (pseudo_key %in% early_legal_all$pseudo_key[early_legal_all$case_grp_cd == "CUST"]) ~ "both",
    TRUE ~ "both"
 #TRUE ~ NA_character_   
  ))
early_legal_all

#Proportion of types of Card Members
prop <- early_legal_all %>% 
  group_by(cm_type) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr) %>% 
  summarize(freq = n()) %>% 
  ungroup() %>% 
  mutate(Total_Frequency = sum(freq),
         Percentage = (freq / Total_Frequency) * 100,
         Cumulative_Percentage = cumsum(Percentage)) 
prop

# Combining the Datasets 
# total_cases <- merge(newraw, early_legal, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr"))

####################################################
#Early Legal:
#Non Payers: SENT TO COLLECTION
####################################################

# Sub-setting the CMs with early legal message eventually sent to collection
collection_data <- early_legal %>%
  filter(portfo_w_lvl_cd == "051") %>% 
  distinct(pseudo_key, .keep_all = T)
collection_data

##Total Number of unique CMs by triplicates (pseudo_key, case_grp_cd, case_seq_nbr) sent to collections
# Overall Dataset: 4,193
collection_overall <- newraw %>%
  filter(portfo_w_lvl_cd == "051") %>%
  distinct(pseudo_key, case_grp_cd, case_seq_nbr) %>% 
  summarize(Frequency = n())
collection_overall  

# Classification of the unique card members (by triplicates) sent to collection by type of card member (Customer, Small business or Both) 
  prob2 <- collection_data %>% 
    group_by(cm_type) %>% 
    summarize(freq = n()) %>% 
    ungroup() %>% 
    mutate(Total_Frequency = sum(freq),
           Percentage = (freq / Total_Frequency) * 100,
           Cumulative_Percentage = cumsum(Percentage)) %>% 
  select(cm_type, freq, Percentage, Cumulative_Percentage)
  prob2

  ####################################################
  #Early Legal:
  #Non Payers: PAYMENT MADE AND DAYS PAST BILL
  ####################################################  
  
#Creating the dataset for unique cardmembers making payment
  payment_data <- early_legal %>% 
    filter(act_type_cd == "PY") %>% 
    distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T)  
    #select(pseudo_key, case_grp_cd, case_seq_nbr, act_type_cd)
  payment_data
  
  notice_data <- early_legal %>% 
    filter(act_type_cd == "LS") %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T)  
  notice_data 
  
# Calculating the number and proportions of the actual payment made   
  payment <- payment_data %>% 
    filter(act_type_cd == "PY") %>% 
    distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T) %>% 
  summarize(freq = n()) %>% 
    ungroup %>% 
    summarize(total_unique_count = freq,
              proportion = freq/nrow(early_legal)*100)
  payment
  
# Creating the dataset for payment cases 
  payment_cases <- merge(total_cases, payment_data, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr"))
  
#Cross-Checking for the unique CMs in payment_cases datasets. 
#Looks good !! We have same no. unique triplicates: 932
  cases <- payment_cases %>% 
    distinct(pseudo_key, case_grp_cd, case_seq_nbr)  %>% 
    summarize(frequency = n())
  cases
  
# Number of DPB when message was sent
  days_count_msgSENT <- early_legal %>% 
    filter(act_let_cd %in% c("GENLTPRL", "GENLTPON")) %>%
    distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T)
    days_count_msgSENT
    
  notice_sent <- days_count_msgSENT %>% 
    group_by(messasge_DPB) %>% 
    summarize(Frequency = n()) %>% 
    ungroup() %>% 
    mutate(Total_Frequency = sum(Frequency),
           Percentage = (Frequency / Total_Frequency) * 100,
           Cumulative_Percentage = cumsum(Percentage)) %>% 
    select(age_num_days, Frequency, Percentage, Cumulative_Percentage)
  notice_sent

# Create a histogram
  custom_breaks <- c(0, 60, 70, 75, 80, 125)
  
p <- ggplot(days_count_msgSENT, aes(x = age_num_days)) +
    geom_histogram(breaks = custom_breaks, fill = "blue", color = "black", na.rm = T) +
    labs(x = "Days Past Billing (DPB)", y = "Frequency", title = "Days Past Billing (DPB) When Early Legal Message was Sent") +
    scale_x_continuous(breaks = custom_breaks, labels = custom_breaks)+
    scale_y_continuous(breaks = seq(0, 4000, by = 500),labels = scales::number_format(scale = 1))
p

# Number of days after receiving the early legal message notice the payment was made

payment_done <- early_legal %>%
  filter(act_type_cd == "PY") %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T) 
payment_done

summary_dpb_payment <- payment_done %>% 
  group_by(age_num_days) %>% 
  summarize(Frequency = n()) %>% 
  ungroup() %>% 
  mutate(Total_Frequency = sum(Frequency),
         Percentage = (Frequency / Total_Frequency) * 100,
         Cumulative_Percentage = cumsum(Percentage)) %>% 
  select(age_num_days, Frequency, Percentage, Cumulative_Percentage)
summary_dpb_payment

custom_break1 <- c(seq(0,91, by= 7), 184)
p <- ggplot(payment_done, aes(x = age_num_days)) +
  geom_histogram(breaks = custom_break1,fill = "purple", color = "black", na.rm = T) +
  labs(x = "Days Past Billing (DPB)", y = "Frequency", title = "Days Past Billing (DPB) When Payment Was Made") +
  scale_x_continuous(breaks = custom_break1, labels = custom_break1)+
  scale_y_continuous(breaks = seq(0, 300, by= 20), labels = scales::number_format(scale = 1))
p

#d) Right and Left Censoring
  
censoring <- early_legal_all %>% 
  filter(message_date > "08/26/2022") %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T) 
censoring

####################################################
#2. Differences in Observable's
####################################################


paid <- early_legal_all %>% 
  filter(act_type_cd == "PY") %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T)
paid

p_cat <- paid %>% 
  group_by(cm_type) %>% 
  summarize(Frequency = n ()) %>% 
  ungroup() %>% 
  mutate(Total_Frequency = sum(Frequency),
         Percentage = (Frequency / Total_Frequency) * 100) %>% 
  select(cm_type, Frequency, Percentage)
p_cat


summary(paid$age_num_days, na.rm = T)
summary(collection$age_num_days, na.rm = T)

summary(paid$total_case_exposure, na.rm = T)
summary(collection$total_case_exposure, na.rm = T)

summary(paid$total_case_amount_due, na.rm = T)
summary(collection$total_case_amount_due, na.rm = T)

# Check for normality- Days past Bill
par(mfrow = c(2, 2))
hist(paid$age_num_days, main = "Payer- Days Past Bill", xlab = "Days Past Bill")
hist(collection$age_num_days, main = "Non-Payer-Days Past Bill", xlab = "Days Past Bill")
qqnorm(paid$age_num_days, main = "Payer - Q-Q Plot")
qqline(paid$age_num_days)
qqnorm(collection$age_num_days, main = "Non-Payer-Q-Q Plot")
qqline(collection$age_num_days)

# Check for normality- Total Exposure Amount
par(mfrow = c(2, 2))
hist(paid$total_case_exposure, main = "Payer- Total Case Exposure", xlab = "Total Case Exposure")
hist(collection$total_case_exposure, main = "Non-Total Case Exposure", xlab = "Total Case Exposure")
qqnorm(paid$total_case_exposure, main = "Payer - Q-Q Plot")
qqline(paid$total_case_exposure)
qqnorm(collection$total_case_exposure, main = "Non-Payer-Q-Q Plot")
qqline(collection$total_case_exposure)


qqnorm(collection$age_num_days)
qqline(collection$age_num_days, col = 2)
title("Q-Q Plot for Paid Dataset - age_num_days")

ttest <- t.test(paid$total_case_amount_due, collection$total_case_amount_due)
ttest

collection <- early_legal_all %>% 
  filter(portfo_w_lvl_cd == "051") %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T)
collection

c_cat <- collection %>% 
  group_by(cm_type) %>% 
  summarize(Frequency = n ()) %>% 
  ungroup() %>% 
  mutate(Total_Frequency = sum(Frequency),
         Percentage = (Frequency / Total_Frequency) * 100) %>% 
  select(cm_type, Frequency, Percentage)
c_cat




