##########################################################################
#PS4
#DATE:5 Nov, 2023
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
library(gtsummary)

#Load the necessary datasets and creating the combined dataset 
load("~/Library/CloudStorage/Box-Box/Project AMEX/working_data/myca_mob.RData")
myca_mob <- x
load("~/Library/CloudStorage/Box-Box/Project AMEX/working_data/myca_web.RData")
myca_web <- x
load("~/Library/CloudStorage/Box-Box/Project AMEX/working_data/Imputed_Final.RData")
early_legal <- x

## Creating the filter for the card member types (CUST Only, COMP Only and Both)
early_legal <- early_legal %>%
  mutate(cm_type = case_when(
    case_grp_cd == "CUST" & !(pseudo_key %in% early_legal$pseudo_key[early_legal$case_grp_cd == "COMP"]) ~ "Customer_Only",
    case_grp_cd == "COMP" & !(pseudo_key %in% early_legal$pseudo_key[early_legal$case_grp_cd == "CUST"]) ~ "Business_Only",
    case_grp_cd == "COMP" & (pseudo_key %in% early_legal$pseudo_key[early_legal$case_grp_cd == "CUST"]) ~ "Both",
    TRUE ~ "Both"
  ))


# Looking the summary of types of customers (Costumer, Business and Both)
cardmember_types <- early_legal %>% 
  group_by(cm_type) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  mutate(total = sum(freq),
         percentage = freq/total *100)

# Creating the uniform date format across the dataset (Similar to the MYCA Dataset)
early_legal <- early_legal %>% 
  mutate (act_dt = as.Date(act_dt, format = "%d%b%Y"),
          case_open_dt = as.Date(case_open_dt, format = "%d%b%Y"),
          message_date = as.Date(message_date, format = "%m/%d/%Y"),
          case_close_dt = as.Date(case_close_dt, format = "%d%b%Y"))


# Creating the period of the activity (Before or After Early Legal Notice)
 early_legal <- early_legal %>% 
   mutate(period = if_else(act_dt <= message_date, "before EL", "after EL"))

# Looking for Right Party Contact (Phone, Inbound Phone, Outbound Phone, Email, Chat). Will normalize the counts in the final dataset
early_legal <- early_legal %>%
  mutate(kept_promise = if_else(act_type_cd %in% c("KP"), 1, 0),
         kept_promise_before = if_else(period =="before EL" & kept_promise == 1, 1, 0),
         kept_promise_after = if_else(period =="after EL" & kept_promise == 1, 1, 0),
         
         brok_promise = if_else(act_type_cd %in% c("BP"), 1, 0),
         brok_promise_before = if_else(period =="before EL" & brok_promise == 1, 1, 0),
         brok_promise_after = if_else(period =="after EL" & brok_promise == 1, 1, 0),
         
         phone_rpc = if_else(act_type_cd %in% c("OC", "IC") & ctcpty %in% c('B', 'C', 'D', 'K', 'P', 'S', 'T', 'Z'), 1, 0),
         phone_rpc_before = if_else(period =="before EL" & phone_rpc == 1, 1, 0),
         phone_rpc_after = if_else(period =="after EL" & phone_rpc == 1, 1, 0),
         
         phone_inbound = if_else(act_type_cd %in% c("IC") & ctcpty %in% c('B','C','D','K','P','S','T','Z'), 1, 0),
         phone_inbound_before = if_else(period =="before EL" & phone_inbound == 1, 1, 0),
         phone_inbound_after = if_else(period =="after EL" & phone_inbound == 1, 1, 0),
         
         phone_outbound = if_else(act_type_cd %in% c("OC") & ctcpty %in% c('B','C','D','K','P','S','T','Z'), 1, 0),
         phone_outbound_before = if_else(period =="before EL" & phone_outbound == 1, 1, 0),
         phone_outbound_after = if_else(period =="after EL" & phone_outbound == 1, 1, 0),
         
         phone_outbound_attempts = if_else(act_type_cd %in% c("OC") & ctcpty %in% c('A', 'E', 'F', 'G', 'H', 'I', 'J', 'L', 'M', 'N', 'O', 'Q','R', 'U', 'V', 'W', 'Y'), 1, 0),
         phone_outbound_attempt_before = if_else(period =="before EL" & phone_outbound_attempts == 1, 1, 0),
         phone_outbound_attempt_after = if_else(period =="after EL" & phone_outbound_attempts == 1, 1, 0),
         
         chat = if_else(act_type_cd %in% c("CH") & ctcpty %in% c('B'), 1, 0),
         chat_before = if_else(period =="before EL" & chat == 1, 1, 0),
         chat_after = if_else(period =="after EL" & chat == 1, 1, 0),
         
         sms = if_else(act_type_cd %in% c("MR"), 1, 0),
         sms_before = if_else(period =="before EL" & sms == 1, 1, 0),
         sms_after = if_else(period =="after EL" & sms == 1, 1, 0),
         
         email_sent = if_else(act_type_cd %in% c("ES"), 1, 0),
         email_before = if_else(period =="before EL" & email_sent == 1, 1, 0),
         email_after = if_else(period =="after EL" & email_sent == 1, 1, 0),
         
         email_received = if_else(act_type_cd %in% c("ER"), 1, 0),
         email_received_before = if_else(period =="before EL" & email_received == 1, 1, 0),
         email_received_after = if_else(period =="after EL" & email_received == 1, 1, 0),
         
         letter_sent = if_else(act_type_cd %in% c("LS"), 1, 0),
         letter_sent_before = if_else(period =="before EL" & letter_sent == 1, 1, 0),
         letter_sent_after = if_else(period =="after EL" & letter_sent == 1, 1, 0),
         
         letter_received = if_else(act_type_cd %in% c("LR"), 1, 0),
         letter_received_before = if_else(period =="before EL" & letter_received == 1, 1, 0),
         letter_received_after = if_else(period =="after EL" & letter_received == 1, 1, 0),
         
         email_letter_sent = if_else(act_type_cd %in% c("MS"), 1, 0),
         email_letter_sent_before = if_else(period =="before EL" & email_letter_sent == 1, 1, 0),
         email_letter_sent_after = if_else(period =="after EL" & email_letter_sent == 1, 1, 0)) %>%
  
  group_by(pseudo_key, case_grp_cd, case_seq_nbr) %>%
  mutate(count_kept_promise = sum(kept_promise),
         count_brok_promise = sum(brok_promise),
         count_phone_rpc = sum(phone_rpc),
         count_phone_inbound = sum(phone_inbound),
         count_phone_outbound = sum(phone_outbound),
         count_phone_outbound_attempts = sum(phone_outbound_attempts),
         count_chat = sum(chat),
         count_sms = sum(sms),
         count_email = sum(email_sent),
         count_email_received = sum(email_received),
         count_letter_sent = sum(letter_sent),
         count_letter_received = sum(letter_received),
         count_email_letter_sent = sum(email_letter_sent)) %>%
  ungroup() 

# Counting the number of engagement before and after the early legal notice
early_legal <- early_legal %>%
  group_by(pseudo_key, case_grp_cd, case_seq_nbr) %>%
  mutate (
    count_kept_promise_before = sum(case_when(period == "before EL" ~ kept_promise, TRUE ~ 0)),
    count_kept_promise_after = sum(case_when(period == "after EL" ~ kept_promise, TRUE ~ 0)),
    count_brok_promise_before = sum(case_when(period == "before EL" ~ brok_promise, TRUE ~ 0)),
    count_brok_promise_after = sum(case_when(period == "after EL" ~ brok_promise, TRUE ~ 0)),
    count_phone_rpc_before = sum(case_when(period == "before EL" ~ phone_rpc, TRUE ~ 0)),
    count_phone_rpc_after = sum(case_when(period == "after EL" ~ phone_rpc, TRUE ~ 0)),
    count_phone_inbound_before = sum(case_when(period == "before EL" ~ phone_inbound, TRUE ~ 0)),
    count_phone_inbound_after = sum(case_when(period == "after EL" ~ phone_inbound, TRUE ~ 0)),
    count_phone_outbound_before = sum(case_when(period == "before EL" ~ phone_outbound, TRUE ~ 0)),
    count_phone_outbound_after = sum(case_when(period == "after EL" ~ phone_outbound, TRUE ~ 0)),
    count_phone_outbound_attempt_before = sum(case_when(period == "before EL" ~ phone_outbound_attempts, TRUE ~ 0)),
    count_phone_outbound_attempt_after = sum(case_when(period == "after EL" ~ phone_outbound_attempts, TRUE ~ 0)),
    count_chat_before = sum(case_when(period == "before EL" ~ chat, TRUE ~ 0)),
    count_chat_after = sum(case_when(period == "after EL"~ chat, TRUE ~ 0)),
    count_sms_before = sum(case_when(period == "before EL" ~ sms, TRUE ~ 0)),
    count_sms_after = sum(case_when(period == "after EL"~ sms, TRUE ~ 0)),
    count_email_before = sum(case_when(period == "before EL"~ email_sent, TRUE ~ 0)),
    count_email_after = sum(case_when(period == "after EL"~ email_sent, TRUE ~ 0)),
    count_email_received_before = sum(case_when(period == "before EL"~ email_received, TRUE ~ 0)),
    count_email_received_after = sum(case_when(period == "after EL"~ email_received, TRUE ~ 0)),
    count_letter_sent_before = sum(case_when(period == "before EL"~ letter_sent, TRUE ~ 0)),
    count_letter_sent_after = sum(case_when(period == "after EL"~ letter_sent, TRUE ~ 0)),
    count_letter_received_before = sum(case_when(period == "before EL"~ letter_received, TRUE ~ 0)),
    count_letter_received_after = sum(case_when(period == "after EL"~ letter_received, TRUE ~ 0)),
    count_email_letter_sent_before = sum(case_when(period == "before EL"~ email_letter_sent, TRUE ~ 0)),
    count_email_letter_sent_after = sum(case_when(period == "after EL"~ email_letter_sent, TRUE ~ 0))) %>% 
ungroup()

# Distributing the engagement status for overall case, before EL and after EL
early_legal <- early_legal %>%
  group_by(pseudo_key, case_grp_cd, case_seq_nbr) %>%
  mutate(kept_promise = if_else(any(kept_promise == 1), 1, 0),
         kept_promise_before = if_else(any(kept_promise_before == 1), 1, 0),
         kept_promise_after = if_else(any(kept_promise_after == 1), 1, 0),
         
         brok_promise = if_else(any(brok_promise == 1), 1, 0),
         brok_promise_before = if_else(any(brok_promise_before == 1), 1, 0),
         brok_promise_after = if_else(any(brok_promise_after == 1), 1, 0),
         
         phone_rpc = if_else(any(phone_rpc == 1), 1, 0),
         phone_rpc_before = if_else(any(phone_rpc_before == 1), 1, 0),
         phone_rpc_after = if_else(any(phone_rpc_after == 1), 1, 0),
         
         phone_inbound = if_else(any(phone_inbound == 1), 1, 0),
         phone_inbound_before = if_else(any(phone_inbound_before == 1), 1, 0),
         phone_inbound_after = if_else(any(phone_inbound_after == 1), 1, 0),
         
         phone_outbound = if_else(any(phone_outbound == 1), 1, 0),
         phone_outbound_before = if_else(any(phone_outbound_before == 1), 1, 0),
         phone_outbound_after = if_else(any(phone_outbound_after == 1), 1, 0),
         
         phone_outbound_attempt = if_else(any(phone_outbound_attempts == 1), 1, 0),
         phone_outbound_attempt_before = if_else(any(phone_outbound_attempt_before == 1), 1, 0),
         phone_outbound_attempt_after = if_else(any(phone_outbound_attempt_after == 1), 1, 0),
         
         chat = if_else(any(chat == 1), 1, 0),
         chat_before = if_else(any(chat_before == 1), 1, 0),
         chat_after = if_else(any(chat_after == 1), 1, 0),
         
         sms = if_else(any(sms == 1), 1, 0),
         sms_before = if_else(any(sms_before == 1), 1, 0),
         sms_after = if_else(any(sms_after == 1), 1, 0),
         
         email_sent = if_else(any(email_sent == 1), 1, 0),
         email_sent_before = if_else(any(email_before == 1), 1, 0),
         email_sent_after = if_else(any(email_after == 1), 1, 0),
         
         email_received =  if_else(any(email_received == 1), 1, 0),
         email_received_before =  if_else(any(email_received_before == 1), 1, 0),
         email_received_after =  if_else(any(email_received_after == 1), 1, 0),
         
         letter_sent = if_else(any(letter_sent == 1), 1, 0),
         letter_sent_before = if_else(any(letter_sent_before == 1), 1, 0),
         letter_sent_after = if_else(any(letter_sent_after == 1), 1, 0),
         
         letter_received = if_else(any(letter_received == 1), 1, 0),
         letter_received_before = if_else(any(letter_received_before == 1), 1, 0),
         letter_received_after = if_else(any(letter_received_after == 1), 1, 0),
         
         email_letter_sent = if_else(any(email_letter_sent == 1), 1, 0),
         email_letter_sent_before = if_else(any(email_letter_sent_before == 1), 1, 0),
         email_letter_sent_after = if_else(any(email_letter_sent_after == 1), 1, 0)) %>% 
  
ungroup()


# Early Legal: Unique Card members: 5,262
early_legal_unique <- early_legal %>%
  filter(act_let_cd %in% c("GENLTPRL", "GENLTPON")) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T) 

# Case Open: Unique Card members: 5,283
case_open_unique <- early_legal %>%
  filter(act_type_cd == "EN")%>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T) 

# Selecting the desired variables from case open dataset to integrate in the final dataset (case open and early legal)
case_open_select <- case_open_unique %>%
  select(pseudo_key, case_grp_cd, case_seq_nbr, case_cbr_score, tsr_max_prob, setup_cdss, total_case_amount_due, total_case_exposure, total_past_due) %>% 
  rename(co_case_cbr_score = case_cbr_score,
         co_tsr_max_prob = tsr_max_prob,
         co_setup_cdss = setup_cdss,
         co_total_case_amount_due = total_case_amount_due,
         co_total_case_exposure = total_case_exposure,
         co_total_past_due = total_past_due)

# Creating the desired dataset with variable of interest from case open 
early_legal_unique <-  early_legal_unique %>% 
  left_join(case_open_select, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr"))

#Censoring the data-sets for Early legal message sent after 29th August: 4,784
early_legal_unique_censored <- early_legal_unique %>% 
  filter(message_date <= "2022-08-29")

export(early_legal_unique_censored, "intermediate_done.Rdata")

######################################################################
#Creating the variables of interest in the MYCA MOBILE DATASET
######################################################################
early_legal123 <- early_legal_unique_censored %>% 
  select(pseudo_key, case_grp_cd, case_seq_nbr, case_open_dt, message_date, case_close_dt) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T)

myca_mob1 <- myca_mob %>% 
  mutate(mobile = 1) 

result_msgSET <- myca_mob1 %>%
  left_join(early_legal123, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) 

#Looking at the unique triplicates (Not required to Run)
result_msgSET_unq <- result_msgSET %>%
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T)

#Filtering the cases with login dates before case open and after case close date and creating a variable before/after EL login
result_msgSET1 <- result_msgSET %>% 
  filter(is.na(myca_mobile_dt) | is.na(case_close_dt) | myca_mobile_dt <= case_close_dt | myca_mobile_dt >= case_open_dt) %>% 
  mutate(mob_login = if_else(myca_mobile_dt <= message_date, "beforeEL", "afterEL")) 

# Counting the number of logins in any particular period (logins before EL or after EL Notice)
result_msgSET1 <- result_msgSET1 %>% 
  group_by(pseudo_key, case_grp_cd, case_seq_nbr) %>% 
  mutate(mobile_login_count = sum(mobile)) %>% 
  mutate(mobile_login_count_before = sum(case_when(mob_login == "beforeEL" ~ mobile, TRUE ~ 0)),
         mobile_login_count_after = sum(case_when(mob_login == "afterEL" ~ mobile, TRUE ~ 0))) %>% 
  ungroup()
    
#Looking at the unique triplicates (Not required to Run)
result_msgSET1_unq <- result_msgSET1 %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T)

# Detailed Lookout: Classification of the login periods (before/after/both) of early legal notice
result_msgSET1 <- result_msgSET1 %>%
  mutate(mobile_login_period = case_when(
    mob_login == "beforeEL" & !(pseudo_key %in% result_msgSET1$pseudo_key[result_msgSET1$mob_login == "afterEL"]) ~ "before_only",
    mob_login == "afterEL" & !(pseudo_key %in% result_msgSET1$pseudo_key[result_msgSET1$mob_login == "beforeEL"]) ~ "after_only",
    mob_login == "afterEL" & (pseudo_key %in% result_msgSET1$pseudo_key[result_msgSET1$mob_login == "beforeEL"]) ~ "both_BA",
    mob_login == "beforeEL" & (pseudo_key %in% result_msgSET1$pseudo_key[result_msgSET1$mob_login == "afterEL"]) ~ "both_BA",
    TRUE ~ NA
  )) 

#Final mobile login filtered unique data-set
mob_logins <- result_msgSET1 %>% 
  select(-myca_mobile_dt, -message_date, -case_close_dt, -case_open_dt) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T)

export(mob_logins, "mob_logins_done.Rdata")

######################################################################
#Creating the variables of interest in the MYCA WEB DATASET
######################################################################

myca_web1 <- myca_web %>% 
  mutate(web = 1) 

result_msgSET_web <- myca_web1 %>%
  left_join(early_legal123, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr"))

#Looking at the unique triplicates (Not Required to Run)
result_msgSET_web_unq <- result_msgSET_web %>%
  distinct(pseudo_key, case_grp_cd, case_seq_nbr)

#Filtering the cases with login dates before case open and after case close date and creating a variable before/after EL login
result_msgSET_web1 <- result_msgSET_web %>% 
  filter(is.na(myca_dt) | is.na(case_close_dt) | myca_dt <= case_close_dt | myca_dt >= case_open_dt) %>% 
  mutate(web_login = if_else(myca_dt <= message_date, "beforeEL", "afterEL")) 

# Counting the number of logins before EL and after EL  
result_msgSET_web1 <- result_msgSET_web1 %>% 
  group_by(pseudo_key, case_grp_cd, case_seq_nbr) %>% 
  mutate(web_login_count = sum(web)) %>% 
  mutate(web_login_count_before = sum(case_when(web_login == "beforeEL" ~ web, TRUE ~ 0)),
         web_login_count_after = sum(case_when(web_login == "afterEL" ~ web, TRUE ~ 0))) %>% 
  ungroup()

#Looking at the unique triplicates (Not Required to Run)
result_msgSET_web1_unq <- result_msgSET_web1 %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr)

# Detailed Lookout: Classification of the login periods (before/after/both) of early legal notice
result_msgSET_web1 <- result_msgSET_web1 %>%
  mutate(web_login_period = case_when(
    web_login == "beforeEL" & !(pseudo_key %in% result_msgSET_web1$pseudo_key[result_msgSET_web1$web_login == "afterEL"]) ~ "before_only",
    web_login == "afterEL" & !(pseudo_key %in% result_msgSET_web1$pseudo_key[result_msgSET_web1$web_login == "beforeEL"]) ~ "after_only",
    web_login == "afterEL" & (pseudo_key %in% result_msgSET_web1$pseudo_key[result_msgSET_web1$web_login == "beforeEL"]) ~ "both_BA",
    web_login == "beforeEL" & (pseudo_key %in% result_msgSET_web1$pseudo_key[result_msgSET_web1$web_login == "afterEL"]) ~ "both_BA",
    TRUE ~ NA
  )) 

#Final web login filtered unique data-set
web_logins <- result_msgSET_web1 %>% 
  select(-myca_dt, -message_date, -case_close_dt, -case_open_dt) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T)

export(web_logins, "web_logins_done.Rdata")

######################################################################
#Combining the all three datasets: MYCA-MOB/WEBSITE AND EARLY LEGAL
######################################################################
mob_logins <- x
web_logins <- x

both_logins <- inner_join(mob_logins, web_logins, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) %>% 
  select(-mobile, -web) %>% 
  mutate(both = 1)

both_logins <- both_logins %>% 
  rename(both_mob_login_count = mobile_login_count,
         both_mob_login_count_before = mobile_login_count_before,
         both_mob_login_count_after = mobile_login_count_after,
         both_web_login_count = web_login_count,
         both_web_login_count_before = web_login_count_before,
         both_web_login_count_after = web_login_count_after,
         both_mob_login = mob_login,
         both_web_login = web_login,
         both_mobile_login_period = mobile_login_period,
         both_web_login_period = web_login_period) 

both_logins <- both_logins %>% 
  mutate(both_login_before = if_else(both_mob_login == "beforeEL" | both_web_login == "beforeEL", 1, 0),
         both_login_after = if_else(both_mob_login == "afterEL" | both_web_login == "afterEL", 1, 0))

both_logins <- both_logins %>% 
  mutate(both_login_count = both_mob_login_count + both_web_login_count,
         both_login_count_before = both_mob_login_count_before + both_web_login_count_before,
         both_login_count_after = both_mob_login_count_after + both_web_login_count_after) %>% 
  select(-both_mob_login_count_before, -both_mob_login_count_after, -both_web_login_count_before, -both_web_login_count_after, -both_mob_login, -both_web_login, -both_mobile_login_period, -both_web_login_period)
         


mobile_only <- anti_join(mob_logins, both_logins, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr"))
website_only <- anti_join(web_logins, both_logins, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr"))

combined_mob_el <- left_join(early_legal_unique_censored, mobile_only, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) 

combined_mob_web_el <- left_join(combined_mob_el, website_only, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) 

combined_all <- left_join(combined_mob_web_el, both_logins, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) 


# Creating the Dummy Variable for the 3 MYCA logins Variables: Mobile, Website and Both

combined_all <- combined_all %>% 
  mutate(mobile = case_when(mobile == 1 ~ 1, 
                            mobile == NA ~ 0,
                            TRUE ~ 0),
         web = case_when(web == 1 ~ 1, 
                         web == NA ~ 0,
                         TRUE ~ 0),
         both = case_when(both == 1 ~ 1, 
                          both == NA ~ 0,
                          TRUE ~ 0))

#########################################################
#Payers Vs Non-Payers
#########################################################

# Creating the dataset for Unique Non-Payers
nonpayers_unique <- early_legal %>%
  filter(portfo_w_lvl_cd == "051") %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr)

# Creating the dataset with all Non-Payers observations
nonpayers <- merge(early_legal, nonpayers_unique, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr"))

# Creating the dataset for Unique Payers (by excluding the unique cases sent to collection)
payers_unique <- anti_join(early_legal_unique, nonpayers_unique, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr"))

# Creating the data-set with all Payers observations (by excluding all the observations sent to collection)
payers <-  anti_join(early_legal, nonpayers, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr"))

#############################################################################################
#WORKING TO CREATE A DUMMY FOR PAYER VS NON PAYERS AND JOINING WITH THE ORIGINAL DATASET
##############################################################################################

payers1 <- payers %>% 
  mutate(payers_code = 1) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T) %>% 
  select(1:3, payers_code)

nonpayers1 <- nonpayers %>% 
  mutate(nonpayers_code = 1) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T) %>% 
  select(1:3, nonpayers_code) 

combined1 <- left_join(combined_all, payers1, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) 
combined2_all <- left_join(combined1, nonpayers1, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) 

#############################################################################################
#COMBINED DATASET: MANAGEMENT OF VARIABLE OF INTEREST
##############################################################################################
# Creating the Dummy Variable for the 3 MYCA logins Variables: Mobile, Website and Both

combined2_all <- combined2_all %>% 
  mutate(pvcDUM = case_when(payers_code == 1 ~ 1, 
                            nonpayers_code == 1 ~ 0, 
                            TRUE ~ 0))

#Censoring the data-sets for Early legal message sent after 29th August
combined2_all <- combined2_all %>% 
  filter(message_date <= "2022-08-29")

#Exporting the final combined dataset
combined2_all <- combined2_all %>% 
  select(-payers_code, -nonpayers_code)

# Looking at the summary of dependent variable (Not required to Run)
b <- combined2_all %>% 
  group_by(pvcDUM) %>% 
  summarise(freq = n()) %>% 
  ungroup () %>% 
  mutate(total = sum(freq),
         percentage = freq/total * 100)
b

#############################################################################################
#DATA CLEANING AND MANAGEMENT STUFF
##############################################################################################

# Removing the variables not used 
combined_all <- combined2_all %>%  
  select(-d_consumer, -psdkey, -psdkey_1, -portfo_sta_lvl_cd, -act_seq_nbr, -act_info_cd1, 
         -act_info_cd2, -act_info_cd3,-ctcpty, -ctcplace, -rule_nbr, -misc_amt, -digit_group, -grad_date, 
         -st_frp_case, -case_type_at_setup, -portfo_sta_lvl_cd, -act_tm)

# Filling up the NAs with 0 (for count variables) and "No Login" (for categorical variables) in mobile, web and both login counts in the final dataset
combined_all50 <- combined_all %>%
  mutate(
    mobile_login_count = if_else(is.na(mobile_login_count), 0, mobile_login_count),
    mobile_login_count_before = if_else(is.na(mobile_login_count_before), 0, mobile_login_count_before),
    mobile_login_count_after = if_else(is.na(mobile_login_count_after), 0, mobile_login_count_after),
    web_login_count_before = if_else(is.na(web_login_count), 0, web_login_count),
    web_login_count_after = if_else(is.na(web_login_count_after), 0, web_login_count_after),
    web_login_count = if_else(is.na(web_login_count), 0, web_login_count),
    both_login_count = if_else(is.na(both_login_count), 0, both_login_count),
    both_mob_login_count = if_else(is.na(both_mob_login_count), 0, both_mob_login_count),
    both_web_login_count = if_else(is.na(both_web_login_count), 0, both_web_login_count),
    both_login_count_before = if_else(is.na(both_login_count_before), 0, both_login_count_before),
    both_login_count_after = if_else(is.na(both_login_count_after), 0, both_login_count_after),
    both_login_before = if_else(is.na(both_login_before), 0, both_login_before),
    both_login_after = if_else(is.na(both_login_after), 0, both_login_after)) %>% 
  
  mutate(
    mob_login = if_else(is.na(mob_login), "No Login", mob_login),
    web_login = if_else(is.na(web_login), "No Login", web_login),
    mobile_login_period = if_else(is.na(mobile_login_period), "No Login", mobile_login_period),
    web_login_period = if_else(is.na(web_login_period), "No Login", web_login_period))

# Saving the final combined file
export(combined_all50, "Combined_Final.RData")

#Load the necessary datasets and creating the combined dataset 
early_legal <- combined_all50

#Calculating the length of case for each individual (Total length of the case and length from case open to early legal notice)
early_legal <- early_legal %>% 
  mutate(age_co_to_earlylegal = as.numeric(message_date - case_open_dt),
         age_el_to_caseclose = as.numeric(case_close_dt - message_date),
         total_age = as.numeric(case_close_dt - case_open_dt))

# Normalized Counts  
early_legal <- early_legal %>%   
  mutate(nrm_mob_login_count = mobile_login_count/total_age, 
         nrm_mob_login_count_before = mobile_login_count_before/age_co_to_earlylegal,
         nrm_mob_login_count_after = mobile_login_count_after/age_el_to_caseclose,
         
         nrm_web_login_count = web_login_count/total_age,
         nrm_web_login_count_before = web_login_count_before/age_co_to_earlylegal,
         nrm_web_login_count_after = web_login_count_after/age_el_to_caseclose,
         
         nrm_both_login_count = both_login_count/total_age,
         nrm_both_login_count_before = both_login_count_before/age_co_to_earlylegal,
         nrm_both_login_count_after = both_login_count_after/age_el_to_caseclose,
         
         nrm_phone_rpc = count_phone_rpc/total_age,
         nrm_phone_rpc_before = count_phone_rpc_before/age_co_to_earlylegal,
         nrm_phone_rpc_after = count_phone_rpc_after/age_el_to_caseclose,
         
         nrm_kept_promise = count_kept_promise/total_age,
         nrm_kept_promise_before = count_kept_promise_before/age_co_to_earlylegal,
         nrm_kept_promise_after = count_kept_promise_after/age_el_to_caseclose,
         
         nrm_brok_promise = count_brok_promise/total_age,
         nrm_brok_promise_before = count_brok_promise_before/age_co_to_earlylegal,
         nrm_brok_promise_after = count_brok_promise_after/age_el_to_caseclose,
         
         nrm_phone_inbound = count_phone_inbound/total_age,
         nrm_phone_inbound_before = count_phone_inbound_before/age_co_to_earlylegal,
         nrm_phone_inbound_after = count_phone_inbound_after/age_el_to_caseclose,
         
         nrm_phone_outbound = count_phone_outbound/total_age,
         nrm_phone_outbound_before = count_phone_outbound_before/age_co_to_earlylegal,
         nrm_phone_outbound_after = count_phone_outbound_after/age_el_to_caseclose,
         
         nrm_phone_outbound_attempts = count_phone_outbound_attempts/total_age,
         nrm_phone_outbound_attempts_before = count_phone_outbound_attempt_before/age_co_to_earlylegal,
         nrm_phone_outbound_attempts_after = count_phone_outbound_attempt_after/age_el_to_caseclose,
         
         nrm_chat = count_chat/total_age,
         nrm_chat_before = count_chat_before/age_co_to_earlylegal,
         nrm_chat_after = count_chat_after/age_el_to_caseclose,
         
         nrm_sms = count_sms/total_age,
         nrm_sms_before = count_sms_before/age_co_to_earlylegal,
         nrm_sms_after = count_sms_after/age_el_to_caseclose,
         
         nrm_email_sent = count_email/total_age,
         nrm_email_sent_before = count_email_before/age_co_to_earlylegal,
         nrm_email_sent_after = count_email_after/age_el_to_caseclose,
         
         nrm_email_received = count_email_received/total_age,
         
         nrm_letter_sent = count_letter_sent/total_age,
         nrm_letter_sent_before = count_letter_sent_before/age_co_to_earlylegal,
         nrm_letter_sent_after = count_letter_sent_after/age_el_to_caseclose,
         
         nrm_letter_received = count_letter_received/total_age,
         
         nrm_email_letter = count_email_letter_sent/total_age)

# Scaling all dollar-denominated variables by $1000 and creating the ratio of total exposure amount to total due amount 
early_legal <- early_legal %>%   
  mutate(sca_exposure = total_case_exposure/1000,
         sca_tca_due = total_case_amount_due/1000,
         sca_tp_due = total_past_due/1000,
         exposure_to_tca_due = sca_exposure/sca_tca_due,
         exposure_to_tp_due = sca_exposure/sca_tp_due,
         sca_co_exposure = co_total_case_exposure/1000,
         sca_co_tca_due = co_total_case_amount_due/1000,
         sca_co_tp_due = co_total_past_due/1000,
         co_exposure_to_tca_due = sca_exposure/sca_tca_due,
         co_exposure_to_tp_due = sca_exposure/sca_tp_due)

# Communication success Rate before the EL messsage
early_legal <- early_legal %>%   
  mutate(outbound_success_rate = count_phone_outbound_before/(count_phone_outbound_attempt_before + count_phone_outbound_before)*100)

summary(early_legal$outbound_success_rate)

# Normalizing the difference in various due amounts and exposure (by the total age of the case)
early_legal <- early_legal %>% 
  mutate(del_exposure_norm = (sca_exposure - sca_co_exposure)/age_co_to_earlylegal,
         del_tot_amt_due_norm = (sca_tca_due - sca_co_tca_due)/age_co_to_earlylegal ,
         del_past_due_amt_norm = (sca_tp_due - sca_co_tp_due)/age_co_to_earlylegal,
         del_FICO_score_norm = (case_cbr_score - co_case_cbr_score)/age_co_to_earlylegal,
         percent_change_FICO = (case_cbr_score - co_case_cbr_score)/co_case_cbr_score*100
         ) 

# Saving the final combined-scaled-&-normalized file
export(early_legal, "Final_scaled_normalized1.RData")
