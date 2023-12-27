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
load("~/Library/CloudStorage/Box-Box/Project AMEX/working_data/myca_mob.RData")
myca_mob <- x
load("~/Library/CloudStorage/Box-Box/Project AMEX/working_data/myca_web.RData")
myca_web <- x
load("~/Library/CloudStorage/Box-Box/Project AMEX/working_data/Imputed_earlylegal_all.RData")
early_legal <- x

## Creating the filter for the card member types (CUST Only, COMP Only and Both)
early_legal <- early_legal %>%
  mutate(cm_type = case_when(
    case_grp_cd == "CUST" & !(pseudo_key %in% early_legal$pseudo_key[early_legal$case_grp_cd == "COMP"]) ~ "cust_only",
    case_grp_cd == "COMP" & !(pseudo_key %in% early_legal$pseudo_key[early_legal$case_grp_cd == "CUST"]) ~ "comp_only",
    case_grp_cd == "COMP" & (pseudo_key %in% early_legal$pseudo_key[early_legal$case_grp_cd == "CUST"]) ~ "both",
    TRUE ~ "both"
    #TRUE ~ NA_character_   
  ))
early_legal

# Creating the uniform date format across the dataset
early_legal <- early_legal %>% 
  mutate (act_dt = as.Date(act_dt, format = "%d%b%Y"),
          case_open_dt = as.Date(case_open_dt, format = "%d%b%Y"),
          message_date = as.Date(message_date, format = "%m/%d/%Y"),
          case_close_dt = as.Date(case_close_dt, format = "%d%b%Y"))

# Looking for Right Party Contact 
early_legal <- early_legal %>% 
  group_by(pseudo_key) %>%
  mutate(RPC_modf = ifelse("OC" %in% act_type_cd | "IC" %in% act_type_cd, 1, 0)) %>%
  ungroup() 

# Unique Card members: 5,262
early_legal_unique <- early_legal %>%
  filter(act_type_cd == "EN") %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T) 

#Censoring the data-sets for Early legal message sent after 29th August
early_legal_unique_censored <- early_legal_unique %>% 
  filter(message_date <= "2022-08-29")


######################################################################
#Creating the variables of interest in the MYCA MOBILE DATASET
######################################################################
early_legal123 <- early_legal %>% 
  select(pseudo_key, case_grp_cd, case_seq_nbr, message_date, case_close_dt) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T)

myca_mob1 <- myca_mob %>% 
  mutate(mobile = 1) 
#  group_by(pseudo_key, case_grp_cd, case_seq_nbr) %>% 
#  mutate (mob_login_count = sum(mobile)) %>% 
#  ungroup

result_msgSET <- myca_mob1 %>%
  left_join(early_legal123, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) 

#Looking at the unique triplicates
result_msgSET_unq <- myca_mob1 %>%
  left_join(early_legal123, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T)

#Filtering the cases with login dates after case close date and creating a variable before/after EL login
result_msgSET1 <- result_msgSET %>% 
  filter(is.na(myca_mobile_dt) | is.na(case_close_dt) | myca_mobile_dt <= case_close_dt) %>% 
  mutate(loginEL = if_else(myca_mobile_dt < message_date, 1, 2)) %>% 
  mutate(loginEL = case_when(loginEL == 1 ~ "before",
                             loginEL == 2 ~ "after",
                             TRUE ~ NA)) 

#Looking at the unique triplicates
result_msgSET1_unq <- result_msgSET1 %>% 
  filter(is.na(myca_mobile_dt) | is.na(case_close_dt) | myca_mobile_dt <= case_close_dt) %>% 
  mutate(loginEL = if_else(myca_mobile_dt < message_date, 1, 2)) %>% 
  mutate(loginEL = case_when(loginEL == 1 ~ "before",
                             loginEL == 2 ~ "after",
                             TRUE ~ NA)) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T)

# Classification of the login periods (before/after/both) of early legal notice
result_msgSET11 <- result_msgSET1 %>%
  mutate(mobile_login_period = case_when(
    loginEL == "before" & !(pseudo_key %in% result_msgSET1$pseudo_key[result_msgSET1$loginEL == "after"]) ~ "before_only",
    loginEL == "after" & !(pseudo_key %in% result_msgSET1$pseudo_key[result_msgSET1$loginEL == "before"]) ~ "after_only",
    loginEL == "after" & (pseudo_key %in% result_msgSET1$pseudo_key[result_msgSET1$loginEL == "before"]) ~ "both_BA",
    loginEL == "before" & (pseudo_key %in% result_msgSET1$pseudo_key[result_msgSET1$loginEL == "after"]) ~ "both_BA",
    TRUE ~ NA
  )) 

# Counting the number of logins in any particular period (logins before/after/both EL Notice) 
result_msgSET11 <- result_msgSET11 %>% 
  group_by(pseudo_key, case_grp_cd, case_seq_nbr) %>% 
  mutate(mobile_login_count = sum(mobile)) %>% 
#  mutate(before_el_count = case_when(mobile_login_period == "before_only" ~ sum(mobile)),                          
#          after_el_count = case_when(mobile_login_period == "after_only" ~ sum(mobile)),
#          both_el_count =  case_when(mobile_login_period == "both_BA" ~ sum(mobile),
#                                     T ~ NA)) %>% 
ungroup()


#Final mobile login filtered unique data-set
mob_logins <- result_msgSET11 %>% 
  select(-myca_mobile_dt, -message_date, -case_close_dt, -loginEL) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T)

######################################################################
#Creating the variables of interest in the MYCA WEB DATASET
######################################################################

myca_web1 <- myca_web %>% 
  mutate(web = 1) 

result_msgSET_web <- myca_web1 %>%
  left_join(early_legal123, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr"))

#Looking at the unique triplicates
result_msgSET_web_unq <- result_msgSET_web %>%
  distinct(pseudo_key, case_grp_cd, case_seq_nbr)

#Filtering the cases with login dates after case close date and creating a variable before/after EL login
result_msgSET_web1 <- result_msgSET_web %>% 
  filter(is.na(myca_dt) | is.na(case_close_dt) | myca_dt <= case_close_dt) %>% 
  mutate(loginEL = if_else(myca_dt < message_date, 1, 2)) %>% 
  mutate(loginEL = case_when(loginEL == 1 ~ "before",
                             loginEL == 2 ~ "after",
                             TRUE ~ NA)) 

#Looking at the unique triplicates
result_msgSET_web1_unq <- result_msgSET_web1 %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr)

result_msgSET_web11 <- result_msgSET_web1 %>%
  mutate(web_login_period = case_when(
    loginEL == "before" & !(pseudo_key %in% result_msgSET_web1$pseudo_key[result_msgSET_web1$loginEL == "after"]) ~ "before_only",
    loginEL == "after" & !(pseudo_key %in% result_msgSET_web1$pseudo_key[result_msgSET_web1$loginEL == "before"]) ~ "after_only",
    loginEL == "after" & (pseudo_key %in% result_msgSET_web1$pseudo_key[result_msgSET_web1$loginEL == "before"]) ~ "both_BA",
    loginEL == "before" & (pseudo_key %in% result_msgSET_web1$pseudo_key[result_msgSET_web1$loginEL == "after"]) ~ "both_BA",
    TRUE ~ NA
  )) 

# Counting the number of logins in any particular period (logins before/after/both EL Notice) 
result_msgSET_web11 <- result_msgSET_web11 %>% 
  group_by(pseudo_key, case_grp_cd, case_seq_nbr) %>% 
  mutate(web_login_count = sum(web)) %>% 
  ungroup()

web_logins <- result_msgSET_web11 %>% 
  select(-myca_dt, -message_date, -loginEL) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T)

######################################################################
#Combining the all three datasets: MYCA-MOB/WEBSITE AND EARLY LEGAL
######################################################################
both_logins <- inner_join(mob_logins, web_logins, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) %>% 
  select(-mobile, -web) %>% 
  mutate(both = 1)

both_logins <- both_logins %>% 
  rename(both_mob_login_count = mobile_login_count,
         both_mob_login_period = mobile_login_period,
         both_web_login_count = web_login_count,
         both_web_login_period= web_login_period) %>% 
  select(-case_close_dt)

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
  mutate(payers_code = 1,
         rpc_payer = RPC_modf) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T) %>% 
  select(1:3, payers_code, rpc_payer)

nonpayers1 <- nonpayers %>% 
  mutate(nonpayers_code = 1,
         rpc_nonpayer = RPC_modf) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T) %>% 
  select(1:3, nonpayers_code, rpc_nonpayer) 

combined1 <- left_join(combined_all, payers1, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) 
combined2_all <- left_join(combined1, nonpayers1, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) 

#############################################################################################
#COMBINED DATASET: MANAGEMENT OF VARIABLE OF INTEREST
##############################################################################################
# Creating the Dummy Variable for the 3 MYCA logins Variables: Mobile, Website and Both

combined2_all <- combined2_all %>% 
  mutate(pvcDUM = case_when(payers_code == 1 ~ 1, 
                            nonpayers_code == 1 ~ 0, 
                            TRUE ~ 0),
         rpc = case_when(rpc_payer == 1 ~ 1,
                         rpc_nonpayer == 1 ~ 1,
                         TRUE ~ 0))

#Censoring the data-sets for Early legal message sent after 29th August
combined2_all <- combined2_all %>% 
  filter(message_date <= "2022-08-29")

#Exporting the final combined dataset
combined2_all <- combined2_all %>% 
  select(-RPC_modf, -payers_code, -rpc_payer, -nonpayers_code, -rpc_nonpayer)

#############################################################################################
#DATA CLEANING AND MANAGEMENT STUFF
##############################################################################################

combined_all <- combined2_all %>%  
  select(-case_close_dt.x, -case_close_dt.y, -d_consumer, -psdkey, -psdkey_1, -portfo_sta_lvl_cd, -act_seq_nbr, -act_info_cd1, 
         -act_info_cd2, -act_info_cd3,-ctcpty, -ctcplace, -rule_nbr, -misc_amt, -digit_group, -grad_date, 
         -st_frp_case, -cm_type, -case_type_at_setup)


combined_all50 <- combined_all %>%
  mutate(
    mobile_login_count = if_else(is.na(mobile_login_count), 0, mobile_login_count),
    web_login_count = if_else(is.na(web_login_count), 0, web_login_count),
    both_mob_login_count = if_else(is.na(both_mob_login_count), 0, both_mob_login_count),
    both_web_login_count = if_else(is.na(both_web_login_count), 0, both_web_login_count),
    both_login_count = (both_mob_login_count + both_web_login_count),
    both_login_count = if_else(is.na(both_login_count), 0, both_login_count)) %>% 
  
  mutate(
    mobile_login_period = if_else(is.na(mobile_login_period), "No Login", mobile_login_period),
    
    web_login_period = if_else(is.na(web_login_period), "No Login", web_login_period),
    
    both_mob_login_period = if_else(is.na(both_mob_login_period), "No Login", both_mob_login_period),
    
    both_web_login_period = if_else(is.na(both_web_login_period), "No Login", both_web_login_period))


export(combined_all50, "Caseopen_Dataset.RData")
