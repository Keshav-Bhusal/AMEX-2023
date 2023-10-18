##########################################################################
#PS4
#DATE:20th Sept, 2023
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
load("~/Library/CloudStorage/Box-Box/Project AMEX/myca_mob.RData")
myca_mob <- x
load("~/Library/CloudStorage/Box-Box/Project AMEX/myca_web.RData")
myca_web <- x

early_legal_all <- x


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
  filter(act_let_cd %in% c("GENLTPRL", "GENLTPON")) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T) 

#Censoring the data-sets for Early legal message sent after 29th August
early_legal_unique_censored <- early_legal_unique %>% 
  filter(message_date < "2022-08-29")

######################################################################
#Combining the all three datasets: MYCA-MOB/WEBSITE AND EARLY LEGAL
######################################################################

# Working to combine the data-sets 
myca_mob <- myca_mob %>% 
  mutate (mobile = 1) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T)

myca_web <- myca_web %>% 
  mutate (web = 2) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T)

both_logins <- inner_join(myca_mob, myca_web, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) %>% 
  select(-myca_mobile_dt, -myca_dt, -mobile, -web ) %>% 
  mutate(both = 3)
both_logins

mobile_only <- anti_join(myca_mob, both_logins, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr"))
website_only <- anti_join(myca_web, both_logins, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr"))

combined_mob_el <- left_join(early_legal_unique, mobile_only, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) %>% 
  select(-myca_mobile_dt)

combined_mob_web_el <- left_join(combined_mob_el, website_only,
                                 by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) %>% 
  select(-myca_dt)

combined_all <- left_join(combined_mob_web_el, both_logins, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) 

# Creating the Dummy Variable for the 3 MYCA logins Variables: Mobile, Website and Both
combined_all <- combined_all %>% 
  mutate(mobile = case_when(mobile == 1 ~ 1, 
                            mobile == NA ~ 0,
                            TRUE ~ 0),
         web = case_when(web == 2 ~ 1, 
                             web == NA ~ 0,
                             TRUE ~ 0),
         both = case_when(both == 3 ~ 1, 
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

#####################################################################
#LOOKING FOR THE NUMBER OF DAYS AFTER EL NOTICE PAYMENT WAS MADE 
#####################################################################

 payers_dt_count <- payers %>%
 mutate(msg_to_pmyt_days = ifelse(act_type_cd == "PY" & act_dt >= message_date, 
                                    as.numeric(difftime(act_dt, message_date, units = "days")),NA))  

# Summary of notice to payment dates among Payers 
payers_summary <- payers_dt_count %>%
  filter(!is.na(msg_to_pmyt_days)) %>% 
  group_by(msg_to_pmyt_days) %>%
  summarize(freq = n_distinct(pseudo_key, case_grp_cd, case_seq_nbr)) %>% 
  mutate(Totalfreq = sum(freq),
         Percent = (freq/Totalfreq * 100),
         Cumulative = cumsum(Percent)) %>% 
  select(msg_to_pmyt_days, freq, Totalfreq, Percent, Cumulative)

#Histogram of the days to payment after the EL Notice 
custom_break1 <- c(0, 7, 14, 21, 28, 50, 100, 150, 207)

p <- ggplot(payers_dt_count, aes(x = msg_to_pmyt_days)) +
  geom_histogram(fill = "purple", color = "black", na.rm = T) +
  labs(x = "EL Notice to Payment Days", y = "Frequency", title = "Number of Days the Payment was made after the EL Notice")+
  scale_x_continuous(breaks = custom_break1, labels = custom_break1)+
  scale_y_continuous(labels = scales::number_format(scale = 1))
p

custom_break1 <- c(0-7, 8-14, 15-21, 22-28, 29-50, 51-100, 101-150, 151-207)

p <- ggplot(payers_dt_count, aes(x = msg_to_pmyt_days)) +
  geom_histogram(aes(y = ..count../sum(..count..) * 100), 
                 fill = "purple", color = "black", binwidth = 15, na.rm = T) +
  labs(x = "EL Notice to Payment Days", y = "Percentage", 
       title = "Number of Days the Payment was made after the EL Notice") +
  scale_x_continuous(breaks = custom_break1, labels = custom_break1) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

p

#############################################################################################
#WORKING TO CREATE A DUMMY FOR PAYER VS NON PAYERS AND JOINING WITH THE ORIGINAL DATASET
##############################################################################################

payers1 <- payers %>% 
  mutate(payers_code =1,
         rpc_payer = RPC_modf) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T) %>% 
  select(1:3, payers_code, rpc_payer)
  
nonpayers1 <- nonpayers %>% 
  mutate(nonpayers_code =1,
         rpc_nonpayer = RPC_modf) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T) %>% 
  select(1:3, nonpayers_code, rpc_nonpayer) 

combined1 <- left_join(combined_all, payers1, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) 
combined2_all <- left_join(combined1, nonpayers1, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) 

# Creating the Dummy Variable for the 3 MYCA logins Variables: Mobile, Website and Both
combined2_all <- combined2_all %>% 
  mutate(pvcDUM = case_when(payers_code == 1 ~ 1, 
                            nonpayers_code == 1 ~ 0, 
                            TRUE ~ 0),
         rpc = case_when(rpc_payer == 1 ~ 1,
                         rpc_nonpayer == 1 ~ 1,
                         TRUE ~ 0))

check <- combined2_all%>% 
  group_by(pvcDUM, rpc) %>% 
  summarize(freq = n()) %>% 
  mutate(totalfreq = sum(freq),
         percentage = freq/totalfreq*100)
check


#Exporting the final combined dataset

combined2_all <- combined2_all %>% 
  select(-RPC_modf, -payers_code, -rpc_payer, -nonpayers_code, -rpc_nonpayer)

export(combined2_all, "combined_dataset.RData")

# Checking the correlation among the parameters in 'brfss' data set (We omit the parameter 'BMI and Obesity')

plotting <- combined2_all %>% 
  select(age_num_days, message_DPB, setup_cdss, total_case_exposure,total_case_amount_due, total_past_due, pvcDUM, rpc)
  
plotting <- na.omit(plotting)

corrplot(cor(plotting), method = "square")

cor(plotting)


# Logistic Regression
#1.1. Generating the training and test set
combined2_all_final <- combined2_all
set.seed(400)
train <- sample(nrow(combined2_all_final) * 0.8)
train_set <- combined2_all_final[train, ]
test_set <- combined2_all_final[-train, ]

# 3.2. Response vector for the training and test data sets
payersvscollection_train <- train_set$pvcDUM
payersvscollection_test <- test_set$pvcDUM

# 3.3. Fitting the logistic regression
fit_logistic <- glm(pvcDUM ~  age_num_days + factor(rpc) + factor(cm_type) + tsr_max_prob + 
                      setup_cdss + total_case_exposure + factor(product_type) + factor(mobile) +
                      factor(web)+factor(both),
                      data = combined2_all_final, 
                     family = binomial(link = "probit"))
summary(fit_logistic)

# 3.4. Creating a confusion matrix 
# 3.4.1. Predicting the probability
log_probs <- predict(fit_logistic, test_set, type = "response")

# 3.4.2. Creating the vector with all the '0' (No weight gain) elements
log_pred = rep(0, length(payersvscollection_test))

# 3.4.3. Creating the '1' (Weight gain/Obesity) elements which predicted probability exceeds 0.5
log_pred[log_probs > 0.5] <- 1

# 3.4.4. Confusion Matrix with the Test Data Set
table(log_pred, payersvscollection_test)

# 3.4.5. Fraction of the correct prediction 
mean(log_pred == payersvscollection_test)*100

# 3.4.6. Fraction of the in-correct prediction (Test set error rate)
mean(log_pred != payersvscollection_test)*100

# 3.4.7. False Positive Rate
table(log_pred, payersvscollection_test)[2,1]/sum(table(log_pred, payersvscollection_test)[,1])*100

# 3.4.8. False Negative Rate
table(log_pred, payersvscollection_test)[1,2]/sum(table(log_pred, payersvscollection_test)[,2])*100


# Verification for the collection

nonpayers <- nonpayers %>%
  mutate(msg_to_pmyt_days = difftime(act_dt, message_date, units = "days"))

nonpayers <- nonpayers %>%
  mutate(msg_to_pmyt_days1 = ifelse(act_type_cd == "PY" & act_dt >= message_date, 
                                    as.numeric(difftime(act_dt, message_date, units = "days")), NA))

## 224 people payed after the EL notice but did not pay enough and ENDED IN COLLECTION
nonpayers_check <- nonpayers %>% 
  filter(!is.na(msg_to_pmyt_days1)) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T) %>% 
  select(1:3, act_dt, message_date, msg_to_pmyt_days, msg_to_pmyt_days1, act_type_cd, everything())

nonpayers_check


#####################################################################
#LOOKING FOR THE outliers and missing values 
#####################################################################
# CBR Score 
any_missing <- any(is.na(early_legal$case_cbr_score))
any_missing

missing_CBR <- early_legal %>%
  group_by(case_cbr_score) %>% 
  summarize(freq = n()) %>% 
  ungroup() %>% 
  mutate(Totalfreq = sum(freq),
          Percent = (freq/Totalfreq * 100),
          Cumulative = cumsum(Percent)) %>% 
  select(case_cbr_score, freq, Totalfreq, Percent, Cumulative)

missing_CBR

# TSR Score
any_missing <- any(is.na(early_legal$tsr_max_prob))
any_missing

missing_TSR <- early_legal %>%
  group_by(tsr_max_prob) %>% 
  summarize(freq = n()) %>% 
  ungroup() %>% 
  mutate(Totalfreq = sum(freq),
         Percent = (freq/Totalfreq * 100),
         Cumulative = cumsum(Percent)) %>% 
  select(tsr_max_prob, freq, Totalfreq, Percent, Cumulative)


# CDSS Score
any_missing <- any(is.na(early_legal$setup_cdss))
any_missing

missing_CDSS <- early_legal %>%
  group_by(setup_cdss) %>% 
  summarize(freq = n()) %>% 
  ungroup() %>% 
  mutate(Totalfreq = sum(freq),
         Percent = (freq/Totalfreq * 100),
         Cumulative = cumsum(Percent)) %>% 
  select(setup_cdss, freq, Totalfreq, Percent, Cumulative)

plot_cdss_missing <- early_legal %>%
  filter(is.na(setup_cdss)) %>%
  ggplot(aes(x = setup_cdss)) +
  geom_histogram(fill = "blue", color = "black", bins = 3) +
  labs(title = "Distribution of Missing Values in setup_cdss",
       x = "setup_cdss",
       y = "Frequency")
plot_cdss_missing

# Looking at the distribution of missing values or out-liers

marginplot(early_legal[c(24,25)], col = c("blue", "red", "orange"))
marginplot(early_legal[c(29,30)], col = c("blue", "red", "orange"))


### PROBLEM SET 

# py_date <- payers %>% 
#   group_by(pseudo_key, case_grp_cd, case_seq_nbr) %>% 
#   mutate(first_py_date =ifelse(lag(message_date) <= act_dt & act_type_cd == "PY", act_dt, NA)) %>% 
#   fill(first_py_date, .direction = "up")