##########################################################################
#PS4
#DATE:12 Nov, 2023
#Author: Keshav Bhusal
#AREC 559 Advanced Applied Econometric 
#Affiliation: Department of Agriculture and Resource Economics(AREC)
##########################################################################


#Variables of Interest 

count_variables <- c(count_kept_promise,count_brok_promise, count_phone_rpc,                    
                     count_phone_inbound, count_phone_outbound , count_phone_outbound_attempts,      
                     count_chat, count_sms,  count_email,                        
                     count_email_received,count_letter_sent,count_letter_received   ,           
                     count_email_letter_sent, count_kept_promise_before,count_kept_promise_after,           
                     count_brok_promise_before,count_brok_promise_after ,count_phone_rpc_before,             
                     count_phone_rpc_after,count_phone_inbound_before ,count_phone_inbound_after,         
                     count_phone_outbound_before ,count_phone_outbound_after,count_phone_outbound_attempt_before,
                     count_phone_outbound_attempt_after,count_chat_before , count_chat_after,                  
                     count_sms_before ,count_sms_after , count_email_before ,                
                     count_email_after ,count_email_received_before ,count_email_received_after,       
                     count_letter_sent_before , count_letter_sent_after, count_letter_received_before ,      
                     count_letter_received_after, count_email_letter_sent_before, count_email_letter_sent_after)
                     
                  
normalized_variables <- c(nrm_mob_login_count ,nrm_mob_login_count_before,nrm_mob_login_count_after,          
                           nrm_web_login_count,nrm_web_login_count_before, nrm_web_login_count_after ,         
                           nrm_both_login_count, nrm_both_login_count_before , nrm_both_login_count_after ,        
                           nrm_phone_rpc  , nrm_phone_rpc_before ,nrm_phone_rpc_after,                
                           nrm_kept_promise , nrm_kept_promise_before, nrm_kept_promise_after ,            
                           nrm_brok_promise , nrm_brok_promise_before , nrm_brok_promise_after ,            
                           nrm_phone_inbound , nrm_phone_inbound_before , nrm_phone_inbound_after,
                           nrm_phone_outbound , nrm_phone_outbound_before , nrm_phone_outbound_after ,          
                           nrm_phone_outbound_attempts , nrm_phone_outbound_attempts_before , nrm_phone_outbound_attempts_after,  
                           nrm_chat, nrm_chat_before , nrm_chat_after,                  
                           nrm_sms ,nrm_sms_before , nrm_sms_after,                      
                           nrm_email_sent , nrm_email_sent_before , nrm_email_sent_after,               
                           nrm_email_received , nrm_letter_sent , nrm_letter_sent_before,             
                           nrm_letter_sent_after ,nrm_letter_received  , nrm_email_letter)

#Setting up the environment
library(rio)
library(ggplot2)  
library(dplyr)            
library(tidyr)  
library(readr)
library(VIM)
library(zoo)
library(gtsummary)
library(labelled)
library(stargazer)
library(scales)
library(reshape2)
library(margins)
library(jtools)
library(dotwhisker)
library(broom.mixed)
library(ggtext)

#Load the necessary data sets and creating the combined dataset 
early_legal <- x

# Imputing the missing values in FICO score at case open and early legal with the median FICO score
early_legal <- early_legal %>% 
  mutate(case_cbr_score = ifelse(is.na(case_cbr_score), median(case_cbr_score, na.rm = TRUE), case_cbr_score),
         co_case_cbr_score = ifelse(is.na(co_case_cbr_score), median(co_case_cbr_score, na.rm = TRUE), co_case_cbr_score))

# Creating the dummy for some missing or left over variables 
early_legal <- early_legal %>%   
  mutate(co_exposure_to_tca_due = sca_co_exposure/sca_co_tca_due,
         co_exposure_to_tp_due = sca_co_exposure/sca_co_tp_due) %>% 
  mutate(mobile_login_before = if_else(mobile_login_period %in% c("before_only", "both_BA"), 1,0),
         mobile_login_after = if_else(mobile_login_period %in% c("after_only", "both_BA"), 1,0),
         web_login_before = if_else(web_login_period %in% c("before_only", "both_BA"), 1,0),
         web_login_after = if_else(web_login_period %in% c("after_only", "both_BA"), 1,0))

# Creating the dummy for consumer and business type
early_legal <- early_legal %>% 
mutate(consumer = if_else(cm_type %in%  c("Customer_Only", "Both"), 1, 0),
       business = if_else(cm_type %in%  c("Business_Only", "Both"), 1, 0))

# Looking at the summary ("both" category -136 are evenly distributed in the categories: consumer (136 + 3601) and business (136 + 1047))
table(early_legal$cm_type)  
table(early_legal1$consumer)
table(early_legal1$business)

# Creating the bins for the exposure 
early_legal <- early_legal %>%   
  mutate(exposure_bins = 
           case_when(total_case_exposure <= 10000 ~ "less than 10k",
                     total_case_exposure > 10000 & total_case_exposure <= 15000 ~ "Between 10k and 15k",
                     total_case_exposure > 15000 & total_case_exposure <= 20000 ~ "Between 15k and 20k",
                     total_case_exposure > 20000 & total_case_exposure <= 25000 ~ "Between 20k and 25k",
                     total_case_exposure > 25000 & total_case_exposure <= 30000 ~ "Between 25k and 30k",
                     total_case_exposure > 30000 & total_case_exposure <= 35000 ~ "Between 30k and 35k",
                     total_case_exposure > 35000 & total_case_exposure <= 40000 ~ "Between 35k and 40k",
                     total_case_exposure > 40000 & total_case_exposure <= 45000 ~ "Between 40k and 45k",
                     total_case_exposure > 45000 & total_case_exposure <= 50000 ~ "Between 45k and 50k",
                     total_case_exposure > 50000 & total_case_exposure <= 100000 ~ "Between 50k and 100k",
                     total_case_exposure > 100000 & total_case_exposure <= 200000 ~ "Between 100k and 200k",
                     total_case_exposure > 200000 & total_case_exposure <= 400000 ~ "Between 200k and 400k",
                     total_case_exposure > 400000 ~ "Above 400k")) %>% 
  mutate(fico_categories = 
           case_when(case_cbr_score <= 579 ~ "Poor",
                     case_cbr_score > 579 & case_cbr_score <= 669 ~ "Fair",
                     case_cbr_score > 669 & case_cbr_score <= 739 ~ "Good",
                     case_cbr_score > 739 & case_cbr_score <= 799 ~ "Very Good",
                     case_cbr_score > 799 ~ "Exceptional"))

# Reorder the "fico_categories" levels with "Fair" as the reference level
early_legal$fico_categories <- factor(early_legal$fico_categories, 
                                      levels = c("Fair", "Poor", "Good", "Very Good", "Exceptional"))

# Reorder the "exposure_bins" levels with "less than 10k" as the reference level
early_legal$exposure_bins <- factor(early_legal$exposure_bins, 
                                    levels = c("Between 10k and 15k", "less than 10k",
                                               "Between 15k and 20k", "Between 20k and 25k", 
                                               "Between 25k and 30k", "Between 30k and 35k", 
                                               "Between 35k and 40k","Between 40k and 45k", 
                                               "Between 45k and 50k","Between 50k and 100k", 
                                               "Between 100k and 200k", "Between 200k and 400k", 
                                               "Above 400k"))

# Reorder the "cm_type" levels with "Business_Only" as the reference level
early_legal$cm_type <- factor(early_legal$cm_type, 
                                      levels = c("Both", "Business_Only", "Customer_Only"))

#Checking the summary of the categories: exposure_bins 
check <- early_legal %>% 
  group_by(exposure_bins) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  mutate(total = sum(freq))
check

#Checking the summary of the categories: fico_categories 
check1 <- early_legal %>% 
  group_by(fico_categories) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  mutate(total = sum(freq))
check1

#Checking the summary of the categories: fico_categories 
check2 <- early_legal %>% 
  group_by(cm_type) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  mutate(total = sum(freq))
check2

##########################################################################
#Prediction: Logistic Model (General)
##########################################################################

model1 <- glm(pvcDUM ~  del_FICO_score_norm + fico_categories +
              + co_exposure_to_tca_due + exposure_to_tca_due + exposure_bins
              + age_co_to_earlylegal + age_el_to_caseclose + 
              + phone_rpc + phone_inbound +  phone_outbound + brok_promise + chat + sms + email_sent + email_letter_sent + mobile + web + both
              + cm_type, data = early_legal, family = binomial(link = "logit"))

summary(model1)
margins(model1)

logit_margins <- summary(margins(model1))

stargazer(model1, logit_margins, type="text",
          dep.var.labels="Sent to Collection",
          covariate.labels=c("FICO Score", "Total Exposure", "Total Case Amount Due", "Total Past Due", "Exposure to TCA Due",
                             "Case Open to EL (Days)", "EL to Case Close (Days)", "Phone Contact", "Phone Inbound",  "Phone Outbound",
                             "Chat", "Email Sent", "letter Sent", "Email letter Sent", "Mobile Only", "Web Only",  "Both",  "Card Member Type"),
          out="msg_models.txt")

##########################################################################
#Prediction: Logistic Model (General Dummy Model)
##########################################################################

model1 <- glm(pvcDUM ~  fico_categories + exposure_bins
                + age_co_to_earlylegal + age_el_to_caseclose + 
                + phone_rpc + phone_inbound +  phone_outbound + chat + sms + email_sent + email_letter_sent + mobile + web + both
                + cm_type, data = early_legal, family = binomial(link = "logit"))

summary(model1)

#Plot coefficient 

# Creating the CI of models
tidy_model <- tidy(model1, conf.int = TRUE)

# Determine significance levels (adjust as needed)
alpha_levels <- c(0.01, 0.05, 0.1)

# Create a new variable for significance
tidy_model <- tidy_model %>%
  mutate(significance_level = ifelse(p.value < alpha_levels[1], "1%", 
                                     ifelse(p.value < alpha_levels[2], "5%", 
                                            ifelse(p.value < alpha_levels[3], "10%", "")))) %>% 
  filter(term!="(Intercept)")

# Plot using ggplot
ggplot(tidy_model, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, color = significance_level)) +
  geom_pointrange() +
  geom_vline(xintercept = 0, lty = 2) +
  scale_color_manual(values = c("black", "red", "steelblue", "purple")) +  
  labs(
    title = "Logistic Regression Coefficients with Confidence Intervals: OVERALL DUMMY MODEL",
    x = "Estimated Coefficient",
    y = "Variables"
  ) +
  theme_linedraw () +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold") 
  )

##########################################################################
#Prediction: Logistic Model (Normalized Count Model)
##########################################################################
model2 <- glm(pvcDUM ~  fico_categories + exposure_bins
                + age_co_to_earlylegal + age_el_to_caseclose + 
                + nrm_phone_rpc  + nrm_phone_outbound  + nrm_chat + nrm_sms + nrm_email_sent 
                + nrm_email_letter + nrm_mob_login_count + nrm_web_login_count + nrm_both_login_count
                + cm_type, data = early_legal, family = binomial(link = "logit"))

summary(model2)

#Plot coefficient 

# Creating the CI of models
tidy_model <- tidy(model2, conf.int = TRUE)

# Determine significance levels (adjust as needed)
alpha_levels <- c(0.01, 0.05, 0.1)

# Create a new variable for significance
tidy_model <- tidy_model %>%
  mutate(significance_level = ifelse(p.value < alpha_levels[1], "1%", 
                                     ifelse(p.value < alpha_levels[2], "5%", 
                                            ifelse(p.value < alpha_levels[3], "10%", "")))) %>% 
  filter(term!="(Intercept)")

# Plot using ggplot
ggplot(tidy_model, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, color = significance_level)) +
  geom_pointrange() +
  geom_vline(xintercept = 0, lty = 2) +
  scale_color_manual(values = c("black", "red", "steelblue", "purple")) +  
  labs(
    title = "Logistic Regression Coefficients with CI: NORMALIZED COUNT MODEL",
    x = "Estimated Coefficient",
    y = "Variables"
  ) +
  theme_linedraw () +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold") 
  )

##########################################################################
#Prediction: Logistic Model (Normalized Count Before/After Model)
##########################################################################

model3 <- glm(pvcDUM ~  fico_categories + exposure_bins
              + age_co_to_earlylegal + age_el_to_caseclose + 
              + nrm_phone_rpc_before + nrm_phone_rpc_after + nrm_phone_outbound_before + nrm_phone_outbound_after 
              + nrm_kept_promise_before + nrm_kept_promise_after +  nrm_brok_promise_before + nrm_brok_promise_after
              + nrm_chat_before + nrm_chat_after + nrm_sms_before + nrm_sms_after + nrm_email_sent_before 
              + nrm_email_sent_after
              + nrm_mob_login_count_before + nrm_mob_login_count_after
              + nrm_web_login_count_before + nrm_web_login_count_after
              + nrm_both_login_count_before + nrm_both_login_count_after
              + cm_type, data = early_legal, family = binomial(link = "logit"))

summary(model3)

#Plot coefficient 

# Creating the CI of models
tidy_model <- tidy(model3, conf.int = TRUE)

# Determine significance levels (adjust as needed)
alpha_levels <- c(0.01, 0.05, 0.1)

# Create a new variable for significance
tidy_model <- tidy_model %>%
  mutate(significance_level = ifelse(p.value < alpha_levels[1], "1%", 
                                     ifelse(p.value < alpha_levels[2], "5%", 
                                            ifelse(p.value < alpha_levels[3], "10%", "")))) %>% 
  filter(term!="(Intercept)")

# Plot using ggplot
ggplot(tidy_model, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, color = significance_level)) +
  geom_pointrange() +
  geom_vline(xintercept = 0, lty = 2) +
  scale_color_manual(values = c("black", "red", "steelblue", "purple")) +  
  labs(
    title = "Logistic Regression Coefficients with CI: NORMALIZED COUNT MODEL (BEFORE/AFTER)",
    x = "Estimated Coefficient",
    y = "Variables"
  ) +
  theme_linedraw () +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold") 
  )

##########################################################################
#Prediction: Logistic Model (Before/After DUMMY Model)
##########################################################################
model4 <- glm(pvcDUM ~  fico_categories + exposure_bins + 
                age_co_to_earlylegal + age_el_to_caseclose + 
                kept_promise_before + brok_promise_before + phone_inbound_before +
                phone_outbound_before + phone_outbound_attempt_before +
                chat_before + sms_before + email_sent_before + email_letter_sent_before + 
                mobile_login_before + web_login_before + both_login_before +
                kept_promise_after + brok_promise_after + phone_inbound_after + 
                phone_outbound_after + phone_outbound_attempt_after +       
                chat_after + sms_after + email_sent_after + email_letter_sent_after + 
                mobile_login_after + web_login_after + both_login_after +
                cm_type, data = early_legal, family = binomial(link = "logit"))

summary(model4)

#Plot coefficient 

# Creating the CI of models
tidy_model <- tidy(model4, conf.int = TRUE)

# Determine significance levels (adjust as needed)
alpha_levels <- c(0.01, 0.05, 0.1)

# Create a new variable for significance
tidy_model <- tidy_model %>%
  mutate(significance_level = ifelse(p.value < alpha_levels[1], "1%", 
                                     ifelse(p.value < alpha_levels[2], "5%", 
                                            ifelse(p.value < alpha_levels[3], "10%", "")))) %>% 
  filter(term!="(Intercept)")

# Plot using ggplot
ggplot(tidy_model, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, color = significance_level)) +
  geom_pointrange() +
  geom_vline(xintercept = 0, lty = 2) +
  scale_color_manual(values = c("black", "red", "steelblue", "purple")) +  
  labs(
    title = "Logistic Regression Coefficients with CI: DUMMY MODEL (BEFORE/AFTER)",
    x = "Estimated Coefficient",
    y = "Variables"
  ) +
  theme_linedraw () +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold") 
  )

##########################################################################
#Prediction: Logistic Model (Interaction Model)
##########################################################################

model1_1 <- glm(pvcDUM ~  fico_categories + exposure_bins 
              + age_co_to_earlylegal + age_el_to_caseclose + 
                + phone_rpc + phone_inbound +  phone_outbound + chat + sms + email_sent + email_letter_sent + mobile + web + both
              + cm_type, data = early_legal, family = binomial(link = "logit"))

summary(model1_1)


model2_1 <- glm(pvcDUM ~  fico_categories + exposure_bins
              + age_co_to_earlylegal + age_el_to_caseclose + 
                + nrm_phone_rpc  + nrm_phone_outbound  + nrm_chat + nrm_sms + nrm_email_sent 
              + nrm_email_letter + nrm_mob_login_count + nrm_web_login_count + nrm_both_login_count
              + cm_type, data = early_legal, family = binomial(link = "logit"))

summary(model2_1)


model3_1 <- glm(pvcDUM ~  fico_categories + exposure_bins
              + age_co_to_earlylegal + age_el_to_caseclose + 
                + nrm_phone_rpc_before + nrm_phone_rpc_after + nrm_phone_outbound_before + nrm_phone_outbound_after 
              + nrm_kept_promise_before + nrm_kept_promise_after +  nrm_brok_promise_before + nrm_brok_promise_after
              + nrm_chat_before + nrm_chat_after + nrm_sms_before + nrm_sms_after + nrm_email_sent_before 
              + nrm_email_sent_after
              + nrm_mob_login_count_before + nrm_mob_login_count_after
              + nrm_web_login_count_before + nrm_web_login_count_after
              + nrm_both_login_count_before + nrm_both_login_count_after
              + cm_type, data = early_legal, family = binomial(link = "logit"))

summary(model3_1)


model4_1 <- glm(pvcDUM ~  fico_categories + exposure_bins +  setup_cdss +
                age_co_to_earlylegal + age_el_to_caseclose + 
                kept_promise_before + brok_promise_before + phone_inbound_before +
                phone_outbound_before + phone_outbound_attempt_before +
                chat_before + sms_before + email_sent_before + email_letter_sent_before + 
                mobile_login_before + web_login_before + both_login_before +
                kept_promise_after + brok_promise_after + phone_inbound_after + 
                phone_outbound_after + phone_outbound_attempt_after +       
                chat_after + sms_after + email_sent_after + email_letter_sent_after + 
                mobile_login_after + web_login_after + both_login_after +
                cm_type, 
                data = early_legal, family = binomial(link = "logit"))

summary(model4_1)

##########################################################################
# Plotting the regression coefficient
##########################################################################

# Creating the CI of models
tidy_model <- tidy(model2, conf.int = TRUE)

# Determine significance levels (adjust as needed)
alpha_levels <- c(0.01, 0.05, 0.1)

# Create a new variable for significance
tidy_model <- tidy_model %>%
  mutate(significance_level = ifelse(p.value < alpha_levels[1], "1%", 
                      ifelse(p.value < alpha_levels[2], "5%", 
                             ifelse(p.value < alpha_levels[3], "10%", "")))) %>% 
  filter(term!="(Intercept)")

# Plot using ggplot
ggplot(tidy_model, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, color = significance_level)) +
  geom_pointrange() +
  geom_vline(xintercept = 0, lty = 2) +
  scale_color_manual(values = c("black", "red", "steelblue", "purple")) +  
  labs(
    title = "Logistic Regression Coefficients with Confidence Intervals",
    x = "Estimated Coefficient",
    y = "Variables"
  ) +
  theme_linedraw () +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold") 
  )


#theme_classic()
#theme_gray()
#theme_light()
#theme_dark()
#theme_linedraw()
#theme_dark() + theme(panel.grid = element_line(color = "white"))


##########################################################################
#Machine Learning 
#Lasso Model
##########################################################################

# Loading the library and setting seed for unique outcome
library(glmnet)
set.seed(400)

# Creating the data sets
early_legal1 <- early_legal %>% 
  select(- act_dt, -act_type_cd, - act_let_cd, -case_open_dt, -case_close_dt, -age_num_days,- message_date, -message_DPB, -portfo_w_lvl_cd, -co_exposure_to_tp_due, -outbound_success_rate) %>% 
  filter(!is.na(case_cbr_score)) %>% 
  filter(!is.na(co_case_cbr_score)) %>% 
  filter(age_el_to_caseclose != 0) %>% 
  na.omit()

as.factor(early_legal1$pvcDUM)

# Creating the training and test data sets
train <- sample(nrow(early_legal1) * 0.7)
train_set <- early_legal1[train, ]
test_set <- early_legal1[-train, ]

# Creating the training and testing matrices 
train.matrix <- model.matrix(glm(pvcDUM ~  fico_categories + exposure_bins + setup_cdss + case_cbr_score + tsr_max_prob +
                               total_case_amount_due + total_case_exposure + total_past_due + 
                               co_case_cbr_score + co_total_case_amount_due + co_total_case_exposure + co_total_past_due + 
                               product_type + age_co_to_earlylegal + age_el_to_caseclose + 
                               kept_promise + kept_promise_before + brok_promise + brok_promise_before + 
                               phone_rpc + phone_rpc_before + phone_inbound + phone_inbound_before +
                               phone_outbound + phone_outbound_before + phone_outbound_attempt +
                               phone_outbound_attempt_before + chat + sms + email_sent + web + both +
                               chat_before + sms_before + email_sent_before + email_letter_sent_before + 
                               mobile_login_before + web_login_before + both_login_before +
                               kept_promise_after + brok_promise_after + phone_rpc_after + phone_inbound_after + 
                               phone_outbound_after + phone_outbound_attempt_after +       
                               chat_after + sms_after + email_sent_after + email_letter_sent_after + 
                               mobile_login_after + web_login_after + both_login_after + nrm_mob_login_count + 
                               nrm_mob_login_count_before + nrm_mob_login_count_after +          
                               nrm_web_login_count + nrm_web_login_count_before + nrm_web_login_count_after +         
                               nrm_both_login_count + nrm_both_login_count_before + nrm_both_login_count_after +       
                               nrm_phone_rpc + nrm_phone_rpc_before  + nrm_phone_rpc_after+                
                               nrm_kept_promise + nrm_kept_promise_before + nrm_kept_promise_after +            
                               nrm_brok_promise + nrm_brok_promise_before + nrm_brok_promise_after +            
                               nrm_phone_inbound + nrm_phone_inbound_before + nrm_phone_inbound_after+
                               nrm_phone_outbound + nrm_phone_outbound_before + nrm_phone_outbound_after +          
                               nrm_phone_outbound_attempts + nrm_phone_outbound_attempts_before + nrm_phone_outbound_attempts_after+  
                               nrm_chat+ nrm_chat_before + nrm_chat_after +                  
                               nrm_sms+ nrm_sms_before + nrm_sms_after +                      
                               nrm_email_sent + nrm_email_sent_before+ nrm_email_sent_after +               
                               nrm_email_received + nrm_email_letter +
                               cm_type, data=train_set, family = binomial(link = "logit")))

   
test.matrix <- model.matrix(glm(pvcDUM ~  fico_categories + exposure_bins + setup_cdss + case_cbr_score + tsr_max_prob +
                                  total_case_amount_due + total_case_exposure + total_past_due + 
                                  co_case_cbr_score + co_total_case_amount_due + co_total_case_exposure + co_total_past_due + 
                                  product_type + age_co_to_earlylegal + age_el_to_caseclose + 
                                  kept_promise + kept_promise_before + brok_promise + brok_promise_before + 
                                  phone_rpc + phone_rpc_before + phone_inbound + phone_inbound_before +
                                  phone_outbound + phone_outbound_before + phone_outbound_attempt +
                                  phone_outbound_attempt_before + chat + sms + email_sent + web + both +
                                  chat_before + sms_before + email_sent_before + email_letter_sent_before + 
                                  mobile_login_before + web_login_before + both_login_before +
                                  kept_promise_after + brok_promise_after + phone_rpc_after + phone_inbound_after + 
                                  phone_outbound_after + phone_outbound_attempt_after +       
                                  chat_after + sms_after + email_sent_after + email_letter_sent_after + 
                                  mobile_login_after + web_login_after + both_login_after + 
                                  nrm_mob_login_count + nrm_mob_login_count_before + nrm_mob_login_count_after +          
                                  nrm_web_login_count + nrm_web_login_count_before + nrm_web_login_count_after +         
                                  nrm_both_login_count + nrm_both_login_count_before + nrm_both_login_count_after +       
                                  nrm_phone_rpc + nrm_phone_rpc_before  + nrm_phone_rpc_after+                
                                  nrm_kept_promise + nrm_kept_promise_before + nrm_kept_promise_after +            
                                  nrm_brok_promise + nrm_brok_promise_before + nrm_brok_promise_after +            
                                  nrm_phone_inbound + nrm_phone_inbound_before + nrm_phone_inbound_after+
                                  nrm_phone_outbound + nrm_phone_outbound_before + nrm_phone_outbound_after +          
                                  nrm_phone_outbound_attempts + nrm_phone_outbound_attempts_before + nrm_phone_outbound_attempts_after+  
                                  nrm_chat+ nrm_chat_before + nrm_chat_after +                  
                                  nrm_sms+ nrm_sms_before + nrm_sms_after +                      
                                  nrm_email_sent + nrm_email_sent_before+ nrm_email_sent_after +               
                                  nrm_email_received + nrm_email_letter +
                                  cm_type, data=train_set, family = binomial(link = "logit")))

# Now, check the dimensions
dim(train_set)
length(train_set$pvcDUM)

# Implementing a function over a grid of values ranging from $λ$ = 10^{10} to $λ = 10^{−2}$
grid <- 10^seq(10,-2,length=100)

# Fitting lasso model 
lasso_fit <- glmnet(train.matrix, train_set$pvcDUM, alpha=1, lambda=grid)
plot(lasso_fit, label = TRUE)

# Cross validating the model
cv_lasso <- cv.glmnet(train.matrix, train_set$pvcDUM, alpha=1)
plot(cv_lasso)

# Selecting the best lambda from the Cross Validation
lasso_best_lambda <- cv_lasso$lambda.min

cat('The best $λ$ from the cross validation =', lasso_best_lambda)

# Fitting a Lasso Model
pred_lasso <- predict(lasso_fit, s=lasso_best_lambda, newx = test.matrix)
lasso_mse <- mean((pred_lasso - test_set$pvcDUM)^2)

cat('The test MSE of the lasso model = :', lasso_mse ) 

# Non Zero Coefficient Estimates
lasso_coef <- predict(lasso_fit, s=lasso_best_lambda, type='coefficients')[1:110,]

# 4.8. Printing the Non Zero Coefficient Estimates
lasso_coef[lasso_coef!=0]
lasso_coef[lasso_coef == 0]

##########################################################################
#Machine Learning 
#Random Forest
##########################################################################
# load packages
library(ranger)
library(tree)
library(randomForest)

##########################################################################
Example
##########################################################################

library(ISLR2)
attach(Carseats)

colnames(Carseats)

High <- factor(ifelse(Sales <= 8, "No", "Yes"))
Carseats <- data.frame(Carseats , High)
tree.carseats <- tree(High ~ . - Sales, Carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats , pretty = 0)
tree.carseats

# Performance of classification using the test error
set.seed (2)

train <- sample (1: nrow(Carseats), 200)
Carseats.test <- Carseats[-train, ]
High.test <- High[-train]

tree.carseats <- tree(High ~ . - Sales, Carseats,
                        subset = train)

tree.pred <- predict(tree.carseats, Carseats.test ,
                       type = "class")

table(tree.pred , High.test)

# Accuracy Rate
(104 + 50) / 200

# Pruning using the cross validation (To see if it leads to better result)

# cv.tree() determine the optimal level of tree complexity; cost complexity pruning is used in order to select a sequence of trees for consideration.
# We use the argument FUN = prune.misclass in order to indicate that we want the classification error rate to guide the cross-validation and pruning process, 
# rather than the default for the cv.tree() function, which is deviance.

cv.carseats <- cv.tree(tree.carseats , FUN = prune.misclass) #performs cross-validation
names(cv.carseats)
cv.carseats

# Plot the error rate
par(mfrow = c(1, 1))
plot(cv.carseats$size , cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

# prune the tree to obtain the nine-node tree (Lowest CV Error: The tree with 9 terminal nodes results in only 74 cross-validation errors )
prune.carseats <- prune.misclass(tree.carseats , best = 9)
plot(prune.carseats)
text(prune.carseats , pretty = 0)

# Checking how well it predicted after pruning
tree.pred <- predict(prune.carseats , Carseats.test, type = "class")
table(tree.pred , High.test)

(97 + 58) / 200

##########################################################################
Trying
##########################################################################

# Creating a feasible dataset
early_legal1 <- early_legal %>% 
  select(-pseudo_key, -case_grp_cd, -case_seq_nbr, - act_dt, -act_type_cd, - act_let_cd, -case_open_dt, -case_close_dt, -age_num_days,- message_date, -message_DPB, -portfo_w_lvl_cd, -co_exposure_to_tp_due, -outbound_success_rate) %>% 
  filter(!is.na(case_cbr_score)) %>% 
  filter(!is.na(co_case_cbr_score)) %>% 
  filter(age_el_to_caseclose != 0) %>% 
  na.omit()

early_legal1$pvcDUM <- as.factor(early_legal1$pvcDUM)

#Checking for the infinite values in the datasets

# Identify columns with 'Inf' values
inf_columns <- sapply(early_legal1, function(x) any(is.infinite(x)))

# Remove 'Inf' values from identified columns
early_legal1[, inf_columns] <- lapply(early_legal1[, inf_columns], function(x) ifelse(is.infinite(x), NA, x))

# Remove rows with 'NA' values
early_legal1 <- na.omit(early_legal1)

# set.seed() will make the results reproducible
set.seed(2023)

# Creating the training and test data sets
train <- sample(nrow(early_legal1) * 0.7)
train_set <- early_legal1[train, ]
test_set <- early_legal1[-train, ]

#Tree model
model50 <- tree(pvcDUM ~ . , early_legal1)
summary(model50)

#Plotting the tree
plot(model50)
text(model50 , pretty = 0)

model50

# See if pruning is required or not 
cv.model50 <- cv.tree(model50)
plot(cv.model50$size , cv.model50$dev, type = "b")

# Looking at the MSE 
yhat <- predict(model50 , newdata = test_set)
boston.test <- test_set$pvcDUM
plot(yhat , boston.test)
abline (0, 1)
mean((yhat - boston.test)^2)

##########################################################################
# Bagging and Random Forest
##########################################################################

bag.el <- randomForest(pvcDUM ~ ., data = train_set, 
                           mtry = 181, importance = TRUE)

bag.el

plot(bag.el)
text(bag.el , pretty = 0)

yhat.bag <- predict(bag.el , newdata = test_set, type = "class")
plot(yhat.bag , test_set$pvcDUM)
abline (0, 1)
table(yhat.bag , test_set$pvcDUM)

(1211+139)/1395

importance(bag.el)
varImpPlot(bag.el)
