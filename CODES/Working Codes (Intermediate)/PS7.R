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
library(gtsummary)
library(labelled)
library(stargazer)

#Load the necessary datasets and creating the dependent variable (Sent to collection = 1 and 0 = Otherwise)
early_legal <- newtry 


##########################################################################
# Normalizing 
##########################################################################
#Calculating the length of case for each individual 

early_legal <- early_legal %>% 
  mutate(date_diff = message_date - case_open_dt)


# Normalized Counts  
early_legal <- early_legal %>%   
  mutate(nrm_MLC = as.numeric(mobile_login_count)/as.numeric(date_diff), 
         nrm_MLC_before = as.numeric(mobile_login_count_before)/as.numeric(date_diff),
         nrm_WLC = as.numeric(web_login_count)/as.numeric(date_diff),
         nrm_WLC_before = as.numeric(web_login_count_before)/as.numeric(date_diff),
         nrm_both_login_count = as.numeric(both_login_count)/as.numeric(date_diff),
         nrm_both_login_count_before = as.numeric(both_login_count_before)/as.numeric(date_diff),
         nrm_both_login_count_after = as.numeric(both_login_count_after)/as.numeric(date_diff),
         nrm_phone_rpc = as.numeric(count_phone_rpc)/as.numeric(date_diff),
         nrm_phone_inbound = as.numeric(count_phone_inbound)/as.numeric(date_diff),
         nrm_phone_outbound = as.numeric(count_phone_outbound)/as.numeric(date_diff),
         nrm_phone_outbound_attempts = as.numeric(count_phone_outbound_attempts)/as.numeric(date_diff),
         nrm_chat = as.numeric(count_chat)/as.numeric(date_diff),
         nrm_email_sent = as.numeric(count_email)/as.numeric(date_diff),
         nrm_email_received = as.numeric(count_email_received)/as.numeric(date_diff),
         nrm_letter_sent = as.numeric(count_letter_sent)/as.numeric(date_diff),
         nrm_letter_received = as.numeric(count_letter_received)/as.numeric(date_diff),
         nrm_email_letter = as.numeric(count_email_letter_sent)/as.numeric(date_diff))
  
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
         co_exposure_to_tp_due = sca_exposure/sca_tp_due
         )

# Communication success Rate 

early_legal <- early_legal %>%   
  mutate(outbound_success_rate = count_phone_outbound_before/(count_phone_outbound_attempt_before + count_phone_outbound_before)*100)

summary(early_legal$outbound_success_rate)

# Models

model1 <- glm(pvcDUM ~ phone_rpc + phone_inbound + phone_outbound + chat + email_sent + letter_sent + email_letter_sent + 
                mobile + web + both + cm_type, data = early_legal, family = "binomial")

model2 <- glm(pvcDUM ~ 
                data = df, family = "binomial")

model3 <- glm(pvcDUM ~ 
                data = df, family = "binomial")

model3 <- glm(payers ~ myca_mob_only.x + myca_web_only.x + myca_both_only.x + rpc_yes.x + cbr_diff + tsr_diff + 
                d_consumer.x + dpb_diff + exp_total_ratio_diff,data = df, family = "binomial")

 stargazer(model1,model2, model3, type="text",
           dep.var.labels="Sent to Collection",
           covariate.labels=c("MYCA Mob Only (Dum)","MYCA Web Only (Dum)",
                              "MYCA Both (Dum)",
                              "Right Party Contact","Credit Bureau Score",
                              "TSR Score","CDSS Score","Consumer (Dum)",
                              "Days Past Billed","Exposure",
                              "Total Case Amount Due","Total Past Due",
                              "Exposure per Total Due"), out="msg_models.txt")

#Descriptive Statistics 

discriptives <- early_legal %>% 
  group_by(mobile, mobile_login_period) %>% 
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


