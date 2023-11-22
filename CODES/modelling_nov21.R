##########################################################################
#PS13
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

table(early_legal$case_grp_cd)
table(early_legal$cm_type)
table(early_legal$case_grp_cd)

check <- early_legal %>% 
  mutate(cons = if_else(case_grp_cd %in% c("CUST"), 1, 0),
         small_business = if_else(case_grp_cd %in% c("COMP"), 1, 0))

##########################################################################
# Case Open: Logistic Model (General Model)
##########################################################################
# Creating a modified variable for interaction term
check$cons_co_FICO <- check$co_case_cbr_score * check$cons
check$sm_bus_co_FICO <- check$co_case_cbr_score * check$small_business


model1 <- glm(pvcDUM ~ sca_co_tca_due + sca_co_exposure + sca_co_tp_due + 
                co_setup_cdss + co_tsr_max_prob + small_business +
                cons_co_FICO + sm_bus_co_FICO, data = check, family = binomial(link = "logit"))

summary(model1)


check$cons_co_FICO <- check$co_case_cbr_score * check$consumer
check$sm_bus_co_FICO <- check$co_case_cbr_score * check$business


model1 <- glm(pvcDUM ~ sca_co_tca_due + sca_co_exposure + sca_co_tp_due + 
                co_setup_cdss + co_tsr_max_prob + small_business +
                cons_co_FICO + sm_bus_co_FICO, data = check, family = binomial(link = "logit"))

summary(model1)





##########################################################################
# Early Legal Notice: Logistic Model (General Model)
##########################################################################
















