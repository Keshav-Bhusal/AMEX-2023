##########################################################################
#PS15
#DATE:28 Nov, 2023
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
library(scales)
library(reshape2)
library(margins)
library(jtools)
library(dotwhisker)
library(broom.mixed)
library(ggtext)

#Load the necessary data sets and creating the combined dataset 
early_legal <- x

checking_the_exposure <- early_legal %>% 
  mutate(thres_co_10k = if_else(co_total_case_exposure >= 10000, 1, 0),
         thres_co_15k = if_else(co_total_case_exposure >= 15000, 1, 0),
         thres_co_25k = if_else(co_total_case_exposure >= 25000, 1, 0),
         thres_co_20k = if_else(co_total_case_exposure >= 20000, 1, 0),
         thres_co_30k = if_else(co_total_case_exposure >= 30000, 1, 0),
         thres_co_40k = if_else(co_total_case_exposure >= 40000, 1, 0),
         thres_co_45k = if_else(co_total_case_exposure >= 45000, 1, 0),
         thres_co_55k = if_else(co_total_case_exposure >= 55000, 1, 0),
         thres_co_50k = if_else(co_total_case_exposure >= 50000, 1, 0),
         thres_co_60k = if_else(co_total_case_exposure >= 60000, 1, 0),
         thres_co_70k = if_else(co_total_case_exposure >= 70000, 1, 0),
         thres_co_80k = if_else(co_total_case_exposure >= 80000, 1, 0),
         thres_co_90k = if_else(co_total_case_exposure >= 90000, 1, 0),
         thres_co_100k = if_else(co_total_case_exposure >= 100000, 1, 0),
         thres_10k = if_else(total_case_exposure >= 10000, 1, 0),
         thres_15k = if_else(total_case_exposure >= 15000, 1, 0),
         thres_25k = if_else(total_case_exposure >= 25000, 1, 0),
         thres_20k = if_else(total_case_exposure >= 20000, 1, 0),
         thres_30k = if_else(total_case_exposure >= 30000, 1, 0),
         thres_40k = if_else(total_case_exposure >= 40000, 1, 0),
         thres_45k = if_else(total_case_exposure >= 45000, 1, 0),
         thres_55k = if_else(total_case_exposure >= 55000, 1, 0),
         thres_50k = if_else(total_case_exposure >= 50000, 1, 0),
         thres_60k = if_else(total_case_exposure >= 60000, 1, 0),
         thres_70k = if_else(total_case_exposure >= 70000, 1, 0),
         thres_80k = if_else(total_case_exposure >= 80000, 1, 0),
         thres_90k = if_else(total_case_exposure >= 90000, 1, 0),
         thres_100k = if_else(co_total_case_exposure >= 100000, 1, 0))

# Dataset for Consumer and Small Business
data_cons <- checking_the_exposure %>% 
  filter(cons == 1)

data_sm <- checking_the_exposure %>% 
  filter(small_business == 1)


### Case Open ######
checking_the_exposure$cons_co_FICO <- checking_the_exposure$co_case_cbr_score * checking_the_exposure$cons
checking_the_exposure$sm_bus_co_FICO <- checking_the_exposure$co_case_cbr_score * checking_the_exposure$small_business

model1 <- glm(pvcDUM ~ sca_co_tca_due +  sca_co_tp_due + 
                co_setup_cdss + co_tsr_max_prob + small_business +
                cons_co_FICO + sm_bus_co_FICO + thres_co_10k, 
              data = checking_the_exposure, family = binomial(link = "logit"))
              
model2 <- glm(pvcDUM ~ sca_co_tca_due  + sca_co_tp_due +     
              co_setup_cdss + co_tsr_max_prob + co_fico_categories + thres_co_10k,
              data = data_cons, family = binomial(link = "logit"))
    
model3 <- glm(pvcDUM ~ sca_co_tca_due  + sca_co_tp_due + 
              co_setup_cdss + co_tsr_max_prob + co_fico_categories + thres_co_10k,
              data = data_sm, family = binomial(link = "logit"))

stargazer(model1, model2, model3, 
          type="text",
          dep.var.labels = "Not Placed")


### Early Legal ####

checking_the_exposure$cons_FICO <- checking_the_exposure$case_cbr_score * checking_the_exposure$cons
checking_the_exposure$sm_bus_FICO <- checking_the_exposure$case_cbr_score * checking_the_exposure$small_business


# model11 <- glm(pvcDUM ~ sca_tca_due + sca_exposure + sca_tp_due + 
#                  setup_cdss + tsr_max_prob + small_business +
#                  cons_FICO + sm_bus_FICO + thres_10k, data = early_legal, family = binomial(link = "logit"))

model2 <- glm(pvcDUM ~ sca_co_tca_due  + sca_co_tp_due +     
                co_setup_cdss + co_tsr_max_prob + co_fico_categories + thres_co_55k,
              data = data_cons, family = binomial(link = "logit"))

model3 <- glm(pvcDUM ~ sca_co_tca_due  + sca_co_tp_due + 
                co_setup_cdss + co_tsr_max_prob + co_fico_categories + thres_co_55k,
              data = data_sm, family = binomial(link = "logit"))

model22 <- glm(pvcDUM ~ sca_tca_due + sca_tp_due + 
                 setup_cdss + tsr_max_prob + fico_categories + thres_55k,
               data = data_cons, family = binomial(link = "logit"))

model33 <- glm(pvcDUM ~ sca_tca_due  + sca_tp_due + 
                 setup_cdss + tsr_max_prob + fico_categories + thres_55k, 
               data = data_sm, family = binomial(link = "logit"))

stargazer(model2, model3, model22, model33,
          type="text",
          dep.var.labels = "Not Placed")

########### Before/After (General + Engagement) #######################

model_consumer_before <- glm(pvcDUM ~ sca_tca_due + sca_tp_due + 
                               setup_cdss + tsr_max_prob + 
                               phone_inbound_before + phone_outbound_before +
                               chat_before + sms_before + email_sent_before + email_letter_sent_before + 
                               mobile_login_before + web_login_before + both_login_before + thres_100k, 
                             data = data_cons, family = binomial(link = "logit"))

model_business_before <- glm(pvcDUM ~ sca_tca_due + sca_tp_due + 
                               setup_cdss + tsr_max_prob + 
                               phone_inbound_before + phone_outbound_before +
                               chat_before + sms_before + email_sent_before + email_letter_sent_before + 
                               mobile_login_before + web_login_before + both_login_before + thres_100k, 
                             data = data_sm, family = binomial(link = "logit"))

stargazer(model_consumer_before, model_business_before,  
          type="text",
          dep.var.labels = "Not Placed")

##################################################

a <- checking_the_exposure %>% 
  group_by(thres_10k) %>% 
  summarise(freq = n()) %>% 
  mutate(total = sum(freq),
         Percentage = (freq/total * 100)) %>% 
  ungroup() 

a

