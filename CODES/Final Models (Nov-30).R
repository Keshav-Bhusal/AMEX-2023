##########################################################################
#PS15
#DATE:30 Nov, 2023
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

############# Early Legal: General + Engagements (Before and After EL)  ######################
early_legal$sb_FICO_poor <- early_legal$small_business * early_legal$FICO_poor
early_legal$sb_FICO_fair <- early_legal$small_business * early_legal$FICO_fair
early_legal$sb_FICO_good <- early_legal$small_business * early_legal$FICO_good
early_legal$sb_FICO_verygood <- early_legal$small_business * early_legal$FICO_very_good
early_legal$sb_FICO_exceptional <- early_legal$small_business * early_legal$FICO_exceptional

model_before1 <- glm(pvcDUM ~ sca_tca_due + sca_tp_due + el_exposure_Q1 + el_exposure_Q2 + el_exposure_Q3 + el_exposure_Q4 + 
                      FICO_poor + FICO_fair + FICO_good + FICO_very_good + FICO_exceptional +  
                      setup_cdss + 
                      nrm_web_login_count_before + nrm_mob_login_count_before + nrm_both_login_count_before +
                      nrm_email_sent_before + nrm_email_letter_before + nrm_chat_before + nrm_sms_before +
                      nrm_phone_inbound_before + nrm_phone_outbound_before,
                      data = early_legal, family = binomial(link = "logit"))

summary(model_before1)

model_before2 <- glm(pvcDUM ~ sca_tca_due + sca_tp_due + el_exposure_Q1 + el_exposure_Q2 + el_exposure_Q3 + el_exposure_Q4 + 
                       setup_cdss + 
                       nrm_web_login_count_before + nrm_mob_login_count_before + nrm_both_login_count_before +
                       nrm_email_sent_before + nrm_email_letter_before + nrm_chat_before + nrm_sms_before +
                       nrm_phone_inbound_before + nrm_phone_outbound_before + 
                       sb_FICO_poor + sb_FICO_fair + sb_FICO_good +
                       sb_FICO_verygood + sb_FICO_exceptional,
                     data = early_legal, family = binomial(link = "logit"))

summary(model_before2)

model_after1 <- glm(pvcDUM ~ sca_tca_due + sca_tp_due + el_exposure_Q1 + el_exposure_Q2 + el_exposure_Q3 + el_exposure_Q4 + 
                      FICO_poor + FICO_fair + FICO_good + FICO_very_good + FICO_exceptional +  
                      setup_cdss + 
                      nrm_web_login_count_after + nrm_mob_login_count_before + nrm_both_login_count_after +
                      nrm_email_sent_after + nrm_email_letter_after + nrm_chat_after + nrm_sms_after +
                      nrm_phone_inbound_after + nrm_phone_outbound_after,
                    data = early_legal, family = binomial(link = "logit"))

summary(model_after1)


model_after2 <- glm(pvcDUM ~ sca_tca_due + sca_tp_due + el_exposure_Q1 + el_exposure_Q2 + el_exposure_Q3 + el_exposure_Q4 + 
                       setup_cdss + 
                       nrm_web_login_count_after + nrm_mob_login_count_after + nrm_both_login_count_after +
                       nrm_email_sent_after + nrm_email_letter_after + nrm_chat_after + nrm_sms_after +
                       nrm_phone_inbound_after + nrm_phone_outbound_after + 
                       sb_FICO_poor + sb_FICO_fair + sb_FICO_good +
                       sb_FICO_verygood + sb_FICO_exceptional,
                     data = early_legal, family = binomial(link = "logit"))

summary(model_after2)

modal_dummy_before1 <- glm(pvcDUM ~ sca_tca_due + sca_tp_due + el_exposure_Q1 + el_exposure_Q2 + el_exposure_Q3 + el_exposure_Q4 + 
                            FICO_poor + FICO_fair + FICO_good + FICO_very_good + FICO_exceptional +  
                            setup_cdss + 
                            web_login_before + mobile_login_before + both_login_before +
                            email_sent_before + email_letter_sent_before + chat_before + sms_before +
                            phone_inbound_before + phone_outbound_before,
                            data = early_legal, family = binomial(link = "logit"))

summary(modal_dummy_before1)

modal_dummy_before2 <- glm(pvcDUM ~ sca_tca_due + sca_tp_due + el_exposure_Q1 + el_exposure_Q2 + el_exposure_Q3 + el_exposure_Q4 + 
                             setup_cdss + 
                             web_login_before + mobile_login_before + both_login_before +
                             email_sent_before + email_letter_sent_before + chat_before + sms_before +
                             phone_inbound_before + phone_outbound_before + sb_FICO_verygood + sb_FICO_exceptional,
                             data = early_legal, family = binomial(link = "logit"))
  
stargazer(modal_dummy_before1, modal_dummy_before2,
          type="text",
          dep.var.labels = "Not Placed")

stargazer(
  modal_dummy_before1, modal_dummy_before2,
  type = "text",
  dep.var.labels = "Not Placed",
  se = list(modal_dummy_before1$coefficients[, "Pr(>|z|)"]),  # Use Z-values
)

stargazer(
  modal_dummy_before1, modal_dummy_before2,
  type = "text",
  dep.var.labels = "Not Placed",
  se = list(modal_dummy_before1$coefficients[, "Pr(>|z|)"], modal_dummy_before2$coefficients[, "Pr(>|z|)"]),  # Use Z-values
)

