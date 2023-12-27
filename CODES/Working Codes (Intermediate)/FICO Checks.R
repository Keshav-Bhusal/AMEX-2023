##########################################################################
#DATE:5 Dec, 2023
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

#Loading the dataset
early_legal <- x

# Creating the dummy for "Consumer" and "Small Business"
early_legal <- early_legal %>% 
  mutate(consumer = if_else(cm_type %in% c("Customer_Only"), 1, 0),
         small_business = if_else(cm_type %in% c("Business_Only", "Both"), 1, 0)) %>% 
  mutate(nrm_email_letter_before = count_email_letter_sent_before/age_co_to_earlylegal,
         nrm_email_letter_after = count_email_letter_sent_after/age_el_to_caseclose)

# Checking the range of FICO Score  (437 - 847)
range(early_legal$case_cbr_score)

# Creating the FICO bins (Continuous) in 50 points interval 
early_legal <- early_legal %>% 
  mutate(FICO_1 = if_else(case_cbr_score <= 450, case_cbr_score, 0),
         FICO_2 = if_else(case_cbr_score > 450 & case_cbr_score <= 500, case_cbr_score, 0),
         FICO_3 = if_else(case_cbr_score > 500 & case_cbr_score <= 550, case_cbr_score, 0),
         FICO_4 = if_else(case_cbr_score > 550 & case_cbr_score <= 600, case_cbr_score, 0),
         FICO_5 = if_else(case_cbr_score > 600 & case_cbr_score <= 650, case_cbr_score, 0),
         FICO_6 = if_else(case_cbr_score > 650 & case_cbr_score <= 700, case_cbr_score, 0),
         FICO_7 = if_else(case_cbr_score > 700 & case_cbr_score <= 750, case_cbr_score, 0),
         FICO_8 = if_else(case_cbr_score > 750 & case_cbr_score <= 800, case_cbr_score, 0),
         FICO_9 = if_else(case_cbr_score > 800 & case_cbr_score <= 850, case_cbr_score, 0)) %>% 
  
  mutate(DUM_FICO_1 = if_else(case_cbr_score <= 450, 1, 0),
         DUM_FICO_2 = if_else(case_cbr_score > 450 & case_cbr_score <= 500, 1, 0),
         DUM_FICO_3 = if_else(case_cbr_score > 500 & case_cbr_score <= 550, 1, 0),
         DUM_FICO_4 = if_else(case_cbr_score > 550 & case_cbr_score <= 600, 1, 0),
         DUM_FICO_5 = if_else(case_cbr_score > 600 & case_cbr_score <= 650, 1, 0),
         DUM_FICO_6 = if_else(case_cbr_score > 650 & case_cbr_score <= 700, 1, 0),
         DUM_FICO_7 = if_else(case_cbr_score > 700 & case_cbr_score <= 750, 1, 0),
         DUM_FICO_8 = if_else(case_cbr_score > 750 & case_cbr_score <= 800, 1, 0),
         DUM_FICO_9 = if_else(case_cbr_score > 800 & case_cbr_score <= 850, 1, 0))


# LOOKING AT THE DISTRIBITION OF BINs
variable_names <- paste0("FICO_", 1:9)

# Create a function to calculate summary statistics
calculate_summary <- function(variable) {
  data.frame(
    observations = sum(variable != 0, na.rm = TRUE),
    mean = mean(variable[variable != 0], na.rm = TRUE),
    min = min(variable[variable != 0], na.rm = TRUE),
    max = max(variable[variable != 0], na.rm = TRUE)
  )
}

# Apply the function to each variable
summary_table <- early_legal %>%
  select(all_of(variable_names)) %>%
  summarize(across(all_of(variable_names), calculate_summary))

summary_table

# Creating the Interactions
early_legal$sb_FICO_1 <- early_legal$small_business * early_legal$FICO_1
early_legal$sb_FICO_2 <- early_legal$small_business * early_legal$FICO_2
early_legal$sb_FICO_3 <- early_legal$small_business * early_legal$FICO_3
early_legal$sb_FICO_4 <- early_legal$small_business * early_legal$FICO_4
early_legal$sb_FICO_5 <- early_legal$small_business * early_legal$FICO_5
early_legal$sb_FICO_6 <- early_legal$small_business * early_legal$FICO_6
early_legal$sb_FICO_7 <- early_legal$small_business * early_legal$FICO_7
early_legal$sb_FICO_8 <- early_legal$small_business * early_legal$FICO_8
early_legal$sb_FICO_9 <- early_legal$small_business * early_legal$FICO_9

early_legal$con_FICO_1 <- early_legal$consumer * early_legal$FICO_1
early_legal$con_FICO_2 <- early_legal$consumer * early_legal$FICO_2
early_legal$con_FICO_3 <- early_legal$consumer * early_legal$FICO_3
early_legal$con_FICO_4 <- early_legal$consumer * early_legal$FICO_4
early_legal$con_FICO_5 <- early_legal$consumer * early_legal$FICO_5
early_legal$con_FICO_6 <- early_legal$consumer * early_legal$FICO_6
early_legal$con_FICO_7 <- early_legal$consumer * early_legal$FICO_7
early_legal$con_FICO_8 <- early_legal$consumer * early_legal$FICO_8
early_legal$con_FICO_9 <- early_legal$consumer * early_legal$FICO_9

early_legal$Exposure_SmallBusiness <- early_legal$small_business * early_legal$sca_exposure
early_legal$Exposure_Consumer <- early_legal$consumer * early_legal$sca_exposure

early_legal$FICO_SmallBusinesss <- early_legal$small_business * early_legal$case_cbr_score
early_legal$FICO_Consumer <- early_legal$consumer * early_legal$case_cbr_score

# Model 1 ( With No Interaction)
model2 <- glm(pvcDUM ~ sb_exposure + con_exposure + setup_cdss + 
                FICO_1 + FICO_2 + FICO_3 + FICO_4 + FICO_5 + FICO_6 + FICO_7 + FICO_8 + FICO_9 +
                nrm_web_login_count_before + nrm_mob_login_count_before + nrm_both_login_count_before +
                phone_inbound_before + nrm_chat_before + nrm_sms_before +
                nrm_email_sent_before, 
                data = early_legal, family = binomial(link = "logit"))

summary(model2)

# Model 0 ( With Interaction: Small Business Vs Consumer)
model0 <- glm(pvcDUM ~ Exposure_SmallBusiness + Exposure_Consumer + setup_cdss + 
                FICO_SmallBusinesss + FICO_Consumer +
                nrm_web_login_count_after + nrm_mob_login_count_after + nrm_both_login_count_after +
                phone_inbound_after + nrm_chat_after + nrm_sms_after +
                nrm_email_sent_before, 
              data = early_legal, family = binomial(link = "logit"))

summary(model0)



# Model 2 ( With Interaction: Small Business Vs Consumer)
model2 <- glm(pvcDUM ~ sb_exposure + con_exposure + setup_cdss + 
                sb_FICO_1 + sb_FICO_2 + sb_FICO_3 + sb_FICO_4 + sb_FICO_5 + sb_FICO_6 + sb_FICO_7 + sb_FICO_8 + sb_FICO_9 +
                con_FICO_1 + con_FICO_2 + con_FICO_3 + con_FICO_4 + con_FICO_5 + con_FICO_6 + con_FICO_7 + con_FICO_8 + con_FICO_9 +
                nrm_web_login_count_before + nrm_mob_login_count_before + nrm_both_login_count_before +
                phone_inbound_before + nrm_chat_before + nrm_sms_before +
                nrm_email_sent_before, 
              data = early_legal, family = binomial(link = "logit"))

summary(model2)

######################################################################
# Trying with DUMMY (FICO Bins and Exposure)
######################################################################

# Creating the Interactions
early_legal$sb_DUMFICO_1 <- early_legal$small_business * early_legal$DUM_FICO_1
early_legal$sb_DUMFICO_2 <- early_legal$small_business * early_legal$DUM_FICO_2
early_legal$sb_DUMFICO_3 <- early_legal$small_business * early_legal$DUM_FICO_3
early_legal$sb_DUMFICO_4 <- early_legal$small_business * early_legal$DUM_FICO_4
early_legal$sb_DUMFICO_5 <- early_legal$small_business * early_legal$DUM_FICO_5
early_legal$sb_DUMFICO_6 <- early_legal$small_business * early_legal$DUM_FICO_6
early_legal$sb_DUMFICO_7 <- early_legal$small_business * early_legal$DUM_FICO_7
early_legal$sb_DUMFICO_8 <- early_legal$small_business * early_legal$DUM_FICO_8
early_legal$sb_DUMFICO_9 <- early_legal$small_business * early_legal$DUM_FICO_9

early_legal$con_DUMFICO_1 <- early_legal$consumer * early_legal$DUM_FICO_1
early_legal$con_DUMFICO_2 <- early_legal$consumer * early_legal$DUM_FICO_2
early_legal$con_DUMFICO_3 <- early_legal$consumer * early_legal$DUM_FICO_3
early_legal$con_DUMFICO_4 <- early_legal$consumer * early_legal$DUM_FICO_4
early_legal$con_DUMFICO_5 <- early_legal$consumer * early_legal$DUM_FICO_5
early_legal$con_DUMFICO_6 <- early_legal$consumer * early_legal$DUM_FICO_6
early_legal$con_DUMFICO_7 <- early_legal$consumer * early_legal$DUM_FICO_7
early_legal$con_DUMFICO_8 <- early_legal$consumer * early_legal$DUM_FICO_8
early_legal$con_DUMFICO_9 <- early_legal$consumer * early_legal$DUM_FICO_9

early_legal$sb_exposure <- early_legal$small_business * early_legal$sca_exposure
early_legal$con_exposure <- early_legal$consumer * early_legal$sca_exposure

# Model 3 ( With Interaction: Small Business Vs Consumer. * DUMMY FICO Bins)
model3 <- glm(pvcDUM ~ sb_exposure + con_exposure + setup_cdss + 
                sb_DUMFICO_1 + sb_DUMFICO_2 + sb_DUMFICO_3 + sb_DUMFICO_4 + sb_DUMFICO_5 + sb_DUMFICO_6 + sb_DUMFICO_7 + sb_DUMFICO_8 + sb_DUMFICO_9 +
                con_DUMFICO_1 + con_DUMFICO_2 + con_DUMFICO_3 + con_DUMFICO_4 + con_DUMFICO_5 + con_DUMFICO_6 + con_DUMFICO_7 + con_DUMFICO_8 + con_DUMFICO_9 +
                nrm_web_login_count_before + nrm_mob_login_count_before + nrm_both_login_count_before +
                phone_inbound_before + nrm_chat_before + nrm_sms_before +
                nrm_email_sent_before, 
              data = early_legal, family = binomial(link = "logit"))

summary(model3)