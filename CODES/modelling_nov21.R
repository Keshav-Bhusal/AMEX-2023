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

############Data Cleaning and Management####################
# Creating the dummy for "Consumer" and "Small Business"
early_legal <- early_legal %>% 
  mutate(cons = if_else(case_grp_cd %in% c("CUST"), 1, 0),
         small_business = if_else(case_grp_cd %in% c("COMP"), 1, 0))

# Creating the bins for FICO and exposure at Case Open
early_legal <- early_legal %>%  
  mutate(co_exposure_bins = 
           case_when(co_total_case_exposure <= 10000 ~ "less than 10k",
                     co_total_case_exposure > 10000 & co_total_case_exposure <= 15000 ~ "Between 10k and 15k",
                     co_total_case_exposure > 15000 & co_total_case_exposure <= 20000 ~ "Between 15k and 20k",
                     co_total_case_exposure > 20000 & co_total_case_exposure <= 25000 ~ "Between 20k and 25k",
                     co_total_case_exposure > 25000 & co_total_case_exposure <= 30000 ~ "Between 25k and 30k",
                     co_total_case_exposure > 30000 & co_total_case_exposure <= 35000 ~ "Between 30k and 35k",
                     co_total_case_exposure > 35000 & co_total_case_exposure <= 40000 ~ "Between 35k and 40k",
                     co_total_case_exposure > 40000 & co_total_case_exposure <= 45000 ~ "Between 40k and 45k",
                     co_total_case_exposure > 45000 & co_total_case_exposure <= 50000 ~ "Between 45k and 50k",
                     co_total_case_exposure > 50000 & co_total_case_exposure <= 100000 ~ "Between 50k and 100k",
                     co_total_case_exposure > 100000 & co_total_case_exposure <= 200000 ~ "Between 100k and 200k",
                     co_total_case_exposure > 200000 & co_total_case_exposure <= 400000 ~ "Between 200k and 400k",
                     co_total_case_exposure > 400000 ~ "Above 400k")) %>% 
mutate(co_fico_categories = 
         case_when(co_case_cbr_score <= 579 ~ "Poor",
                   co_case_cbr_score > 579 & co_case_cbr_score <= 669 ~ "Fair",
                   co_case_cbr_score > 669 & co_case_cbr_score <= 739 ~ "Good",
                   co_case_cbr_score > 739 & co_case_cbr_score <= 799 ~ "Very Good",
                   co_case_cbr_score > 799 ~ "Exceptional"))

# Reorder the "fico_categories" levels with "Fair" as the reference level
early_legal$co_fico_categories <- factor(early_legal$co_fico_categories, 
                                      levels = c("Fair", "Poor", "Good", "Very Good", "Exceptional"))

early_legal$co_exposure_bins <- factor(early_legal$co_exposure_bins, 
                                    levels = c("Between 10k and 15k", "less than 10k",
                                               "Between 15k and 20k", "Between 20k and 25k", 
                                               "Between 25k and 30k", "Between 30k and 35k", 
                                               "Between 35k and 40k","Between 40k and 45k", 
                                               "Between 45k and 50k","Between 50k and 100k", 
                                               "Between 100k and 200k", "Between 200k and 400k", 
                                               "Above 400k"))
# Dataset for Consumer and Small Business
data_cons <- early_legal %>% 
  filter(cons == 1)

data_sm <- early_legal %>% 
  filter(small_business == 1)

############# Case Open: Logistic Models##############
# Creating a modified variable for interaction term
early_legal$cons_co_FICO <- early_legal$co_case_cbr_score * early_legal$cons
early_legal$sm_bus_co_FICO <- early_legal$co_case_cbr_score * early_legal$small_business


model1 <- glm(pvcDUM ~ sca_co_tca_due + sca_co_exposure + sca_co_tp_due + 
                co_setup_cdss + co_tsr_max_prob + small_business +
                cons_co_FICO + sm_bus_co_FICO, data = early_legal, family = binomial(link = "logit"))

summary(model1)


# Model with different set of CUST and COMP Categories (My group categories)
# check$cons_co_FICO <- check$co_case_cbr_score * check$consumer
# check$sm_bus_co_FICO <- check$co_case_cbr_score * check$business
# 
# 
# model2 <- glm(pvcDUM ~ sca_co_tca_due + sca_co_exposure + sca_co_tp_due + 
#                 co_setup_cdss + co_tsr_max_prob + small_business +
#                 cons_co_FICO + sm_bus_co_FICO, data = check, family = binomial(link = "logit"))
# 
# summary2 <- summary(model2)

# Consumer
model3 <- glm(pvcDUM ~ sca_co_tca_due + sca_co_exposure + sca_co_tp_due + 
                co_setup_cdss + co_tsr_max_prob + co_fico_categories,
              data = data_cons, family = binomial(link = "logit"))

summary(model3)

# Small Business
model4 <- glm(pvcDUM ~ sca_co_tca_due + sca_co_exposure + sca_co_tp_due + 
                co_setup_cdss + co_tsr_max_prob + co_fico_categories, 
              data = data_sm, family = binomial(link = "logit"))

summary(model4)


############# Case Open: General + Engagements ######################

early_legal$cons_co_FICO <- early_legal$co_case_cbr_score * early_legal$cons
early_legal$sm_bus_co_FICO <- early_legal$co_case_cbr_score * early_legal$small_business


model_gen_eng_all <- glm(pvcDUM ~ sca_co_tca_due + sca_co_exposure + sca_co_tp_due + 
                           co_setup_cdss + co_tsr_max_prob + 
                           phone_rpc + phone_inbound +  phone_outbound + chat + sms + 
                           email_sent + email_letter_sent + mobile + web + both +
                           cons_co_FICO + sm_bus_co_FICO, data = early_legal, family = binomial(link = "logit"))

summary(model_gen_eng_all)

# Consumer
data_cons$cons_co_FICO <- data_cons$co_case_cbr_score * data_cons$cons
data_cons$sm_bus_co_FICO <- data_cons$co_case_cbr_score * data_cons$small_business


model_gen_eng_consumer <- glm(pvcDUM ~ sca_co_tca_due + sca_co_exposure + sca_co_tp_due + 
                                co_setup_cdss + co_tsr_max_prob + 
                                phone_rpc + phone_inbound +  phone_outbound + chat + sms + 
                                email_sent + email_letter_sent + mobile + web + both, data = data_cons, family = binomial(link = "logit"))

summary(model_gen_eng_consumer)

# Small Business
data_sm$cons_co_FICO <- data_sm$co_case_cbr_score * data_sm$cons
data_sm$sm_bus_co_FICO <- data_sm$co_case_cbr_score * data_sm$small_business


model_gen_eng_business <- glm(pvcDUM ~ sca_co_tca_due + sca_co_exposure + sca_co_tp_due + 
                                co_setup_cdss + co_tsr_max_prob + 
                                phone_rpc + phone_inbound +  phone_outbound + chat + sms + 
                                email_sent + email_letter_sent + mobile + web + both, data = data_sm, family = binomial(link = "logit"))

summary(model_gen_eng_business)

############# Early Legal: Logistic Model ######
# Creating a modified variable for interaction term
early_legal$cons_FICO <- early_legal$case_cbr_score * early_legal$cons
early_legal$sm_bus_FICO <- early_legal$case_cbr_score * early_legal$small_business


model11 <- glm(pvcDUM ~ sca_tca_due + sca_exposure + sca_tp_due + 
                setup_cdss + tsr_max_prob + small_business +
                cons_FICO + sm_bus_FICO, data = early_legal, family = binomial(link = "logit"))

summary(model11)

# Consumer
model22 <- glm(pvcDUM ~ sca_tca_due + sca_exposure + sca_tp_due + 
                 setup_cdss + tsr_max_prob + fico_categories,
               data = data_cons, family = binomial(link = "logit"))

summary(model22)

# Small Business
model33 <- glm(pvcDUM ~ sca_tca_due + sca_exposure + sca_tp_due + 
                 setup_cdss + tsr_max_prob + fico_categories, 
               data = data_sm, family = binomial(link = "logit"))

summary(model33)

############# Early Legal: General + Engagements ######################
early_legal$cons_FICO <- early_legal$case_cbr_score * early_legal$cons
early_legal$sm_bus_FICO <- early_legal$case_cbr_score * early_legal$small_business


model_gen_eng_all <- glm(pvcDUM ~ sca_tca_due + sca_exposure + sca_tp_due + 
                           setup_cdss + tsr_max_prob + 
                           phone_rpc + phone_inbound +  phone_outbound + chat + sms + 
                           email_sent + email_letter_sent + mobile + web + both +
                           cons_FICO + sm_bus_FICO, data = early_legal, family = binomial(link = "logit"))

summary(model_gen_eng_all)

# Consumer
data_cons$cons_FICO <- data_cons$case_cbr_score * data_cons$cons
data_cons$sm_bus_FICO <- data_cons$case_cbr_score * data_cons$small_business


model_gen_eng_consumer <- glm(pvcDUM ~ sca_tca_due + sca_exposure + sca_tp_due + 
                                setup_cdss + tsr_max_prob + 
                                phone_rpc + phone_inbound +  phone_outbound + chat + sms + 
                                email_sent + email_letter_sent + mobile + web + both, data = data_cons, family = binomial(link = "logit"))

summary(model_gen_eng_consumer)

# Small Business
data_sm$cons_FICO <- data_sm$case_cbr_score * data_sm$cons
data_sm$sm_bus_FICO <- data_sm$case_cbr_score * data_sm$small_business


model_gen_eng_business <- glm(pvcDUM ~ sca_tca_due + sca_exposure + sca_tp_due + 
                                setup_cdss + tsr_max_prob + 
                                phone_rpc + phone_inbound +  phone_outbound + chat + sms + 
                                email_sent + email_letter_sent + mobile + web + both, data = data_sm, family = binomial(link = "logit"))

summary(model_gen_eng_business)


############# Early Legal: General + Engagements (Before and After EL)  ######################
early_legal$cons_FICO <- early_legal$case_cbr_score * early_legal$cons
early_legal$sm_bus_FICO <- early_legal$case_cbr_score * early_legal$small_business

model_before <- glm(pvcDUM ~ sca_tca_due + sca_exposure + sca_tp_due + 
                           setup_cdss + tsr_max_prob + 
                           phone_inbound_before + phone_outbound_before +
                           chat_before + sms_before + email_sent_before + email_letter_sent_before + 
                           mobile_login_before + web_login_before + both_login_before +
                           cons_FICO + sm_bus_FICO, data = early_legal, family = binomial(link = "logit"))

summary(model_before)

model_after <- glm(pvcDUM ~ sca_tca_due + sca_exposure + sca_tp_due + 
                      setup_cdss + tsr_max_prob + 
                      phone_inbound_after + phone_outbound_after +       
                      chat_after + sms_after + email_sent_after + email_letter_sent_after + 
                      mobile_login_after + web_login_after + both_login_after +
                      cons_FICO + sm_bus_FICO, data = early_legal, family = binomial(link = "logit"))

summary(model_after)

# Consumer

model_consumer_before <- glm(pvcDUM ~ sca_tca_due + sca_exposure + sca_tp_due + 
                      setup_cdss + tsr_max_prob + 
                      phone_inbound_before + phone_outbound_before +
                      chat_before + sms_before + email_sent_before + email_letter_sent_before + 
                      mobile_login_before + web_login_before + both_login_before, 
                      data = data_cons, family = binomial(link = "logit"))

summary(model_consumer_before)

model_consumer_after <- glm(pvcDUM ~ sca_tca_due + sca_exposure + sca_tp_due + 
                     setup_cdss + tsr_max_prob + 
                     phone_inbound_after + phone_outbound_after +       
                     chat_after + sms_after + email_sent_after + email_letter_sent_after + 
                     mobile_login_after + web_login_after + both_login_after, 
                     data = data_cons, family = binomial(link = "logit"))

summary(model_consumer_after)

# Small Business

model_business_before <- glm(pvcDUM ~ sca_tca_due + sca_exposure + sca_tp_due + 
                               setup_cdss + tsr_max_prob + 
                               phone_inbound_before + phone_outbound_before +
                               chat_before + sms_before + email_sent_before + email_letter_sent_before + 
                               mobile_login_before + web_login_before + both_login_before, 
                             data = data_sm, family = binomial(link = "logit"))

summary(model_business_before)

model_business_after <- glm(pvcDUM ~ sca_tca_due + sca_exposure + sca_tp_due + 
                              setup_cdss + tsr_max_prob + 
                              phone_inbound_after + phone_outbound_after +       
                              chat_after + sms_after + email_sent_after + email_letter_sent_after + 
                              mobile_login_after + web_login_after + both_login_after, 
                            data = data_sm, family = binomial(link = "logit"))

summary(model_business_after)

############ Summary of the Models #############

stargazer(model_before, model_business_before, model_consumer_before, 
          type="text",
          dep.var.labels = "Not Placed")

stargazer(model_after, model_business_after, model_consumer_after, 
          type="text",
          dep.var.labels = "Not Placed")

# covariate.labels=c("MYCA Mob Only (Dum)","MYCA Web Only (Dum)",
#                    "MYCA Both (Dum)",
#                    "Right Party Contact","Credit Bureau Score",
#                    "TSR Score","CDSS Score","Consumer (Dum)",
#                    "Days Past Billed","Exposure",
#                    "Total Case Amount Due","Total Past Due",
#                    "Exposure per Total Due"), out="msg_models.txt"

# Summary of the models 
stargazer(model1, model11, model4, model3, model33, model22,
          type="text",
          dep.var.labels = "Not Placed")


########### Machine Learning: Random Forest ##########################

# load packages
library(ranger)
library(tree)
library(randomForest)
library(pROC)

# Hyper parameter tuning
hyper_grid <- expand.grid(
  mtry       = c(2:16),
  node_size  = c(3, 4, 5, 7, 10),
  sample_size = c(.66, .75, .85),
  num_trees  = c(200, 300, 500),
  OOB_RMSE   = 0
)

# iterate over all possible tuning parameter combinations
# then select model with the best predictive power

for (i in 1:nrow(hyper_grid)) {
  # run rf
  rf_tune <- 
    ranger(
      formula         = pvcDUM ~ sca_tca_due + sca_exposure + sca_tp_due + 
                        setup_cdss + tsr_max_prob + 
                        phone_inbound_before + phone_outbound_before +
                        chat_before + sms_before + email_sent_before + email_letter_sent_before + 
                        mobile_login_before + web_login_before + both_login_before +
                        cons_FICO + sm_bus_FICO,
      
      data            = early_legal, 
      probability     = TRUE,
      num.trees       = hyper_grid$num_trees[i],
      mtry            = hyper_grid$mtry[i],
      min.node.size   = hyper_grid$node_size[i],
      sample.fraction = hyper_grid$sample_size[i]
    )
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(rf_tune$prediction.error)     
}

# select best model (lowest root mean squared error)
best_model <- filter(hyper_grid, OOB_RMSE == min(OOB_RMSE))
best_model

# re-run RF with best tuning parameters
rf_final <- 
  ranger(
    formula         = pvcDUM ~ sca_tca_due + sca_exposure + sca_tp_due + 
                      setup_cdss + tsr_max_prob + 
                      phone_inbound_before + phone_outbound_before +
                      chat_before + sms_before + email_sent_before + email_letter_sent_before + 
                      mobile_login_before + web_login_before + both_login_before +
                      cons_FICO + sm_bus_FICO,
    
    data            = early_legal, 
    probability     = TRUE,
    importance      = "impurity", 
    num.trees       = best_model$num_trees,
    mtry            = best_model$mtry,
    min.node.size   = best_model$node_size,
    sample.fraction = best_model$sample_size
  )

# predict into sample
early_legal$notplaced_pred <- predict(rf_final, early_legal)$predictions[,1]
early_legal$placed_pred <- predict(rf_final, early_legal)$predictions[,2]

early_legal <- early_legal %>% 
  mutate(notplaced_char = ifelse(pvcDUM == 1, "Not Placed", "Placed"),
         notplaced_char = factor(notplaced_char,
                                levels = c("Not Placed", "Placed")))

#Evaluate Model Performance

ggplot(early_legal, 
       aes(x = notplaced_char, y = notplaced_pred)) +
  geom_jitter(alpha = 0.5) +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  labs(title = "Actual Not Placed Vs Predicted Not Placed Probabilities",
       x = "Actual Not Placed", 
       y = "Predicted Not Placed Probability") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14))

ggsave(filename = "RF_prediction_evaluation.png",
       height = 6,
       width = 9,
       dpi = 300)

# check out variable importance
vars <- ranger::importance(rf_final)
var_importance <- data.frame(Variable = names(vars), 
                             Importance = vars)

ggplot(var_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_segment(aes(xend = Variable, yend = 0), color = "blue") +
  geom_point(size = 3) + 
  coord_flip() +
  labs(title = "Variable Importance Measures",
       y = "Importance Metric",
       x = "Variable") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14))

ggsave(filename = "RF_variable_importance.png",
       height = 6,
       width = 9,
       dpi = 300)

# Compute the ROC curve
roc_rf <- roc(early_legal$pvcDUM, early_legal$notplaced_pred)

# Plot the ROC curve
plot(roc_rf, main="ROC Curve for Not Placed Prediction",
     col="#377eb8", lwd=2)
abline(a=0, b=1, lty=2, col="gray")

auc(roc_rf)

# Compare RF to Logit
# output predictions
early_legal$logit_pred <- predict(model_before, early_legal, "response")

# calculate ROC curves
roc_logit <- roc(early_legal$pvcDUM, early_legal$logit_pred)

# plot all ROC curves
plot(roc_rf, main="ROC Curves Comparison", col="#377eb8", lwd=2)
lines(roc_logit, col="red", lwd=2)
# lines(roc_probit, col="green", lwd=2)
legend("bottomright", legend=c("RF", "Logit"),
       col=c("#377eb8", "red"), lty=1, lwd=2) 

# Compare area under the curves
auc(roc_rf) 
auc(roc_logit)