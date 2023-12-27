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

early_legal$con_FICO_poor <- early_legal$consumer * early_legal$FICO_poor
early_legal$con_FICO_fair <- early_legal$consumer * early_legal$FICO_fair
early_legal$con_FICO_good <- early_legal$consumer * early_legal$FICO_good
early_legal$con_FICO_verygood <- early_legal$consumer * early_legal$FICO_very_good
early_legal$con_FICO_exceptional <- early_legal$consumer * early_legal$FICO_exceptional

model_before1 <- glm(pvcDUM ~ sca_tca_due + sca_tp_due + el_exposure_Q1 + el_exposure_Q2 + el_exposure_Q3 + el_exposure_Q4 + 
                      FICO_poor + FICO_fair + FICO_good + FICO_very_good + FICO_exceptional +  
                      setup_cdss + 
                      nrm_web_login_count_before + nrm_mob_login_count_before + nrm_both_login_count_before +
                      nrm_email_sent_before + nrm_email_letter_before + nrm_chat_before + nrm_sms_before +
                      nrm_phone_inbound_before + nrm_phone_outbound_before,
                      data = early_legal, family = binomial(link = "logit"))

summary(model_before1)

model_before2 <- glm(pvcDUM ~ sca_exposure + 
                       setup_cdss + 
                       nrm_web_login_count_before + nrm_mob_login_count_before + nrm_both_login_count_before +
                       nrm_email_sent_before + nrm_email_letter_before + nrm_chat_before + nrm_sms_before +
                       nrm_phone_inbound_before + nrm_phone_outbound_before + 
                       sb_FICO_poor + sb_FICO_fair + sb_FICO_good +
                       sb_FICO_verygood + sb_FICO_exceptional + con_FICO_poor + con_FICO_fair + con_FICO_good +
                       con_FICO_verygood + con_FICO_exceptional,
                     data = early_legal, family = binomial(link = "logit"))

summary(model_before2)

model_after1 <- glm(pvcDUM ~ sca_tca_due + sca_tp_due + el_exposure_Q1 + el_exposure_Q2 + el_exposure_Q3 + el_exposure_Q4 + 
                      FICO_poor + FICO_fair + FICO_good + FICO_very_good + FICO_exceptional +  
                      setup_cdss + 
                      nrm_web_login_count_after + nrm_mob_login_count_after + nrm_both_login_count_after +
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

modal_dummy_after1 <- glm(pvcDUM ~ sca_tca_due + sca_tp_due + el_exposure_Q1 + el_exposure_Q2 + el_exposure_Q3 + el_exposure_Q4 + 
                             FICO_poor + FICO_fair + FICO_good + FICO_very_good + FICO_exceptional +  
                             setup_cdss + 
                             web_login_after + mobile_login_after + both_login_after +
                             email_sent_after + email_letter_sent_after + chat_after + sms_after +
                             phone_inbound_after + phone_outbound_after,
                           data = early_legal, family = binomial(link = "logit"))

summary(modal_dummy_after1)

modal_dummy_before2 <- glm(pvcDUM ~ sca_tca_due + sca_tp_due + el_exposure_Q1 + el_exposure_Q2 + el_exposure_Q3 + el_exposure_Q4 + 
                             setup_cdss + 
                             web_login_before + mobile_login_before + both_login_before +
                             email_sent_before + email_letter_sent_before + chat_before + sms_before +
                             phone_inbound_before + phone_outbound_before + 
                             sb_FICO_poor + sb_FICO_fair + sb_FICO_good +
                             sb_FICO_verygood + sb_FICO_exceptional,
                             data = early_legal, family = binomial(link = "logit"))

modal_dummy_after2 <- glm(pvcDUM ~ sca_tca_due + sca_tp_due + el_exposure_Q1 + el_exposure_Q2 + el_exposure_Q3 + el_exposure_Q4 + 
                             setup_cdss + 
                             web_login_after + mobile_login_after + both_login_after +
                             email_sent_after + email_letter_sent_after + chat_after + sms_after +
                             phone_inbound_after + phone_outbound_after +
                            sb_FICO_poor + sb_FICO_fair + sb_FICO_good +
                            sb_FICO_verygood + sb_FICO_exceptional,
                           data = early_legal, family = binomial(link = "logit"))

summary(modal_dummy_before2)
summary(modal_dummy_after2)

stargazer(modal_dummy_before1, modal_dummy_before2,
          type="text",
          dep.var.labels = "Not Placed")

# load packages
library(ranger)
library(tree)
library(randomForest)
library(pROC)

# Hyper parameter tuning
hyper_grid <- expand.grid(
  mtry       = c(2:20),
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
      formula   =  pvcDUM ~ sca_tca_due + sca_tp_due + el_exposure_Q1 + el_exposure_Q2 + el_exposure_Q3 + el_exposure_Q4 + 
                   FICO_poor + FICO_fair + FICO_good + FICO_very_good + FICO_exceptional +  
                   setup_cdss + 
                   nrm_web_login_count_before + nrm_mob_login_count_before + nrm_both_login_count_before +
                   nrm_email_sent_before + nrm_email_letter_before + nrm_chat_before + nrm_sms_before +
                   nrm_phone_inbound_before + nrm_phone_outbound_before,
      
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
    formula = pvcDUM ~ sca_tca_due + sca_tp_due + el_exposure_Q1 + el_exposure_Q2 + el_exposure_Q3 + el_exposure_Q4 + 
      FICO_poor + FICO_fair + FICO_good + FICO_very_good + FICO_exceptional +  
      setup_cdss + 
      nrm_web_login_count_before + nrm_mob_login_count_before + nrm_both_login_count_before +
      nrm_email_sent_before + nrm_email_letter_before + nrm_chat_before + nrm_sms_before +
      nrm_phone_inbound_before + nrm_phone_outbound_before,
    
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
  labs(title = "BEFORE EL: Not Placed (Actual Vs Predicted) Probabilities",
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

# ggsave(filename = "RF_prediction_evaluation.png",
#        height = 6,
#        width = 9,
#        dpi = 300)

# check out variable importance
vars <- ranger::importance(rf_final)
var_importance <- data.frame(Variable = names(vars), 
                             Importance = vars)

ggplot(var_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_segment(aes(xend = Variable, yend = 0), color = "blue") +
  geom_point(size = 3) + 
  coord_flip() +
  labs(title = "BEFORE EL: Variable Importance Measures",
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

# ggsave(filename = "RF_variable_importance.png",
#        height = 6,
#        width = 9,
#        dpi = 300)

# Compute the ROC curve
roc_rf <- roc(early_legal$pvcDUM, early_legal$notplaced_pred)

# Plot the ROC curve
# plot(roc_rf, main="ROC Curve for Not Placed Prediction",
#      col="#377eb8", lwd=2)
# abline(a=0, b=1, lty=2, col="gray")
# 
# auc(roc_rf)

# Compare RF to Logit
# output predictions
early_legal$logit_pred <- predict(model_before1, early_legal, "response")

# calculate ROC curves
roc_logit <- roc(early_legal$pvcDUM, early_legal$logit_pred)

# plot all ROC curves
plot(roc_rf, main="BEFORE EL:OVERALL(GEN+ENG):ROC Curves Comparison", col="#377eb8", lwd=2)
lines(roc_logit, col="red", lwd=2)
# lines(roc_probit, col="green", lwd=2)
legend("bottomright", legend=c("RF", "Logit"),
       col=c("#377eb8", "red"), lty=1, lwd=2) 

# Compare area under the curves
auc(roc_rf) 
auc(roc_logit)










# Hyper parameter tuning
hyper_grid <- expand.grid(
  mtry       = c(2:20),
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
      formula   =  pvcDUM ~ sca_tca_due + sca_tp_due + el_exposure_Q1 + el_exposure_Q2 + el_exposure_Q3 + el_exposure_Q4 + 
        setup_cdss + 
        web_login_before + mobile_login_before + both_login_before +
        email_sent_before + email_letter_sent_before + chat_before + sms_before +
        phone_inbound_before + phone_outbound_before + 
        sb_FICO_poor + sb_FICO_fair + sb_FICO_good +
        sb_FICO_verygood + sb_FICO_exceptional,
      
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
    formula = pvcDUM ~ sca_tca_due + sca_tp_due + el_exposure_Q1 + el_exposure_Q2 + el_exposure_Q3 + el_exposure_Q4 + 
      setup_cdss + 
      web_login_before + mobile_login_before + both_login_before +
      email_sent_before + email_letter_sent_before + chat_before + sms_before +
      phone_inbound_before + phone_outbound_before + 
      sb_FICO_poor + sb_FICO_fair + sb_FICO_good +
      sb_FICO_verygood + sb_FICO_exceptional,
    
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
  labs(title = "BEFORE EL (INTERACTION): Not Placed (Actual Vs Predicted) Probabilities",
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

# ggsave(filename = "RF_prediction_evaluation.png",
#        height = 6,
#        width = 9,
#        dpi = 300)

# check out variable importance
vars <- ranger::importance(rf_final)
var_importance <- data.frame(Variable = names(vars), 
                             Importance = vars)

ggplot(var_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_segment(aes(xend = Variable, yend = 0), color = "blue") +
  geom_point(size = 3) + 
  coord_flip() +
  labs(title = "BEFORE EL (INTERACTION): Variable Importance Measures",
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

# ggsave(filename = "RF_variable_importance.png",
#        height = 6,
#        width = 9,
#        dpi = 300)

# Compute the ROC curve
roc_rf <- roc(early_legal$pvcDUM, early_legal$notplaced_pred)

# Plot the ROC curve
# plot(roc_rf, main="ROC Curve for Not Placed Prediction",
#      col="#377eb8", lwd=2)
# abline(a=0, b=1, lty=2, col="gray")
# 
# auc(roc_rf)

# Compare RF to Logit
# output predictions
early_legal$logit_pred <- predict(model_before1, early_legal, "response")

# calculate ROC curves
roc_logit <- roc(early_legal$pvcDUM, early_legal$logit_pred)

# plot all ROC curves
plot(roc_rf, main="BEFORE EL (INTERACTION):ROC Curves Comparison", col="#377eb8", lwd=2)
lines(roc_logit, col="red", lwd=2)
# lines(roc_probit, col="green", lwd=2)
legend("bottomright", legend=c("RF", "Logit"),
       col=c("#377eb8", "red"), lty=1, lwd=2) 

# Compare area under the curves
auc(roc_rf) 
auc(roc_logit)