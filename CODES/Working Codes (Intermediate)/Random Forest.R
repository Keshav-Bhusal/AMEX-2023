# ---- Set Up ----
# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(ranger)
library(pROC)

# this script will use random processes
# set.seed() will make the results reproducible
set.seed(2023)

early_legal_clean <- early_legal %>% 
  mutate(survived_char = ifelse(pvcDUM == 1, "Sent to Collection", "Not Sent"),
         survived_char = factor(survived_char,
                                levels = c("Sent to Collection", "Not Sent")))

ggplot(early_legal_clean) +
  geom_jitter(aes(x = case_cbr_score, y = cm_type, shape = survived_char)) +
  facet_wrap(~phone_rpc) +
  scale_shape_manual(values = c(20,4)) +
  labs(y = "cm_type",
       x = "case_cbr_score",
       color = NULL,
       shape = NULL) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14))

ggsave(filename = "titanic_jitter_plot.png",
       height = 6,
       width = 9,
       dpi = 300)

# ---- Random Forest Model ----

# Hyperparameter tuning
hyper_grid <- expand.grid(
  mtry       = c(2, 3, 4),
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
      formula         = survived ~ age + age_sq + income_class + female,
      data            = titanic_clean, 
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

# re-run RF with best tuning parameters
rf_final <- 
  ranger(
    formula         = survived ~ age + age_sq + income_class + female,
    data            = titanic_clean, 
    probability     = TRUE,
    importance      = "impurity", 
    num.trees       = best_model$num_trees,
    mtry            = best_model$mtry,
    min.node.size   = best_model$node_size,
    sample.fraction = best_model$sample_size
  )

# predict into sample
titanic_clean$survived_pred <- predict(rf_final, titanic_clean)$predictions[,1]
titanic_clean$perished_pred <- predict(rf_final, titanic_clean)$predictions[,2]


# ---- Evaluate Model Performance ----

ggplot(titanic_clean, 
       aes(x = survived_char, y = survived_pred)) +
  geom_jitter(alpha = 0.5) +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Survival Probabilities",
       x = "Actual Survival", 
       y = "Predicted Survival Probability") +
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
importance(rf_final)
var_importance <- data.frame(Variable = names(importance(rf_final)), 
                             Importance = importance(rf_final))

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
roc_rf <- roc(titanic_clean$survived, titanic_clean$survived_pred)

# Plot the ROC curve
plot(roc_rf, main="ROC Curve for Survival Prediction",
     col="#377eb8", lwd=2)
abline(a=0, b=1, lty=2, col="gray")

auc(roc_rf)

# ---- Compare RF to Logit and Probit ----

# logistic regression
logit_model <- glm(survived ~ age + age_sq + income_class + female, 
                   family = binomial(link = "logit"), data = titanic_clean)

# probit regression
probit_model <- glm(survived ~ age + age_sq + income_class + female, 
                    family = binomial(link = "probit"), data = titanic_clean)

# output predictions
early_legal$logit_pred <- predict(model1, early_legal, "response")
titanic_clean$probit_pred <- predict(probit_model, titanic_clean, "response")

# calculate ROC curves
roc_logit <- roc(titanic_clean$survived, titanic_clean$logit_pred)
roc_probit <- roc(titanic_clean$survived, titanic_clean$probit_pred)

# plot all ROC curves
plot(roc_rf, main="ROC Curves Comparison", col="#377eb8", lwd=2)
lines(roc_logit, col="red", lwd=2)
lines(roc_probit, col="green", lwd=2)
legend("bottomright", legend=c("RF", "Logit", "Probit"),
       col=c("#377eb8", "red", "green"), lty=1, lwd=2)

# Compare area under the curves
auc(roc_rf)
auc(roc_logit)
auc(roc_probit)

