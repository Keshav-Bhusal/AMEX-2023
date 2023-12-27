##########################################################################
#PS4
#DATE:30 Oct, 2023
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

#Load the necessary datasets and creating the combined dataset 
early_legal <- x

##########################################################################
#Descriptive and Summary statistics 
##########################################################################
# Specify the binary columns 
binary_columns <- c("kept_promise", "brok_promise", "phone_rpc", "phone_inbound", "phone_outbound", "phone_outbound_attempts", 
                    "chat", "sms", "email_sent", "email_received", "letter_sent", 
                    "letter_received", "email_letter_sent", "mobile", "web", "both", "pvcDUM")

# Selecting the categorical variable of interest 
cat_vars <- early_legal %>% 
  select(kept_promise, brok_promise, phone_rpc, phone_inbound, phone_outbound, phone_outbound_attempts, chat, 
         sms, email_sent, email_received, letter_sent, letter_received, email_letter_sent, mobile, web, both, pvcDUM)

# Convert the binary columns to categorical variables
cat_vars <- cat_vars %>%
  mutate_at(vars(all_of(binary_columns)), ~ factor(., levels = c(0, 1), labels = c("No", "Yes")))

# Selecting the continuous variable of interest 
contn_vars <- early_legal %>% 
  select(count_phone_rpc:both_login_count_after, -both, -mobile, -web, -web_login, 
         -mob_login, -mobile_login_period, -web_login_period)

# Descriptive of the categorical variables 
#1. Using gtsummary

cat_summary <- cat_vars %>% 
  tbl_summary()

cat_summary
  
cat_summary <- cat_vars %>%
  tbl_summary(
    statistic = list(
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_categorical(percent) ~ 2),
    label = pvcDUM ~ "Retained",
    missing_text = "(Missing)"
  )

cat_summary


b <- early_legal %>% 
  group_by(brok_promise) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  mutate(total = sum(freq),
         percentage = freq/total * 100) 

b

# Descriptive of the continuous variables (Case open. Early Legal and Differences)

#1. Total Exposure 
summary(early_legal$sca_co_exposure)
summary(early_legal$sca_exposure)
summary(early_legal$del_exposure_norm)

#2. Total case amount due 
summary(early_legal$sca_co_tca_due)
summary(early_legal$sca_tca_due)
summary(early_legal$del_tot_amt_due_norm)

#3. Total past due amount
summary(early_legal$sca_co_tp_due)
summary(early_legal$sca_tp_due)
summary(early_legal$del_past_due_amt_norm)

#4. FICO Score
summary(early_legal$co_case_cbr_score)
summary(early_legal$case_cbr_score)
summary(early_legal$del_FICO_score_norm)

# Descriptive of the continuous engagement variables (Case open. Early Legal and Differences)
# Kept Promise
summary(early_legal$count_kept_promise)
summary(early_legal$count_kept_promise_before)
summary(early_legal$count_kept_promise_after)

summary(early_legal$nrm_kept_promise)
summary(early_legal$nrm_kept_promise_before)
summary(early_legal$nrm_kept_promise_after)

# Broken Promise
summary(early_legal$count_brok_promise)
summary(early_legal$count_brok_promise_before)
summary(early_legal$count_brok_promise_after)

summary(early_legal$nrm_brok_promise)
summary(early_legal$nrm_brok_promise_before)
summary(early_legal$nrm_brok_promise_after)

#SMS
summary(early_legal$count_sms)
summary(early_legal$count_sms_before)
summary(early_legal$count_sms_after)

summary(early_legal$nrm_sms)
summary(early_legal$nrm_sms_before)
summary(early_legal$nrm_sms_after)

#PHONE
summary(early_legal$count_phone_rpc)
summary(early_legal$nrm_phone_rpc)
summary(early_legal$count_phone_rpc_before)
summary(early_legal$count_phone_rpc_after)


summary(early_legal$count_phone_inbound)
summary(early_legal$count_phone_inbound_before)
summary(early_legal$count_phone_inbound_after)

summary(early_legal$count_phone_outbound)
summary(early_legal$count_phone_outbound_before)
summary(early_legal$count_phone_outbound_after)

summary(early_legal$count_phone_outbound_attempt)
summary(early_legal$count_phone_outbound_attempt_before)
summary(early_legal$count_phone_outbound_attempt_after)

summary(early_legal$count_chat)
summary(early_legal$count_chat_before)
summary(early_legal$count_chat_after)

summary(early_legal$count_email)
summary(early_legal$count_email_before)
summary(early_legal$count_email_after)

summary(early_legal$count_letter_sent)
summary(early_legal$count_letter_sent_before)
summary(early_legal$count_letter_sent_after)

summary(early_legal$count_email_letter_sent)
summary(early_legal$count_email_letter_sent_before)
summary(early_legal$count_email_letter_sent_after)



summary(early_legal$nrm_mob_login_count)
summary(early_legal$nrm_mob_login_count_before)
summary(early_legal$nrm_mob_login_count_after)

summary(early_legal$nrm_web_login_count)
summary(early_legal$nrm_web_login_count_before)
summary(early_legal$nrm_web_login_count_after)

summary(early_legal$nrm_both_login_count)
summary(early_legal$nrm_both_login_count_before)
summary(early_legal$nrm_both_login_count_after)


# Total Number of Days from case open to early legal Vs case open to early legal

combined_plot <- ggplot(early_legal, aes(x = total_age, fill = "Total Case Length (Days)")) +
  geom_density(alpha = 0.5) +
  geom_density(aes(x = age_co_to_earlylegal, fill = " Case Open to Early Legal(Days)"), alpha = 0.5) +
  scale_y_continuous(breaks=pretty_breaks(n=10))+
  scale_x_continuous(limits = c(20, 100), breaks=pretty_breaks(n=15))+
  labs(title = "Distribution of Total Case Days Vs Case Open to Early Legal Days",
       x = "Days",
       y = "Density",
       fill = "Variable")+
  theme_bw(base_size = 15)+
  theme(legend.position = "bottom", legend.justification = "center")

print(combined_plot)


# Distribution of FICO scores at Early Legal
early_legal_filtered <- early_legal %>%
  filter(!is.na(case_cbr_score)) %>% 
  mutate(pvcDUM = if_else(pvcDUM == 1, "Collection", "Retained"))
         
combined_plot <- ggplot(early_legal_filtered, aes(x = case_cbr_score, fill = factor(pvcDUM))) +
  geom_density(alpha = 0.5) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = pretty_breaks(n = 15)) +
  labs(
    title = "Distribution of FICO Score (Early Legal)",
    x = "FICO Score",
    y = "Density",
    fill = ""
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom", legend.justification = "left") +
#  facet_wrap(~factor(pvcDUM), ncol = 1) +
  scale_fill_manual(values = c("Retained" = "steelblue", "Collection" = "orange"))  # Specify colors for 0 and 1

print(combined_plot)

# Distribution of FICO scores at Case Open
combined_plot1 <- ggplot(early_legal_filtered, aes(x = co_case_cbr_score, fill = factor(pvcDUM))) +
  geom_density(alpha = 0.5) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = pretty_breaks(n = 15)) +
  labs(
    title = "Distribution of FICO Score (Case Open)",
    x = "FICO Score",
    y = "Density",
    fill = ""
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom", legend.justification = "left") +
  #  facet_wrap(~factor(pvcDUM), ncol = 1) +
  scale_fill_manual(values = c("Retained" = "steelblue", "Collection" = "orange"))  # Specify colors for 0 and 1

print(combined_plot1)

# Distribution of FICO scores at Case Open and Early Legal by Retained or not
combined_plot2 <- ggplot(early_legal_filtered, aes(x = case_cbr_score)) +
  geom_density(aes(fill = "case_cbr_score"), alpha = 0.5) +
  geom_density(aes(x = co_case_cbr_score, fill = "co_case_cbr_score"), alpha = 0.5) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = pretty_breaks(n = 15)) +
  labs(
    title = "Distribution of FICO score at Case Open and Early Legal",
    x = "FICO Score",
    y = "Density",
    fill = "FICO Scores at:"
  ) +
  scale_fill_manual(
    values = c("case_cbr_score" = "steelblue", "co_case_cbr_score" = "red"),  
    labels = c("case_cbr_score" = "Early Legal", "co_case_cbr_score" = "Case Open"))+
  theme_bw(base_size = 13) +
  theme(legend.position = "bottom", legend.justification = "center") +
  facet_wrap(~factor(pvcDUM))

print(combined_plot2)

# Distribution of FICO scores For Consumers and Small Business By Collection and Retained
combined_plot3 <- ggplot(early_legal_filtered, aes(x = case_cbr_score)) +
  geom_density(aes(fill = "case_cbr_score"), alpha = 0.8) +
  geom_density(aes(x = co_case_cbr_score, fill = "co_case_cbr_score"), alpha = 0.6) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = pretty_breaks(n = 15)) +
  labs(
    title = "Distribution of FICO score at Case Open and Early Legal",
    x = "FICO Score",
    y = "Density",
    fill = "FICO Scores at:"
  ) +
  scale_fill_manual(
    values = c("case_cbr_score" = "steelblue", "co_case_cbr_score" = "orange"),  
    labels = c("case_cbr_score" = "Early Legal", "co_case_cbr_score" = "Case Open"))+
  theme_bw(base_size = 13) +
  theme(legend.position = "bottom", legend.justification = "center") +
  facet_wrap(~factor(cm_type))

print(combined_plot3)

# Distribution of total exposure For Consumers and Small Business By Collection and Retained
combined_plot4 <- ggplot(early_legal_filtered, aes(x = sca_exposure)) +
  geom_density(aes(fill = "sca_exposure"), alpha = 0.8) +
  geom_density(aes(x = sca_co_exposure, fill = "sca_co_exposure"), alpha = 0.6) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_continuous(limits = c(-20,100), breaks = pretty_breaks(n = 15)) +
  labs(
    title = "Distribution of Total Exposure at Case Open and Early Legal",
    x = "Total Exposure",
    y = "Density",
    fill = "Total Exposure at:"
  ) +
  scale_fill_manual(
    values = c("sca_exposure" = "steelblue", "sca_co_exposure" = "orange"),  
    labels = c("sca_exposure" = "Early Legal", "sca_co_exposure" = "Case Open"))+
  theme_bw(base_size = 13) +
  theme(legend.position = "bottom", legend.justification = "center") +
  facet_wrap(~factor(pvcDUM))

print(combined_plot4)

# Distribution of total exposure at Case Open
hist_plot <- ggplot(early_legal, aes(x = case_cbr_score, fill = factor(pvcDUM))) +
  geom_histogram(binwidth = 10, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  labs(
    title = "Empirical Distribution of FICO Score by pvcDUM",
    x = "FICO Score",
    y = "Frequency",
    fill = "pvcDUM"
  ) +
  theme_minimal()

print(hist_plot)

# Graphical representation of the promise 
exposure50 <- ggplot(early_legal, aes(x = exposure_bin, y = percent, color = pvcDUM)) +
  geom_line(size = 1) +
  facet_wrap(~ pvcDUM) +  # Facet by pvcDUM with independent y-axes
  scale_y_continuous(breaks = pretty_breaks(n = 15)) +
  scale_x_discrete(labels = scales::label_wrap(10)) +  
  xlab("Exposure Bins") +
  ylab("Percent of Total Sample") +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9))+
  ggtitle("Percent of Total Sample in Each Exposure Bin")

exposure50

##########################################
#Summary of the Engagements 
##########################################

# Specify the binary columns 
binary_columns <- c("kept_promise", "brok_promise", "phone_rpc", "phone_inbound", "phone_outbound", "phone_outbound_attempts", 
                    "chat", "sms", "email_sent", "letter_sent", "letter_received", "email_letter_sent", "mobile", "web", "both")

# Initialize an empty data frame to store variable names and percentages
plot_data <- data.frame(variable = character(0), percentage = numeric(0))

# Calculate the percentages for each binary variable
for (var in binary_columns) {
  percentage <- round(mean(early_legal[, var] == 1)*100, 1)
  plot_data <- rbind(plot_data, data.frame(variable = var, percentage = percentage))
}


ggplot(plot_data, aes(x = variable, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = scales::percent(percentage/100 ,accuracy = 0.01)), vjust = -1, size = 4, angle = 45, hjust = 0) +
  labs(x = "Engagements", y = "Percentage of Total Credit Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(hjust = 0.5, size = 12),
        plot.title = element_text(size = 20, hjust = 0.5)) +
  ggtitle("Summary of Engagement")


library(gridExtra)

early_legal <- x

early_legal1 <- early_legal %>% 
  mutate(kept_promise_before = if_else(period == "before EL" & kept_promise == 1, 1,0),
         kept_promise_after = if_else(period == "after EL" & kept_promise == 1, 1,0)
  ) %>% 
  select(period, kept_promise, kept_promise_before, kept_promise_after)

table(early_legal1$period)

##########################################################################
#Summary of the categorical variables: Engagements
##########################################################################

# Specify the binary columns 
binary_columns <- c("kept_promise", "brok_promise", "phone_rpc", "phone_inbound", "phone_outbound", "phone_outbound_attempts", 
                    "chat", "sms", "email_sent", "letter_sent", "letter_received", "email_letter_sent", "mobile", "web", "both")

# Initialize an empty data frame to store variable names and percentages
plot_data <- data.frame(variable = character(0), percentage = numeric(0)) %>% 
  arrange(percentage)

# Calculate the percentages for each binary variable
for (var in binary_columns) {
  percentage <- round(mean(early_legal[, var] == 1)*100, 1)
  plot_data <- rbind(plot_data, data.frame(variable = var, percentage = percentage))
}

plot_data <- plot_data %>% arrange(percentage)

ggplot(plot_data, aes(x = reorder(variable, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = scales::percent(percentage/100 ,accuracy = 0.01)), vjust = -1, size = 4, angle = 45, hjust = 0) +
  labs(x = "Engagements", y = "Percentage of Total Credit Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(hjust = 0.5, size = 12),
        plot.title = element_text(size = 20, hjust = 0.5)) +
  ggtitle("Summary of Engagement")

##########################################################################
#Summary of the continuous variables: Engagements
##########################################################################

# Selecting the continuous variable of interest 
continuous_vars <- c("count_kept_promise","count_brok_promise", "count_phone_rpc", "count_phone_inbound", 
                     "count_phone_outbound","count_phone_outbound_attempts", "count_chat", "count_email",
                     "count_email_received", "count_letter_sent", "count_letter_received", "count_email_letter_sent",
                     "mobile_login_count","web_login_count","both_login_count")  

# Calculate the means for each continuous variable
mean_values <- sapply(early_legal[continuous_vars], mean, na.rm = TRUE)

# Create a data frame for plotting
plot_data <- data.frame(variable = names(mean_values), mean_value = mean_values)

# Convert variable to factor and reorder by mean values
plot_data$variable <- factor(plot_data$variable, levels = plot_data$variable[order(plot_data$mean_value)])

# Create a horizontal bar plot with mean values displayed at the top
ggplot(plot_data, aes(x = mean_value, y = variable)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.2f", mean_value)), hjust = -0.2, size = 4) +
  labs(x = "Mean Value", y = "Engagements") +
  theme_minimal() +
  ggtitle("Mean Values of Different Engagement Categories")

##########################################################################
# Graphical Representation 
# Proportions of OA by exposure bins
# Exposure Bins Vs Total Exposure
#Exposure Bins Vs Total proportion of the sample 
##########################################################################

# Define the bins and create exposure bins
bins <- seq(0, 900, by = 10)

early_legal$exposure_bin <- cut(early_legal$sca_exposure, breaks = bins, include.lowest = TRUE)

# Group by exposure bin and pvcDUM, and calculate the mean of scaled exposure
el21 <- early_legal %>% 
  group_by(exposure_bin, pvcDUM) %>% 
  summarize(mean_exp = mean(sca_exposure))

# Melt the data for better plotting
el21_plot <- melt(el21, id.vars = c("exposure_bin", "pvcDUM"))

# Rename the "pvcDUM" values to "Sent to Collection" and "Retailed"
el21_plot$pvcDUM <- ifelse(el21_plot$pvcDUM == "Yes", "Sent to Collection", "Retained")

# Create the ggplot with facets
exposure <- ggplot(el21_plot, aes(x = exposure_bin, y = value, color = pvcDUM)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(~ pvcDUM) +  
  scale_y_continuous(breaks = pretty_breaks(n = 15)) +
  scale_x_discrete(labels = scales::label_wrap(10)) +  
  xlab("Exposure Bins") +
  ylab("Mean of scaled Exposure") +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9))

exposure

#Percentages of total samples by exposure bins
el50 <- early_legal %>% 
  group_by(exposure_bin, pvcDUM) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(total = sum(count),
         percent = count/total*100) %>% 
  select(-total)

# Rename the "pvcDUM" values to "Sent to Collection" and "Retailed"
el50$pvcDUM <- ifelse(el50$pvcDUM == "Yes", "Sent to Collection", "Retained")

# Create the ggplot with facets
exposure50 <- ggplot(el50, aes(x = exposure_bin, y = percent, color = pvcDUM)) +
  geom_line(size = 1) +
  facet_wrap(~ pvcDUM) +  # Facet by pvcDUM with independent y-axes
  scale_y_continuous(breaks = pretty_breaks(n = 15)) +
  scale_x_discrete(labels = scales::label_wrap(10)) +  
  xlab("Exposure Bins") +
  ylab("Percent of Total Sample") +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9))+
  ggtitle("Percent of Total Sample in Each Exposure Bin")

exposure50

##########################################################################
# Machine Learning
# Best polynomial fit 
# Using training and test set 
##########################################################################
early_legal55 <- early_legal %>% 
  filter(!is.na(del_FICO_score_norm) & del_FICO_score_norm != 0)

# Setting the seed to generate the unique data set
set.seed(6)

# Sampling the 70% of the Early legal data
train <- sample(nrow(early_legal) * 0.7)

# Split the data into a training set (70%)
train_set <- early_legal[train, ]

# Split the data into a test set
test_set <- early_legal[-train, ]

# Initialize vectors to store MSE values and polynomial degrees
MSE <- rep(NA, 10)
degrees <- 1:10

# Loop through different polynomial degrees
for (k in degrees) {
  # Fit a logistic regression model with a polynomial term using the training set
  formula <- as.formula(paste("pvcDUM ~ poly(age_co_to_earlylegal, ", k, ")"))
  model <- glm(formula, data = train_set, family = binomial)
  
  # Predict the probabilities for the test set
  predicted_probabilities <- predict(model, newdata = test_set, type = "response")

  # Calculate the squared differences between the observed and predicted values for the test set
  squared_errors <- (test_set$pvcDUM - predicted_probabilities) ^ 2
  
  # Calculate the MSE and store it
  MSE[k] <- mean(squared_errors)
}

# Plot the MSE vs Polynomial Order
plot(degrees, MSE, type = "b", xlab = "Polynomial order", ylab = "MSE", main = "Normalized Difference in FICO Score")
  

##########################################################################
#Prediction: Logistic Model
##########################################################################

model1 <- glm(pvcDUM ~ co_case_cbr_score + sca_co_exposure + sca_co_tca_due + 
                sca_co_tp_due + co_exposure_to_tca_due + age_co_to_earlylegal + age_el_to_caseclose + 
                phone_rpc + phone_inbound +  phone_outbound + chat + email_sent + letter_sent + 
                email_letter_sent + mobile + web + both + cm_type, data = early_legal, family = binomial(link = "logit"))

summary(model1)
margins(model1)

logit_margins <- summary(margins(model1))

stargazer(model1, logit_margins, type="text",
          dep.var.labels="Sent to Collection",
          covariate.labels=c("FICO Score", "Total Exposure", "Total Case Amount Due", "Total Past Due", "Exposure to TCA Due",
                             "Case Open to EL (Days)", "EL to Case Close (Days)", "Phone Contact", "Phone Inbound",  "Phone Outbound",
                             "Chat", "Email Sent", "letter Sent", "Email letter Sent", "Mobile Only", "Web Only",  "Both",  "Card Member Type"),
          out="msg_models.txt")


model2 <- glm(pvcDUM ~ co_case_cbr_score + sca_co_exposure + sca_co_tca_due + 
                sca_co_tp_due + co_exposure_to_tca_due + age_co_to_earlylegal + age_el_to_caseclose + 
                phone_rpc + phone_inbound +  phone_outbound + chat + email_sent + letter_sent + 
                email_letter_sent + mobile + web + both + cm_type, data = early_legal, family = binomial(link = "logit"))



