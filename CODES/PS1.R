#PS2
#DATE:1st Sept, 2023
#Author: Keshav Bhusal
#Affilitation: Department of Agriculture and Resource Economics(AREC)

# PACKAGES From CRAN
library(rio)
library(ggplot2)           # graphing
library(scales)            # graphing aids 
library(dplyr)             # data manipulation
library(tidyr)             # data manipulation
library(purrr)             # data manipulation
library(haven)
library(lubridate)         # better date formatting 
library(stringr)           # extracting information from character variable
library(RSQLite)           # Database stuff
library(XML)
library(RCurl)
library(reshape2)
library(readr)
library(units)
library(writexl)

# LOADING DATA FILES
arec_project222 <- read_sas("Raw Data/arec_project222.sas7bdat", 
                            NULL)
arec_project223 <- read_sas("Raw Data/arec_project223.sas7bdat", 
                            NULL)
newraw <- read_sas("Raw Data/newraw.sas7bdat", 
                   NULL)

#CREATING A NEW DATA FORMAT (RData)
export (arec_project222, "/Users/keshavbhusal/Library/CloudStorage/Box-Box/Project AMEX/myca_web.RData")
export (arec_project223, "/Users/keshavbhusal/Library/CloudStorage/Box-Box/Project AMEX/myca_mob.RData")
export (newraw, "/Users/keshavbhusal/Library/CloudStorage/Box-Box/Project AMEX/raw.RData")

newraw <- x
myca_mob <- x
myca_web <- x

#Summarize the data
summary(myca_mob)
summary(myca_web)
summary(newraw)

#Number of unique pseudo_keys
length(unique(myca_mob$pseudo_key))
length(unique(myca_web$pseudo_key))
length(unique(newraw$pseudo_key))

#Categorizing the card members (BUSINESS/CONSUMER/BOTH)
business_members <- newraw %>%
  filter(case_grp_cd == "COMP") %>%
  distinct(pseudo_key) %>% 
  summarize(Frequency = n())
business_members

consumer_members <- newraw %>%
  filter(case_grp_cd == "CUST") %>%
  distinct(pseudo_key) %>% 
  summarize(Frequency = n())
consumer_members

both_members <- newraw %>%
  filter(case_grp_cd == "CUST" & case_grp_cd == "COMP") %>%
  summarize(Frequency = n())
both_members

total = business_members + consumer_members + both_members

business_members
consumer_members
both_members
total

#Checking for any missing values in rawdata(Pseudo_key & Case_grp_cd)
any_missing <- any(is.na(newraw$pseudo_key))
any_missing
any_missing <- any(is.na(newraw$case_grp_cd))
any_missing

#Summarizing the number of CMs based on Product Type
unique(newraw$product_type)

#Without the unique pseudo_key
product_cat <- newraw %>% 
  group_by(product_type) %>% 
  summarize(Frequency = n())
product_cat

#Looking at the distribution of unique product type
charge_type <- newraw %>%
  filter(product_type == "Charge") %>% 
  summarize(Frequency = n_distinct(pseudo_key))
charge_type

lend_type <- newraw %>%
  filter(product_type == "Lend") %>% 
  summarize(Frequency = n_distinct(pseudo_key))
lend_type

linked_type <- newraw %>%
   filter(product_type == "Linked") %>% 
   summarize(Frequency = n_distinct(pseudo_key))
linked_type

missing_type <- newraw %>%
  filter(product_type == "n/a") %>% 
  summarize(Frequency = n_distinct(pseudo_key))
missing_type

total <- charge_type + lend_type + linked_type + missing_type

charge_type
lend_type
linked_type
missing_type
total

#Check-Check

# Group the data by pseudo_key and product_type, then count the unique pseudo_keys in each group
result <- newraw %>%
  group_by(pseudo_key, product_type) %>%
  summarise(n = n_distinct(pseudo_key)) %>%
  ungroup()

# Create a new column to identify the mixed group
result <- result %>%
  mutate(product_type = ifelse(n > 1, "Mixed", product_type))

# Calculate the total count of unique pseudo_keys for each product_type
product_type_counts <- result %>%
  group_by(product_type) %>%
  summarise(total_count = sum(n_distinct(pseudo_key)))

# Calculate the overall total count of unique pseudo_keys
overall_total_count <- sum(product_type_counts$total_count)

# Print the results
cat("Total unique pseudo_keys:", overall_total_count, "\n")
product_type_counts




