#####################################################################
#LOOKING THE OTHER WAY FOR PAYERS VS NON PAYERS 
#####################################################################  
PAYERS_UNIQUE <- early_legal %>% 
  filter(act_type_cd == "PY" & act_dt >= message_date) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr) 

PAYERS <- merge(early_legal, PAYERS_UNIQUE, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr"))

# Creating the dataset for Unique Non-Payers
NONPAYERS_UNIQUE <- early_legal %>%
  filter(portfo_w_lvl_cd == "051") %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr)

# Creating the dataset with all Non-Payers observations
NONPAYERS <- merge(early_legal, NONPAYERS_UNIQUE, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr"))

#####################################################################
#LOOKING FOR THE NUMBER OF DAYS AFTER EL NOTICE PAYMENT WAS MADE 
#####################################################################

PAYERS <- PAYERS %>%
  mutate(msg_to_pmyt_days = ifelse(act_type_cd == "PY" & act_dt >= message_date, 
                                    as.numeric(difftime(act_dt, message_date, units = "days")), NA))



# Summary of notice to payment dates among Payers 
PAYERS_SUMMARY <- PAYERS_PY %>%
  group_by(msg_to_pmyt_days1) %>% 
  summarize(freq = n_distinct(pseudo_key, case_grp_cd, case_seq_nbr)) %>% 
  ungroup() %>% 
  mutate(Totalfreq = sum(freq),
         Percent = (freq/Totalfreq * 100),
         Cumulative = cumsum(Percent)) %>% 
select(msg_to_pmyt_days1, freq, Totalfreq, Percent, Cumulative)

p <- ggplot(payers_PY, aes(x = msg_to_pmyt_days1)) +
  geom_histogram(fill = "purple", color = "black", na.rm = T) +
  labs(x = "EL Notice to Payment Days", y = "Frequency", title = "Distribution of Days from EL Notice to Payment") 
#  scale_x_continuous(breaks = custom_break1, labels = custom_break1)+
#  scale_y_continuous(breaks = seq(0, 300, by= 20), labels = scales::number_format(scale = 1))
p

#############################################################################################
#WORKING TO CREATE A DUMMY FOR PAYER VS NON PAYERS AND JOINING WITH THE ORIGINAL DATASET
##############################################################################################

PAYERS1 <- PAYERS %>% 
  mutate(payers_code =1) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T) %>% 
  select(1:3, payers_code) 

NONPAYERS1 <- NONPAYERS %>% 
  mutate(nonpayers_code =1) %>% 
  distinct(pseudo_key, case_grp_cd, case_seq_nbr, .keep_all = T) %>% 
  select(1:3, nonpayers_code) 

COM1 <- left_join(combined_all, PAYERS1, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) 
COM2_ALL <- left_join(COM1, NONPAYERS1, by = c("pseudo_key", "case_grp_cd", "case_seq_nbr")) 

# Creating the Dummy Variable for the 3 MYCA logins Variables: Mobile, Website and Both
COM2_ALL <- COM2_ALL %>% 
  mutate(pay_non_payDUM = case_when(payers_code == 1 ~ 1, 
                            nonpayers_code == 1 ~ 0, 
                            TRUE ~ 0))
check <- COM2_ALL%>% 
  group_by(pay_non_payDUM) %>% 
  summarize(freq = n())
check

