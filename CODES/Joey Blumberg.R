# Setting up the environment

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# import using a url
titanic <- read.csv("https://web.stanford.edu/class/archive/cs/cs109/cs109.1166/stuff/titanic.csv")

summary(titanic)

#Looking at the bad observations
titanic %>% filter(Fare == 0)

# take the titanic data
titanic_clean <- titanic %>% # and then...
  # remove "bad" data
  filter(Fare != 0) # filter(Fare > 0) would also work

titanic_clean <- titanic %>% # and then...
  # remove "bad" data
  filter(Fare != 0) %>% # and then...
  # create new variables
  mutate( 
    age_sq = Age^2, 
    alone = ifelse(
      Siblings.Spouses.Aboard > 0 | Parents.Children.Aboard > 0, 0, 1
    ),
    alone2 = ifelse(
      Siblings.Spouses.Aboard == 0 & Parents.Children.Aboard == 0, 1, 0
    ),
    age_group = case_when(
      Age <  1             ~ "Infant",
      Age >= 1  & Age < 4  ~ "Toddler",
      Age >= 4  & Age < 13 ~ "Child",
      Age >= 13 & Age < 20 ~ "Teen",
      Age >= 20 & Age < 40 ~ "Adult",
      Age >= 40 & Age < 60 ~ "Middle Age Adult",
      Age >= 60            ~ "Senior Adult"
    ),
    age_group = factor(
      age_group, levels = c("Infant", "Toddler", "Child", "Teen",
                            "Adult", "Middle Age Adult", "Senior Adult")
    ),
    income_class = case_when(
      Pclass == 1 ~ "Upper Class",
      Pclass == 2 ~ "Middle Class",
      Pclass == 3 ~ "Lower Class"
    ),
    Sex = ifelse(
      Sex == "female", "Female", "Male"
    )
  )

sum(titanic_clean$alone == titanic_clean$alone2) == nrow(titanic_clean)


# take the titanic data
titanic_clean <- titanic %>% # and then...
  # remove "bad" data
  filter(Fare != 0) %>% # and then...
  # create new variables
  mutate( 
    age_sq = Age^2, 
    alone = ifelse(
      Siblings.Spouses.Aboard > 0 | Parents.Children.Aboard > 0, 0, 1
    ),
    alone2 = ifelse(
      Siblings.Spouses.Aboard == 0 & Parents.Children.Aboard == 0, 1, 0
    ),
    age_group = case_when(
      Age <  1             ~ "Infant",
      Age >= 1  & Age < 4  ~ "Toddler",
      Age >= 4  & Age < 13 ~ "Child",
      Age >= 13 & Age < 20 ~ "Teen",
      Age >= 20 & Age < 40 ~ "Adult",
      Age >= 40 & Age < 60 ~ "Middle Age Adult",
      Age >= 60            ~ "Senior Adult"
    ),
    age_group = factor(
      age_group, levels = c("Infant", "Toddler", "Child", "Teen",
                            "Adult", "Middle Age Adult", "Senior Adult")
    ),
    income_class = case_when(
      Pclass == 1 ~ "Upper Class",
      Pclass == 2 ~ "Middle Class",
      Pclass == 3 ~ "Lower Class"
    ),
    Sex = ifelse(
      Sex == "female", "Female", "Male"
    )
  ) %>%  # and then...
  # only keep variables of interest
  select(Survived, Sex, Age, alone, age_group, income_class) %>% # and then...
  # rename variables in a consistent manner (snake_case)
  rename(survived = Survived,
         sex = Sex,
         age = Age) %>% # and then...
  # order the data by age
  arrange(age)

head(titanic_clean)


# Grouping and Summarizing Data

titanic_clean %>% 
  # group by income class
  group_by(income_class) %>%
  # calculate survival rate by group
  summarize(survival_rate = mean(survived))

#Survival rates by age group:
  
  titanic_clean %>% 
  group_by(age_group) %>% 
  summarize(survival_rate = mean(survived))

  #Survival rates by solo travelers:
    
    titanic_clean %>% 
    group_by(alone) %>% 
    summarize(survival_rate = mean(survived))
    
    
    titanic_grouped <- titanic_clean %>% 
      group_by(age_group, sex, income_class) %>% 
      summarize(count = n(),
                survival_rate = mean(survived)) %>% 
      ungroup() %>% 
      arrange(-survival_rate, age_group)
    
    head(titanic_grouped, 20)
    
    ggplot(data = titanic_clean,
           # specify aesthetics
           mapping = aes(x = survived)) +
      # specify layer
      geom_bar()
    
    titanic_clean <- titanic_clean %>% 
      mutate(survived_char = ifelse(survived == 1, "Survived", "Perished"),
             survived_char = factor(survived_char,
                                    levels = c("Survived", "Perished")))
  
# Now, change the aesthetics accordingly, change the bar width, and add labels.
    
    ggplot(data = titanic_clean,
           mapping = aes(x = survived_char)) +
      geom_bar(width = 0.5) + # use width to shrink bars
      # specify labels
      labs(x = NULL, # using NULL deletes the a-axis label
           y = "Count",
           title = "Overall Survival Counts of Titanic Passengers")
    
    
    ggplot(data = titanic_clean,
           mapping = aes(x = survived_char, 
                         fill = sex)) + 
      geom_bar(width = 0.5) +
      labs(x = NULL,
           y = "Count",
           title = "Survival Counts of Titanic Passengers by Sex",
           fill = NULL)
    
 #Intermediate Plotting
    
    ggplot(titanic_clean,
           aes(x = survived_char, fill = sex)) +
      geom_bar(width = 0.5) +
      # add text to the plot that shows the size of each bar
      geom_text(aes(label = after_stat(count)), 
                stat = "count", 
                color = "white",
                position = position_stack(vjust = 0.5)) +
      labs(x = NULL,
           y = "Count",
           title = "Survival Counts of Titanic Passengers by Sex",
           fill = NULL) +
      # specify axis breaks
      scale_y_continuous(breaks = seq(0,500,100)) +
      # use classic theme
      theme_classic()
    
    
    ggplot(titanic_clean,
           aes(x = survived_char, fill = sex)) +
      geom_bar(width = 0.5) +
      labs(x = NULL,
           y = "Count",
           title = "Survival Counts of Titanic Passengers by Sex and Socioeconomic Status",
           fill = NULL) +
      scale_y_continuous(breaks = seq(0,400,50)) +
      # split graph by socioeconomic status
      facet_wrap(~income_class) +
      # use the black and white theme
      theme_bw() +
      # eliminate most grid lines
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())
    
    
    
    ggplot(titanic_clean,
           aes(x = age, fill = survived_char)) +
      geom_histogram() +
      # specify colors with hex codes
      scale_fill_manual(values = c("#E69F00", "#999999")) + 
      scale_x_continuous(breaks = seq(0,90,10)) +
      labs(y = "Count",
           x = "Age",
           title = "Survival Counts of Titanic Passengers by Age", 
           fill = NULL) +
      theme_bw() +
      theme(panel.border = element_blank(),
            panel.grid = element_blank(),
            # adjust the legend position
            legend.position = c(0.8,0.8))
    
    
    ggplot(titanic_clean,
           aes(x = age, fill = survived_char, linetype = survived_char)) +
      # alpha changes how transparent a color is
      geom_density(alpha = 0.5, linewidth = 0.6) +
      scale_fill_manual(values = c("#E69F00", "#999999")) + 
      scale_x_continuous(breaks = seq(0,90,10)) +
      labs(y = "Density",
           x = "Age",
           title = "Age Distributions of Titanic Passengers", 
           fill = NULL,
           linetype = NULL) +
      theme_bw() +
      theme(panel.border = element_blank(),
            panel.grid = element_blank(),
            legend.position = c(0.8,0.8))
    
    
# What is this last plot showing?
      
      ggplot(titanic_clean) +
      geom_jitter(aes(x = age, y = sex, shape = survived_char)) +
      facet_wrap(~income_class) +
      scale_shape_manual(values = c(20,4)) +
      labs(y = "Sex",
           x = "Age",
           color = NULL,
           shape = NULL) +
      theme_bw() +
      theme(panel.spacing = unit(1, "lines"),
            panel.grid = element_blank(),
            strip.background = element_blank())
      
      
# Saving Plots      
      p <- ggplot(titanic_clean,
                  aes(x = age, fill = survived_char, linetype = survived_char)) +
        # alpha changes how transparent a color is
        geom_density(alpha = 0.5, linewidth = 0.6) +
        scale_fill_manual(values = c("#E69F00", "#999999")) + 
        scale_x_continuous(breaks = seq(0,90,10)) +
        labs(y = "Density",
             x = "Age",
             title = "Age Distributions of Titanic Passengers", 
             fill = NULL,
             linetype = NULL) +
        theme_bw() +
        theme(panel.border = element_blank(),
              panel.grid = element_blank(),
              legend.position = c(0.8,0.8))
      
      ggsave(plot = p,
             filename = "/Users/keshavbhusal/Desktop/AREC/titanic_density.png",
             height = 6,
             width = 9,
             dpi = 300)
