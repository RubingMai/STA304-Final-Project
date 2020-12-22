#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [https://doi.org/10.7910/DVN/DUS88V]
# Author: Rubing Mai
# Data: December 21, 2020
# Contact: rubing.mai@mail.utoronto.ca
# License: MIT



#### Workspace setup ####

library(haven)
library(tidyverse)

# Read in the raw data

raw_survey_data <- read_dta("2019 Canadian Election Study - Online Survey v1.0.dta")

# Add the labels

raw_survey_data <- labelled::to_factor(raw_survey_data)

reduced_survey_data <- 
  raw_survey_data %>% 
  select(cps19_gender,
         cps19_age,
         cps19_province,
         cps19_votechoice,
         cps19_votechoice_pr,
         pes19_votechoice2019,
         )

#### Clean data ####

reduced_survey_data$cps19_votechoice <- as.character(reduced_survey_data$cps19_votechoice)
reduced_survey_data$cps19_votechoice_pr <- as.character(reduced_survey_data$cps19_votechoice_pr)
reduced_survey_data$pes19_votechoice2019 <- as.character(reduced_survey_data$pes19_votechoice2019)

reduced_survey_data$cps19_votechoice_pr[is.na(reduced_survey_data$cps19_votechoice_pr)] <- "NA"
reduced_survey_data$pes19_votechoice2019[is.na(reduced_survey_data$pes19_votechoice2019)] <- "NA"

reduced_survey_data<-
  reduced_survey_data %>%
  mutate(vote_choice = ifelse(pes19_votechoice2019 == "NA", cps19_votechoice, pes19_votechoice2019))

reduced_survey_data$vote_choice[is.na(reduced_survey_data$vote_choice)] <- "NA"

reduced_survey_data<-
  reduced_survey_data %>%
  mutate(vote_choice = ifelse(vote_choice == "NA", cps19_votechoice_pr, vote_choice))

reduced_survey_data = reduced_survey_data[!reduced_survey_data$vote_choice == "NA",]

reduced_survey_data<-
  reduced_survey_data %>%
  mutate(vote_Justin_Trudeau = 
           ifelse(vote_choice=="Liberal Party", 1, 0)) %>%
  mutate(vote_Andrew_Scheer = 
           ifelse(vote_choice=="Conservative Party", 1, 0))


reduced_survey_data <- reduced_survey_data %>% 
  rename(age = cps19_age,
         gender = cps19_gender,
         province = cps19_province) 


# Prepare for post stratification

reduced_survey_data <- reduced_survey_data %>%
  filter(gender!="Other (e.g. Trans, non-binary, two-spirit, gender-queer)") 

reduced_survey_data <- reduced_survey_data %>%
  mutate(gender = ifelse(gender == "A woman","Female","Male")) %>%
  filter(province!="Northwest Territories") %>%
  filter(province!="Nunavut") %>%
  filter(province!="Yukon")

reduced_survey_data <- 
  reduced_survey_data %>%
  filter(age <= 80)

# Saving the survey/sample data as a csv file

write_csv(reduced_survey_data, "/Users/zella/Desktop/STA304-Final-Project/survey_data.csv")

