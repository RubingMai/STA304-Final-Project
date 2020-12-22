#### Preamble ####
# Purpose: Prepare and clean the census data downloaded from [https://sda-artsci-utoronto-ca.myaccess.library.utoronto.ca/cgi-bin/sda/hsda?harcsda4+gss31]
# Author: Rubing Mai
# Data: December 21, 2020
# Contact: rubing.mai@mail.utoronto.ca
# License: MIT

#### Workspace setup ####

library(haven)
library(tidyverse)

raw_post_data <- read_csv("AAu01LGi.csv")

dict <- read_lines("gss_dict.txt", skip = 18)
labels_raw <- read_file("gss_labels.txt")

#### Set-up the dictionary ####

variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_post_data)[-1]))

labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)

cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))


#### Apply that dictionary to the raw data ####

reduced_post_data <- raw_post_data %>% 
  select(agedc, 
         sex, 
         prv, 
         famincg2,
         ehg3_01b,
         fi_110
         ) %>%
  mutate_at(vars(agedc:fi_110), .funs = funs(ifelse(.>=96, NA, .))) %>% 
  mutate_at(.vars = vars(sex:fi_110),
            .funs = funs(eval(parse(text = cw_statements %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull())))) 

reduced_post_data <- reduced_post_data %>% 
  rename(age = agedc,
         gender = sex,
         province = prv,
         income_family = famincg2,
         education = ehg3_01b
         ) 


#### Clean data ####

reduced_post_data$age <- as.integer(reduced_post_data$age)

reduced_post_data <- 
  reduced_post_data %>%
  filter(age >= 18)

reduced_post_data <- reduced_post_data %>% 
  select(age,
         gender,
         province) 


# Saving the census data as a csv file

write_csv(reduced_post_data, "/Users/zella/Desktop/STA304-Final-Project/census_data.csv")

