library(here)
library(tidyverse)
library(brms)
library(bayestestR)


exp3_tidy = read.csv(here("data", "tidy", "four_choice_data.csv")) %>% 
  rename("CategoryType" = "kind") %>% 
  rename("Selection" = "response") %>% 
  rename("Participant" = "prolific_id") 

b3 <- brm(Selection ~ CategoryType + (CategoryType | Participant), 
          data=exp3_tidy,
          family="categorical")

