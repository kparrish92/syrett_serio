library(here)
library(tidyverse)
library(brms)
library(bayestestR)


exp3_tidy = read.csv(here("data", "tidy", "four_choice_data.csv")) %>% 
  rename("CategoryType" = "kind") %>% 
  rename("Selection" = "response") %>% 
  rename("Participant" = "prolific_id") %>% 
  filter(CategoryType == "natural kind" | 
           CategoryType == "abstract concept" | 
           CategoryType == "artifact")

b3 <- brm(Selection ~ CategoryType + (CategoryType | Participant), 
          data=exp3_tidy,
          family="categorical")

conditional_effects(b3,categorical = TRUE)

b3 %>% 
  write_rds(here("data", "models", "exp3_model.rds"))



