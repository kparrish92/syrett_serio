library(here)
library(tidyverse)
library(brms)
library(bayestestR)



nonmember_tidy = read.csv(here("data", "tidy", "nonmember_tidy.csv")) %>% 
  mutate(membership = "nonmember")
member_tidy = read.csv(here("data", "tidy", "member_tidy.csv")) %>% 
  mutate(membership = "member")


## Set priors 



## Tidy data 
ord_data = rbind(member_tidy, nonmember_tidy) 
## Run member model
ord_mod_member <- brm(as.integer(Rating) ~ CategoryType + 
                 (CategoryType | Participant) + (1 | Item),
               data = ord_data %>% filter(membership == "member"),
               family = cumulative(),
               cores = 4)
## save output
ord_mod_member %>% write_rds(here("data", "models","ord_member.rds"))

## Run nonmember model
ord_mod_nonmember <- brm(as.integer(Rating) ~ CategoryType + 
                        (CategoryType | Participant) + (1 | Item),
                      data = ord_data %>% filter(membership == "nonmember"),
                      family = cumulative(),
                      cores = 4)

## save output
ord_mod_nonmember %>% write_rds(here("data", "models","ord_nonmember.rds"))

summary(ord_mod_member)

fixef(ord_mod_member)

# Baseline - Abstract 
plogis(fixef(ord_mod_member)[1])
plogis(fixef(ord_mod_member)[2]) - plogis(fixef(ord_mod_member)[1])
plogis(fixef(ord_mod_member)[3]) - plogis(fixef(ord_mod_member)[2])  
plogis(fixef(ord_mod_member)[4]) - plogis(fixef(ord_mod_member)[3])  
plogis(fixef(ord_mod_member)[5]) - plogis(fixef(ord_mod_member)[4])  
plogis(fixef(ord_mod_member)[6]) - plogis(fixef(ord_mod_member)[5])  
1- plogis(fixef(ord_mod_member)[6])  


# Artifact 
### Yeah this is complex - basically you need to caculate the cut points and then do subtraction.
### The first two terms are the adjustment to the "intercept" cut point. Then you subtract the prevous cut point to get the predicted probability 
plogis(fixef(ord_mod_member)[1] - fixef(ord_mod_member)[7]) # 1 for artifact 
plogis(fixef(ord_mod_member)[2] - fixef(ord_mod_member)[7]) - plogis(fixef(ord_mod_member)[1] - fixef(ord_mod_member)[7]) 
plogis(fixef(ord_mod_member)[3] - fixef(ord_mod_member)[7]) - plogis(fixef(ord_mod_member)[2] - fixef(ord_mod_member)[7])
plogis(fixef(ord_mod_member)[4] - fixef(ord_mod_member)[7]) - plogis(fixef(ord_mod_member)[3] - fixef(ord_mod_member)[7])
plogis(fixef(ord_mod_member)[5] - fixef(ord_mod_member)[7]) - plogis(fixef(ord_mod_member)[4] - fixef(ord_mod_member)[7])
plogis(fixef(ord_mod_member)[6] - fixef(ord_mod_member)[7]) - plogis(fixef(ord_mod_member)[5] - fixef(ord_mod_member)[7]) 
1 - plogis(fixef(ord_mod_member)[6] - fixef(ord_mod_member)[7])
### For the natrual kinds predictor, change 7 to 8 


a2 = plogis(fixef(ord_mod_member)[1] - fixef(ord_mod_member)[7] - fixef(ord_mod_member)[2]) # 2 for artifact 
a2 = plogis(fixef(ord_mod_member)[2] - fixef(ord_mod_member)[7] - fixef(ord_mod_member)[3]) 

#  Natural 
plogis(fixef(ord_mod_member)[1] - fixef(ord_mod_member)[8])
 
conditional_effects(ord_mod_member)

conditional_effects(ord_mod_member, categorical = TRUE)


conditional_effects(ord_mod_nonmember)
conditional_effects(ord_mod_nonmember, categorical = TRUE)


plogis(-4)


get_prior(ord_mod_member)

df = conditional_effects(ord_mod, categorical = TRUE)