nonmember<-read.csv(here("data","baseline_nonmember.csv"))
PivotData <- pivot_longer(nonmember, Water:Shovel, names_to="Item", values_to="Rating")
FinalData <- PivotData %>% mutate(CategoryType = case_when(
  Item == "Water" ~ "Natural",
  Item == "Tiger" ~ "Natural",
  Item == "Gold" ~ "Natural",
  Item == "Raccoon" ~ "Natural",
  Item == "Helium" ~ "Natural",
  Item == "Zebra" ~ "Natural",
  Item == "Fork" ~ "Artifact",
  Item == "Kettle" ~ "Artifact",
  Item == "Chair" ~ "Artifact",
  Item == "Razor" ~ "Artifact",
  Item == "Harpoon" ~ "Artifact",
  Item == "Shovel" ~ "Artifact",
  Item == "Heavy.Metal" ~ "Abstract",
  Item == "Scientist" ~ "Abstract",
  Item == "Mentor" ~ "Abstract",
  Item == "Argument" ~ "Abstract",
  Item == "Artist" ~ "Abstract",
  Item == "Minister" ~ "Abstract",
  Item == "Spicy" ~ "Vague",
  Item == "Tall" ~ "Vague",
  Item == "Square" ~ "Definite",
  Item == "Gallon" ~ "Definite"))


FinalData$Item <- as.factor(FinalData$Item)
FinalData$CategoryType <- as.factor(FinalData$CategoryType)
# Are vague and defininte control contitions? 


FinalData %>% 
  group_by(Rating, CategoryType) %>% 
  summarize(n = n()) %>% mutate(pct = n/282) %>% 
  filter(CategoryType == "Abstract") %>% 
  mutate(exp2_pred = case_when(
    Rating == 1 | Rating == 2 ~ "is",
    Rating == 3 | Rating == 4 | Rating == 5 ~ "both",
    Rating == 6 | Rating == 7 ~ "is_not",
  )) %>% 
  group_by(exp2_pred) %>% 
  summarise(exp_prob = sum(pct))

d = FinalData %>% ### find sd 
  group_by(Rating, CategoryType, Participant) %>% 
  summarize(n = n()) %>% mutate(pct = n/6) %>% 
  filter(CategoryType == "Abstract") %>% 
  mutate(exp2_pred = case_when(
    Rating == 1 | Rating == 2 ~ "is",
    Rating == 3 | Rating == 4 | Rating == 5 ~ "both",
    Rating == 6 | Rating == 7 ~ "is_not",
  )) %>% 
  group_by(exp2_pred, Participant) %>% 
  summarise(exp_prob = sum(pct)) %>% 
  group_by(exp2_pred) %>% 
  summarise(mean_prob = mean(exp_prob),
            sd_prob = sd(exp_prob))



FinalData %>% 
  group_by(Rating, CategoryType) %>% 
  summarize(n = n()) %>% mutate(pct = n/282) %>% 
  filter(CategoryType == "Natural") %>% 
  mutate(exp2_pred = case_when(
    Rating == 1 | Rating == 2 ~ "is",
    Rating == 3 | Rating == 4 | Rating == 5 ~ "both",
    Rating == 6 | Rating == 7 ~ "is_not",
  )) %>% 
  group_by(exp2_pred) %>% 
  summarise(exp_prob = sum(pct))


FinalData %>% 
  group_by(Rating, CategoryType) %>% 
  summarize(n = n()) %>% mutate(pct = n/282) %>% 
  filter(CategoryType == "Artifact") %>% 
  mutate(exp2_pred = case_when(
    Rating == 1 | Rating == 2 ~ "is",
    Rating == 3 | Rating == 4 | Rating == 5 ~ "both",
    Rating == 6 | Rating == 7 ~ "is_not",
  )) %>% 
  group_by(exp2_pred) %>% 
  summarise(exp_prob = sum(pct))

# is 1-2
# both 3,4,5
# is not 6-7


member<-read.csv(here("data", "baseline_member_r.csv"))

Data <- pivot_longer(member, Water:Shovel, names_to="Item", values_to="Rating")
MutatedData <- Data %>% mutate(CategoryType = case_when(
  Item == "Water" ~ "Natural",
  Item == "Tiger" ~ "Natural",
  Item == "Gold" ~ "Natural",
  Item == "Raccoon" ~ "Natural",
  Item == "Helium" ~ "Natural",
  Item == "Zebra" ~ "Natural",
  Item == "Fork" ~ "Artifact",
  Item == "Kettle" ~ "Artifact",
  Item == "Chair" ~ "Artifact",
  Item == "Razor" ~ "Artifact",
  Item == "Harpoon" ~ "Artifact",
  Item == "Shovel" ~ "Artifact",
  Item == "Heavy.Metal" ~ "Abstract",
  Item == "Scientist" ~ "Abstract",
  Item == "Mentor" ~ "Abstract",
  Item == "Argument" ~ "Abstract",
  Item == "Artist" ~ "Abstract",
  Item == "Minister" ~ "Abstract",
  Item == "Spicy" ~ "Vague",
  Item == "Tall" ~ "Vague",
  Item == "Square" ~ "Definite",
  Item == "Gallon" ~ "Definite"))

MutatedData$Item <- as.factor(MutatedData$Item)
MutatedData$CategoryType <- as.factor(MutatedData$CategoryType)


MutatedData %>% 
  group_by(CategoryType) %>%
  summarise(n = n())



MutatedData %>% 
  group_by(Rating, CategoryType) %>% 
  summarize(n = n()) %>% mutate(pct = n/300) %>% 
  filter(CategoryType == "Abstract") %>% 
  mutate(exp2_pred = case_when(
    Rating == 1 | Rating == 2 ~ "is_not",
    Rating == 3 | Rating == 4 | Rating == 5 ~ "both",
    Rating == 6 | Rating == 7 ~ "is",
  )) %>% 
  group_by(exp2_pred) %>% 
  summarise(exp_prob = sum(pct))


MutatedData %>% 
  group_by(Rating, CategoryType) %>% 
  summarize(n = n()) %>% mutate(pct = n/300) %>% 
  filter(CategoryType == "Natural") %>% 
  mutate(exp2_pred = case_when(
    Rating == 1 | Rating == 2 ~ "is_not",
    Rating == 3 | Rating == 4 | Rating == 5 ~ "both",
    Rating == 6 | Rating == 7 ~ "is",
  )) %>% 
  group_by(exp2_pred) %>% 
  summarise(exp_prob = sum(pct))


MutatedData %>% 
  group_by(Rating, CategoryType) %>% 
  summarize(n = n()) %>% mutate(pct = n/300) %>% 
  filter(CategoryType == "Artifact") %>% 
  mutate(exp2_pred = case_when(
    Rating == 1 | Rating == 2 ~ "is_not",
    Rating == 3 | Rating == 4 | Rating == 5 ~ "both",
    Rating == 6 | Rating == 7 ~ "is",
  )) %>% 
  group_by(exp2_pred) %>% 
  summarise(exp_prob = sum(pct))
