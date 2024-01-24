library(tidyverse)
#load data
surveys <- read_csv("data_raw/portal_data_joined.csv")

str(surveys)
select(surveys, plot_id, species_id, weight)
#to remove unwanted columns
select(surveys, -record_id, -species_id)

filter(surveys, year == 1995)
filter(surveys, year == 1995, sex == "M")

surveys2 <- filter(surveys, weight < 5)
surveys_sml <- select(surveys2, species_id, sex, weight)

select(filter(surveys, weight < 5), species_id, sex, weight)
surveys_sml2 <- select(filter(surveys, weight < 5), species_id, sex, weight)

# %>% (pipes) nesting to avoid using multiple functions
surveys %>% 
  filter(weight < 5) %>%
  select(species_id, sex, weight)
#challenge to subset to animals before 1995, then retain columns year, sex, weight
surveys %>% 
  filter(year < 1995) %>%
  select(year, sex, weight)

#adding columns
surveys %>% 
  mutate(weight_kg = weight/1000) %>%
  View()

surveys %>% 
  mutate(weight_kg = weight/1000, weight_lb = weight_kg * 2.2) %>%
  View()

surveys %>% 
  mutate(weight_kg = weight/1000, weight_lb = weight_kg * 2.2) %>%
  head()

surveys %>% 
  filter(!is.na(weight)) %>% 
  View()

surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight/1000, weight_lb = weight_kg * 2.2) %>%
  head()

surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight/1000, weight_lb = weight_kg * 2.2) %>%
  select(-weight)

#to compare average weights of males to females (split-apply-combine)
surveys %>% 
  group_by(sex) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE))
#removing NA rows
surveys %>% 
  filter(!is.na(sex)) %>% 
  group_by(sex) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE))

surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(sex, species_id) %>% 
  summarize(mean_weight = mean(weight, na.rm = T)) %>% 
  tail()

surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(sex, species_id) %>% 
  summarize(mean_weight = mean(weight, na.rm = T)) %>% 
  print(n=15)

surveys %>% 
  filter(!is.na(weight), !is.na(sex)) %>% 
  group_by(sex, species_id) %>% 
  summarize(mean_weight = mean(weight, na.rm = T)) %>% 
  print(n=15)

#arrange by min weight
surveys %>% 
  filter(!is.na(weight), !is.na(sex)) %>% 
  group_by(sex, species_id) %>% 
  summarize(mean_weight = mean(weight, na.rm = T), min_weight = min(weight)) %>% 
  arrange(min_weight)

#arrange by descending order
surveys %>% 
  filter(!is.na(weight), !is.na(sex)) %>% 
  group_by(sex, species_id) %>% 
  summarize(mean_weight = mean(weight, na.rm = T), min_weight = min(weight)) %>% 
  arrange(desc(min_weight))

#arrange by ascending order
surveys %>% 
  filter(!is.na(weight), !is.na(sex)) %>% 
  group_by(sex, species_id) %>% 
  summarize(mean_weight = mean(weight, na.rm = T), min_weight = min(weight)) %>% 
  arrange((min_weight))







































  
  
