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

#number of observations
surveys %>% 
  count(sex)

surveys %>% 
  count(sex, species)

surveys %>% 
  count(sex, species) %>% 
  arrange(species, desc(n))

surveys_new <- surveys %>% 
  count(sex, species) %>% 
  arrange(species, desc(n))

surveys_new <- surveys %>% 
  count(sex, species) %>% 
  arrange(species, desc(n)) %>% 
  View()

#challenge

surveys %>% 
  count(plot_type)

surveys %>%
  filter(!is.na(hindfoot_length)) %>% 
  group_by(species_id) %>% 
  summarize(mean_hindfoot_length = mean(hindfoot_length),
            min_hindfoot_length = min(hindfoot_length),
            max_hindfoot_length = max(hindfoot_length),
            n = n())

surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(year) %>% 
  filter(weight == max(weight)) %>%
  select(year, genus, species_id, weight) %>% 
  arrange(year)
#to get rid of duplicates in the data  
surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(year) %>% 
  filter(weight == max(weight)) %>%
  select(year, genus, species_id, weight) %>% 
  arrange(year) %>% 
  unique()
  
#to better compare genus and weight for different genra (long to wide table format)
surveys_gw <- surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(plot_id, genus) %>% 
  summarize(mean_weight = mean(weight))

str(surveys_gw)

surveys_wide <- surveys_gw %>%
  pivot_wider(names_from = genus, values_from = mean_weight, values_fill = 0)
  
#going from wide to long format (separate columns)
#surveys_wide <- surveys_gw
surveys_wide %>% 
    pivot_longer(names_to = "genus", values_to = "mean_weight", cols = -plot_id)

#challenge
surveys_long <- surveys %>% 
  pivot_longer(names_to = "measurement", values_to = "values", cols = c(hindfoot_length, weight)) %>% 
  View()

surveys_long %>% 
  group_by(year, measurement, plot_type) %>%
  summarize(mean_value = mean(value, na.rm = TRUE)) %>% 
  pivot_wider(names_from = measurement, values_from = mean_value)




surveys_complete <- surveys %>% 
  filter(!is.na(weight), !is.na(hindfoot_length), !is.na(sex))

#to create a new csv file
write_csv(surveys_complete, file = "surveys_complete.csv")











  































  
  
