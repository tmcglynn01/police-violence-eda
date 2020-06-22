library(tidyverse)
police <- read.csv('spreadsheets/PoliceKillingsUS.csv')
str(police)

#Proportion M/F
sum(police$gender == 'M') / nrow(police)

# What exactly is 'threat_level'?
unique(police$threat_level)
# Officer can claim: attack, other, undetermined)
# What exactly is manner_of_death?
unique(police$manner_of_death)
# Neither of these are pertinent

(df <- police %>%
  group_by(id) %>%
  filter(gender == 'M') %>%
  select(-gender, -manner_of_death, -threat_level) %>%
  rename(illness = signs_of_mental_illness, cam = body_camera) %>%
  mutate(date = dmy(as.character(date))) %>%
  mutate(year = year(date), month = month(date), day = day(date)) %>%
  select(-date) %>%
  # Let's suppose justified use of force would be when an officer encounters
  # someone who has equal power of force (another gun). We'll look at those in a
  # different context
  mutate(just_force = armed == 'gun') %>%
  arrange(year, month, day))

# Base Plot
bplot <- ggplot(df)

# Racial breakdown by month 
bplot + geom_bar(mapping = aes(x = month, fill = race))
# Looks like there may be a clear quarterly trend?
bplot + 
  geom_bar(mapping = aes(x = quarter(month), fill = race)) +
  facet_wrap(~ year)
# Only for 2016
# 2017 data is incomplete

# Interested in some breakdowns by race for later
poc <- df %>% filter(race %in% c('B', 'H', 'N'))
white <- df %>% filter(race == 'W')

# Let's start looking at city data
length(unique(df$city))
top_cities <- df %>%
  group_by(city, state) %>%
  select(-name) %>%
  summarize(city_count = n()) %>%
  arrange(desc(city_count)) %>%
  filter(city_count >= 14)

(city_names <- top_cities[,1])

# Start importing city data, filter for top_cities
income <- read.csv('spreadsheets/MedianHouseholdIncome2015.csv')
str(income)
names(income)

re <- '(.*)(\\s\\w+)$'

income %>%
  rename(area = 'Geographic.Area', 
         city = 'City', 
         median_income = 'Median.Income') %>%
  group_by(city) %>%
  # Seems like there is a classification for each city after their name?
  mutate(city = sub(re, '\\1', city))%>%
  #FIXME: join tables
  print

poverty <- read.csv('spreadsheets/PercentagePeopleBelowPovertyLevel.csv')
str(poverty)

high_school <- read.csv('spreadsheets/PercentOver25CompletedHighSchool.csv')
str(high_school)

race <- read.csv('spreadsheets/ShareRaceByCity.csv')
str(race)
