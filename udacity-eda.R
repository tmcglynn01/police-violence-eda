library(tidyverse)
library(magrittr)

# Load the initial police tibble. We need to do some tidying with this
police <- read.csv('spreadsheets/PoliceKillingsUS.csv')
str(police)

# Check some of the columns
1- sum(police$gender == 'M') / nrow(police)
unique(police$threat_level)
unique(police$manner_of_death)
unique(police$flee)
unique(police$race)
unique(police$armed)

# Noise: since females account for only 4.2% of the total observations, and the
# sake of this analysis, we'll be leaving that column out. Further, looking at
# the variables 'threat_level' and 'manner_of_death', neither seem to determine
# anything useful. Variable 'threat_level' contains factors (attack, other, 
# undetermined) that don't sound reliable, or meaningful: 2 of 3 are ambiguous.
# Variable 'manner_of_death' contains factors (shot, shot and tasered), for an
# analysis purely interested in fatalities it seems inconsequential to look at 
# what other injury occurred and just be looking at the nature of the fatality
# and some other statistical data related to the environment where it happened.

# That said, the scope of this analysis will look at the following from this table:
# * Factors during police encounter (race, weapons, mental illness)
# * City and state
# * Body camera usage
# * Dates (full annual data for 2015 and 2016, partial 2017)

# This pipe creates tibble 'df' from 'police' with column selections.
# Adds: year, month, day, just_force
# 'just_force': Let's suppose justified use of force would be when an officer 
# encounters someone who has equal power of force (another gun). We'll look at 
# those in a different context. The use of force is arguably more nuanced than 
# this, however it creates a good baseline to frame what is, and is not, 
# 'appropriate' use of deadly force by equally matching force available.
(df <- police %>%
  group_by(id) %>%
  filter(gender == 'M') %>%
  select(-gender, -manner_of_death, -threat_level) %>%
  rename(illness = signs_of_mental_illness, cam = body_camera) %>%
  mutate(date = dmy(as.character(date))) %>%
  mutate(year = year(date), month = month(date), day = day(date)) %>%
  select(-date) %>%
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

# Start importing city data, filter for top_cities
income <- read.csv('spreadsheets/MedianHouseholdIncome2015.csv')
str(income)
names(income)

re <- '(.*)(\\s\\w+)$'
income <- income %>%
  rename(state = 'Geographic.Area', 
         city = 'City', 
         median_income = 'Median.Income') %>%
  group_by(city) %>%
  # Seems like there is a classification for each city after their name?
  mutate(city = sub(re, '\\1', city))%>%
  filter(city %in% top_cities$city & state %in% top_cities$state)

# Join tables
top_cities <- left_join(top_cities, income)

poverty <- read.csv('spreadsheets/PercentagePeopleBelowPovertyLevel.csv')
str(poverty)
poverty <- poverty %>%
  rename(state = 'Geographic.Area', city = 'City') %>%
  group_by(city) %>%
  mutate(city = sub(re, '\\1', city)) %>%
  filter(city %in% top_cities$city & state %in% top_cities$state)

(top_cities <- left_join(top_cities, poverty))

high_school <- read.csv('spreadsheets/PercentOver25CompletedHighSchool.csv')
str(high_school)
high_school <- high_school %>%
  rename(state = 'Geographic.Area', 
         city = 'City', 
         hs_completion = 'percent_completed_hs') %>%
  group_by(city) %>%
  mutate(city = sub(re, '\\1', city)) %>%
  filter(city %in% top_cities$city & state %in% top_cities$state)

(top_cities <- left_join(top_cities, high_school))

race_stats <- read.csv('spreadsheets/ShareRaceByCity.csv')
str(race_stats)
race_stats <- race_stats %>%
  rename(state = 'Geographic.area', city = 'City') %>%
  group_by(city) %>%
  mutate(city = sub(re, '\\1', city)) %>%
  filter(city %in% top_cities$city & state %in% top_cities$state) %>%
  rename(w = 'share_white', b = 'share_black', n = 'share_native_american',
         a = 'share_asian', h = 'share_hispanic') %>%
  #mutate_at(vars(w, b, n, a, h), integer)
  print

top_cities <- left_join(top_cities, race_stats)

# Just some memory cleanup
rm(income, poverty, high_school, race_stats, re)

# Clean tibble df
df


df$name %<>% as.character
df$year %<>% as.integer
df$month %<>% as.integer
df$day %<>% as.integer
df

top_cities
top_cities$median_income <- as.numeric(levels(top_cities$median_income))[top_cities$median_income]
top_cities$poverty_rate <- as.numeric(levels(top_cities$poverty_rate))[top_cities$poverty_rate]
top_cities$hs_completion <- as.numeric(levels(top_cities$hs_completion))[top_cities$hs_completion]
top_cities$w <- as.numeric(levels(top_cities$w))[top_cities$w]
top_cities$b <- as.numeric(levels(top_cities$b))[top_cities$b]
top_cities$n <- as.numeric(levels(top_cities$n))[top_cities$n]
top_cities$a <- as.numeric(levels(top_cities$a))[top_cities$a]
top_cities$h <- as.numeric(levels(top_cities$h))[top_cities$h]
top_cities

top_cities <- top_cities %>% 
  mutate(w = w/100, b = b/100, n = n/100, a = a/100, h = h/100,
         poverty_rate = poverty_rate/100, hs_completion = hs_completion/100)
top_cities
df

# Number of shoots correlated with each of the city data variables - scatterplots
# Scatter plots use two continuous variables
# Continuous y, categorical x is good for boxplots
# Discrete data are counts of a presence
# With discrete variables, you can calculate and assess a rate of occurrence or 
# a summary of the count, such as the mean, sum, and standard deviation
# Proportion of police killings side-by-side portion of each city's race
# Binary variables are good for proportions
# Histograms (A, B) for cities racial breakdowns by year?
# Whites vs. PoC
ggplot(data = top_cities, 
       mapping = aes(x = poverty_rate, y = city_count, fill = city)) +
  geom_point()
