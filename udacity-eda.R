library(tidyverse)
library(magrittr)

# Load the initial police tibble. We need to do some tidying with this
police <- read.csv('spreadsheets/PoliceKillingsUS.csv')
str(police)
unique(police$manner_of_death)
unique(police$armed)
1- sum(police$gender == 'M') / nrow(police)
unique(police$race)
sample(police$city, 20)
unique(police$threat_level)
unique(police$flee)

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

# Tidy the data types
df$name %<>% as.character
df$year %<>% as.integer
df$month %<>% as.integer
df$day %<>% as.integer

# Metro statistical data here
income <- read.csv('spreadsheets/MedianHouseholdIncome2015.csv')
poverty <- read.csv('spreadsheets/PercentagePeopleBelowPovertyLevel.csv')
high_school <- read.csv('spreadsheets/PercentOver25CompletedHighSchool.csv')
race_stats <- read.csv('spreadsheets/ShareRaceByCity.csv')

str(income)
str(poverty)
str(high_school)
str(race_stats)

# Create a 'cities' data frame from 'df' which will hold all unique city names 
# and their count of police killings, arranged by state then city, which seemed
# like the simplest way to match the tables quickly, given the data on hand
cities <- df %>%
  group_by(city, state) %>%
  select(-name) %>%
  summarize(deaths = n()) %>%
  arrange(state, city)

income <- income %>% group_by(City) %>% arrange(Geographic.Area, City)
poverty <- poverty %>% group_by(City) %>% arrange(Geographic.Area, City)
high_school <- high_school %>% group_by(City) %>% arrange(Geographic.Area, City)
race_stats <- race_stats %>% 
  rename(Geographic.Area = Geographic.area) %>%
  group_by(City) %>% 
  arrange(Geographic.Area, City)
stats <- left_join(income, poverty)
stats <- left_join(stats, high_school)
stats <- left_join(stats, race_stats)

# Cleans up some column names and runs a regular expression through the city
# names to make match against 'df' containing the police killings data. 
# Looks like it's statistical classification was appended in each table?
stats <- stats %>% 
  rename(state = Geographic.Area, city = City, income = Median.Income,
         hs_completion = percent_completed_hs, W = share_white, B = share_black,
         N = share_native_american, A = share_asian, H = share_hispanic,
         poverty = poverty_rate) %>%
  mutate(city = sub('(.*)(\\s\\w+)$', '\\1', city))

cities <- left_join(cities, stats)
cities$income <- (as.numeric(levels(cities$income))[cities$income])
cities$poverty <- (as.numeric(levels(cities$poverty))[cities$poverty]) / 100
cities$hs_completion <- (as.numeric(levels(cities$hs_completion))[cities$hs_completion]) / 100
cities$W <- (as.numeric(levels(cities$W))[cities$W]) / 100
cities$B <- (as.numeric(levels(cities$B))[cities$B]) / 100
cities$N <- (as.numeric(levels(cities$N))[cities$N]) / 100
cities$A <- (as.numeric(levels(cities$A))[cities$A]) / 100
cities$H <- (as.numeric(levels(cities$H))[cities$H]) / 100

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

### Plots by time
ggplot(df) + 
  geom_bar(mapping = aes(x = month(month, TRUE), fill = race)) +
  facet_grid(year ~ .) +
  labs(title = 'Deaths by Month for Each Year, Total by Race',
       x = 'Month', y = 'Count of Deaths')
ggplot(df %>% filter(year != 2017)) + #2017 incomplete skews proportion
  geom_bar(mapping = aes(x = month(month, TRUE), 
                         y = stat(prop),
                         group = 1,
                         fill = race)) +
  facet_wrap(~ race, nrow = 4) +
  ggtitle('Proportion of Deaths by Month for Each Race') +
  labs(x = 'Month', y = 'Proportion')

### Use of just force by race
ggplot(df) + #IBID, buggy race data
  geom_bar(mapping = aes(x = race, fill = just_force)) +
  facet_wrap(~ year)


### Weapons involved
df %>% 
  select(age, armed) %>%
  group_by(age, armed) %>%
  mutate(age = cut(age, breaks = c(0, 18, 30, seq(40, 100, by = 10)))) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
View(df %>%
  select(armed, month) %>%
  group_by(armed) %>%
  summarize(count = n(), mode = month(max(month),label = TRUE)) %>%
  arrange(desc(count)))

### Age of victims
ggplot(df) +
  geom_boxplot(mapping = aes(x = race, y = age, fill = race)) +
  scale_fill_brewer(palette = 'Reds') +
  scale_y_continuous(breaks = seq(10, 100, by = 10))
summary(df$age)

# Change df ages into bins:
# 0-18, 18-25, 25-35, 35-45, 45-65, 65+
df <- df %>% mutate(age = cut(age, breaks = c(0, 18, 25, 35, 45, 65, 100)))
# Add column for PoC (B/H/N)
df <- df %>% mutate(is_poc = race %in% c('B', 'H', 'N'))

age_poc <- df %>%
  filter((is_poc == TRUE) | (race == 'W')) %>%
  select(id, race, age, just_force, is_poc) %>%
  drop_na()
ggplot(age_poc, mapping = aes(x = age, fill = race)) +
  geom_bar(stat = 'count', position = 'fill') +
  scale_fill_brewer(palette = 'Spectral') +
  facet_wrap(~ just_force) +
  labs(title = 'Use of Justified Force by Age and Race',
       y = 'Proportion of deaths', x = 'Age of Victim')






