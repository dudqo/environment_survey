#### Preamble ####
# Purpose: Prepare the 2021 US GSS data
# Author: Young Suk
# Data: 20 April 2022
# Contact: young.suk@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the GSS data and saved it to inputs/data


#### Workspace setup ####
# Use R Projects, not setwd().
library(haven)
library(tidyverse)
# Read in the raw data. 
raw_data <- haven::read_dta("inputs/data/2021_stata/gss2021.dta")

# Just keep some variables that may be of interest (change 
# this depending on your interests)
names(raw_data)

reduced_data <- 
  raw_data %>% 
  select(age,
         sexnow1, 
         degree,
         income16,
         raceacs1,
         raceacs2,
         raceacs3,
         raceacs4,
         raceacs5,
         raceacs6,
         raceacs7,
         raceacs8,
         raceacs9,
         raceacs10,
         raceacs15,
         raceacs16,
         natrace,
         natfare,
         natfarey,
         natsoc,
         natcrime,
         immlimit,
         relig,
         religinf,
         opsex,
         
  ) %>% 
  rename(gender = sexnow1,
         Family_income = income16,
         nation_race = natrace,
         nation_welfare = natfare,
         Assistance_to_the_poor = natfarey,
         social_security = natsoc,
         Immigration = immlimit,
         )

# rm(raw_data)

# Add age_group column based on age column
reduced_data <- reduced_data %>% 
  mutate(age_group = reduced_data$age %>% cut(breaks = seq(19, 89, by = 10))) %>%
  mutate(age_group = case_when(
    age_group == "(19,29]" ~ "20-29 years old",
    age_group == "(29,39]" ~ "30-39 years old",
    age_group == "(39,49]" ~ "40-49 years old",
    age_group == "(49,59]" ~ "50-59 years old",
    age_group == "(59,69]" ~ "60-69 years old",
    age_group == "(69,79]" ~ "70-79 years old",
    age_group == "(79,89]" ~ "80-89 years old"
  ))

# # Recode to rename variables in imigration according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was America should limit immigration in order to protect our national way of life
reduced_data <- 
  reduced_data %>% 
  mutate(Immigration = case_when(
    Immigration == 1 ~ "Strongly agree",
    Immigration == 2 ~ "Agree",
    Immigration == 3 ~ "Neither",
    Immigration == 4 ~ "Disagree",
    Immigration == 5 ~ "Strongly disagree"
  ))
# Recode to rename variables in gender according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was Do you describe yourself as male, female, or transgender?
reduced_data <- 
  reduced_data %>% 
  mutate(gender = case_when(
    gender == 1 ~ "Male",
    gender == 2 ~ "Female",
    gender == 3 ~ "Transgender",
    gender == 4 ~ "Other",
  ))


# Recode to rename variables in degree according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was the respondent's degree.
reduced_data <- 
  reduced_data %>% 
  mutate(degree = case_when(
    degree == 0 ~ "Less than high school",
    degree == 1 ~ "High School",
    degree == 2 ~ "Associate College",
    degree == 3 ~ "Bachelors",
    degree == 4 ~ "Graduate"
  ))

# Recode to rename variables in Family_income according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was in which of these groups did your total family income from all souces, fall last year?
# Before taxes.For simplicity, I divided groups into 
# 1) under $10,000
# 2) between $10,000 ~ $19,999
# 3) between $20,000 ~ $39,999
# 4) between $40,000 ~ $59,999
# 5) between $60,000 ~ $89,999
# 6) between $90,000 ~ $129,999
# 7) between $130,000 ~ $169,999
# 8) $170,000 or over
# 9) Refused 
reduced_data <- 
  reduced_data %>% 
  mutate(Family_income = case_when(
    Family_income == 1 ~ "Under $10,000",
    Family_income == 2 ~ "Under $10,000",
    Family_income == 3 ~ "Under $10,000",
    Family_income == 4 ~ "Under $10,000",
    Family_income == 5 ~ "Under $10,000",
    Family_income == 6 ~ "Under $10,000",
    Family_income == 7 ~ "Under $10,000",
    Family_income == 8 ~ "Under $10,000",
    Family_income == 9 ~ "$10,000 ~ $19,999",
    Family_income == 10 ~ "$10,000 ~ $19,999",
    Family_income == 11 ~ "$10,000 ~ $19,999",
    Family_income == 12 ~ "$10,000 ~ $19,999",
    Family_income == 13 ~ "$20,000 ~ $39,999",
    Family_income == 14 ~ "$20,000 ~ $39,999",
    Family_income == 15 ~ "$20,000 ~ $39,999",
    Family_income == 16 ~ "$20,000 ~ $39,999",
    Family_income == 17 ~ "$20,000 ~ $39,999",
    Family_income == 18 ~ "$40,000 ~ $59,999",
    Family_income == 19 ~ "$40,000 ~ $59,999",
    Family_income == 20 ~ "$60,000 ~ $89,999",
    Family_income == 21 ~ "$60,000 ~ $89,999",
    Family_income == 22 ~ "$90,000 ~ $129,999",
    Family_income == 23 ~ "$90,000 ~ $129,999",
    Family_income == 24 ~ "$130,000 ~ $169,999",
    Family_income == 25 ~ "$130,000 ~ $169,999",
    Family_income == 26 ~ "$170,000 or over",
    Family_income == 27 ~ "Refused"
  ))


# Recode to rename variables in nation_race according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was whether US is spending too much, too little, or about the right amount on 
# improving the conditions of Blacks
# Similar to nation education, this has 50.7% of respondents who answered "Not Applicable".
# Unfortunately this falls into "NA". 
reduced_data <- 
  reduced_data %>% 
  mutate(nation_race = case_when(
    nation_race == 1 ~ "Too little",
    nation_race == 2 ~ "About right",
    nation_race == 3 ~ "Too much",
  ))

# change the name of values for religion variable as specified in th GSS codebook.
reduced_data <- 
  reduced_data %>% 
  mutate(relig = case_when(
    relig == 1 ~ "Christian",
    relig == 2 ~ "Catholic",
    relig == 3 ~ "Jewish",
    relig == 4 ~ "None",
    relig == 5 ~ "Other",
    relig == 6 ~ "Buddhism",
    relig == 7 ~ "Hinduism",
    relig == 8 ~ "Other",
    relig == 9 ~ "Muslim/Islam",
    relig == 10 ~ "Other",
    relig == 11 ~ "Christian",
    relig == 12 ~ "Native American",
    relig == 13 ~ "Other",
  ))

# First step of merging process of survey response related to race. Create a column race_sum which each row represents number of races of each person.
# If value at column race_sum is greater than 1, that person is of mixed race.
reduced_data$race_sum <- reduced_data %>%
  select(raceacs1, raceacs2, raceacs3, raceacs4, raceacs5, raceacs6, raceacs7, raceacs8, raceacs9, raceacs10, raceacs15, raceacs16) %>%
  rowSums(.)

# Create a column named "race" and assign values according to the columns from the survey.
reduced_data <- reduced_data %>%
  mutate(race = case_when(
    raceacs1 == 1 ~ "White",
    raceacs2 == 1 ~ "Black",
    raceacs3 == 1 ~ "American Native",
    raceacs4 == 1 ~ "Asian",
    raceacs5 == 1 ~ "Asian",
    raceacs6 == 1 ~ "Asian",
    raceacs7 == 1 ~ "Asian",
    raceacs8 == 1 ~ "Asian",
    raceacs9 == 1 ~ "Asian",
    raceacs10 == 1 ~ "Asian",
    raceacs15 == 1 ~ "Other",
    raceacs16 == 1 ~ "Hispanic"
  ))

# If a person is of mixed race, that person is assigned value "other"
reduced_data <- reduced_data %>%
  mutate(race = if_else(race_sum == 1, reduced_data$race, "Other"))

# Remove columns that are no longer needed.
reduced_data <- reduced_data %>%
  select(-c(raceacs1, raceacs2, raceacs3, raceacs4, raceacs5, raceacs6, raceacs7, raceacs8, raceacs9, raceacs10, raceacs15, raceacs16, race_sum))



# Recode to rename variables in nation_welfare according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was whether US is spending too much, too little, or about the right amount on 
# Welfare
# Unfortunetly 50.7% of respondents answered "Not Applicable" which falls into "NA".
reduced_data <- 
  reduced_data %>% 
  mutate(nation_welfare = case_when(
    nation_welfare == 1 ~ "Too little",
    nation_welfare == 2 ~ "About right",
    nation_welfare == 3 ~ "Too much",
  ))

# Recode to rename variables in Assistance_to_the_poor according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was whether US is spending too much, too little, or about the right amount on 
# assistance to the poor
# Unfortunetly 49.3% of respondents answered "Not Applicable" which falls into "NA".

reduced_data <- 
  reduced_data %>% 
  mutate(Assistance_to_the_poor = case_when(
    Assistance_to_the_poor == 1 ~ "Too little",
    Assistance_to_the_poor == 2 ~ "About right",
    Assistance_to_the_poor == 3 ~ "Too much",
  ))


# Recode to rename variables in social_security according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was whether US is spending too much, too little, or about the right amount on 
# social security
reduced_data <- 
  reduced_data %>% 
  mutate(social_security = case_when(
    social_security == 1 ~ "Too little",
    social_security == 2 ~ "About right",
    social_security == 3 ~ "Too much",
  ))

# Rename values of variable natcrime using codebook 'GSS 2021 Codebook R1b.pdf'.
# is the government spending enough to halt rising crime rate?
reduced_data <- 
  reduced_data %>% 
  mutate(natcrime = case_when(
    natcrime == 1 ~ "Too little",
    natcrime == 2 ~ "About right",
    natcrime == 3 ~ "Too much",
  ))

# Rename values of variable natcrime using codebook 'GSS 2021 Codebook R1b.pdf'.
# The U.S would be better country if religion had less influence
reduced_data <- 
  reduced_data %>% 
  mutate(religinf = case_when(
    religinf == 1 ~ "Strongly agree",
    religinf == 2 ~ "Agree",
    religinf == 3 ~ "Neither",
    religinf == 4 ~ "Disagree",
    religinf == 5 ~ "Strongly disagree"
  ))

# Rename values of variable natcrime using codebook 'GSS 2021 Codebook R1b.pdf'.
# how important is being born as a man or woman?
reduced_data <- 
  reduced_data %>% 
  mutate(opsex = case_when(
    opsex == 1 ~ "Essential",
    opsex == 2 ~ "Very important",
    opsex == 3 ~ "Fairly important",
    opsex == 4 ~ "Not very important",
    opsex == 5 ~ "Not important at all"
  ))


#### Save ####
write_csv(reduced_data, "outputs/data/prepared_gss.csv")

