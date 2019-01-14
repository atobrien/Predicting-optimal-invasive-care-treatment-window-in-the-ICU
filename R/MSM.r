# This is the R script used to create the Multi-State Transition Model

# Load library
library(tidyr)
library(dplyr)
library(dummies)
library("tdisplay", lib.loc="~/R/win-library/3.5")
library("survival", lib.loc="~/R/win-library/3.5")
library("survminer", lib.loc="~/R/win-library/3.5")

# Read data
df<-read.csv("C:/Users/Me/Desktop/data-1547335815982.csv")

# Replace NA in death_flag with 0
df<-df %>% 
  mutate(death_flag = replace_na(death_flag, 0))

# Select icustays with more than 1 day stay, only first time admission,
# >=18years and with a sofa score
df<-df %>% 
  group_by(icustay_id) %>% 
  filter(length(icustay_id)>1 & 
           first_icu_stay %in% "True" & 
           admission_age>=18 &
           !is.na(sofa_last))

# Import the cirrhosis dataframe. This contains the patients diagnosed with
# cirrhosis. It will be used as a key to select only cirrhosis patients
key<-read.csv("C:/Users/Me/Desktop/data-1546976302496.csv")

# Prepare the key 
key$charttime<-NULL
key$day_start<-NULL
key$cirrhosis_flag<-NULL

# Using semi-join with the key we filter only the cirrhosis patients in the df 
df<-semi_join(df, key)

# Drop key for memory
rm(key)

# Turn variables into correct types
df<-df %>% 
  mutate(death_flag= as.numeric(death_flag), states= as.numeric(states))

# Create a lead and a lag column for states
# this is to prepare the dataframe for the Multi State Model
# I create a lead and lag state to create the tstart and tstop columns
# I also converted NA in these columns to 99 for the ifelse statesments to work

df<-df %>% 
  mutate(lag_states= lag(states),
              lead_states= lead(states), 
              lag_states = replace_na(lag_states, 99),
              lead_states = replace_na(lead_states, 99),
              tstart= ifelse(states != lag_states, icudayseq_asc, NA),
              tstop= ifelse(states != lead_states, icudayseq_asc, NA))

# Now I have to collapse the tstart, tstop for the MSM so I begin by making the
# dataframe with the row id, icustay_id, tstart and tstop
df1<- df %>% 
  select(icustay_id, tstart, tstop, states)
id<- 1:nrow(df1)
df1<-cbind(id=id, df1)

# As is noted there are many NA's between the points so I have to begin to 
# remove these. To do this Ill filter out a dataframe with the NAs then use an
# antijoin to select only the rows with the data I want for the MSM
key2<-df1 %>% 
  filter(is.na(tstart) & is.na(tstop))
df1<-anti_join(df1, key2)
rm(key2)

# This is close, but there are still many NAs now I need to collapse these rows 
# without losing the data's sanity. To do this I create a lead column then 
# leverage ifelse statements to fill in the NA's in the stop column and then
# use the NAs in the start column to remove those unneeded rows.
df1<- df1 %>% 
  mutate(leadtstop= lead(tstop),
         tstop= ifelse(is.na(tstop),leadtstop,tstop)) %>% 
  filter(!is.na(tstart))

# Then I remove the leadcolumn for being orderly
df1$leadtstop<- NULL
