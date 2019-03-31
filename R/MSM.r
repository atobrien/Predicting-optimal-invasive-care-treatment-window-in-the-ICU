# Load library
library("tidyr")
library("dplyr")
library("markovchain")
library("tdisplay", lib.loc="~/R/win-library/3.5")
library("survival", lib.loc="~/R/win-library/3.5")
library("survminer", lib.loc="~/R/win-library/3.5")
library("parallel")
library("stringr")
library("data.table")
library("runner")
library("DataCombine")

################################################################################
### SELECT CIRRHOSIS COHORT, ASSIGN SOFA QUARTILES #############################
################################################################################


# Read data
df<-read.csv("C:/Users/Me/Desktop/mimic_timecohort_20190304.csv")

# Replace NA in flags with 0
df<-df %>% 
  mutate(death_flag = replace_na(death_flag, 0),
         cv_flag = replace_na(cv_flag, 0),
         crrt_flag = replace_na(crrt_flag, 0),
         vasopres_flag = replace_na(vasopres_flag, 0),
         mechvent_flag = replace_na(mechvent_flag, 0),
         cmo_flag = replace_na(cmo_flag, 0))

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

# Find the quartiles of SOFA for first day of admission (day 0)
sofa<-df %>% filter(icudayseq_asc == 0)
quantile(sofa$sofa_last)
q.sofa<-floor(quantile(sofa$sofa_last))

# Results(floor):
# 0%  25%  50%  75% 100% 
# 0    4    7   10.5   22 
# Q1: 0-4
# Q2: 5-7
# Q3: 8-10
# Q4: 11-22

# Assign cohort groups to sofa data frame

sofa<-sofa %>% mutate(sofa_group= case_when(sofa_last %in% c(q.sofa[[1]]:q.sofa[[2]])~"Q1",
                                            sofa_last %in% c(q.sofa[[2]]+1:q.sofa[[3]])~"Q2",
                                            sofa_last %in% c(q.sofa[[3]]+1:q.sofa[[4]])~"Q3",
                                            sofa_last %in% c(q.sofa[[4]]+1:q.sofa[[5]])~"Q4"))
                                            
sofa<- sofa %>% select(icustay_id, sofa_group)

# Using semi-join with the sofa dataframe we add the sofa group to  df 
df<-merge(df, sofa, by="icustay_id")

# Remove sofa from memory
rm(sofa)
rm(q.sofa)

#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#

################################################################################
### REDEFINE CMO ###############################################################
################################################################################

# CMO will be redefined as follows: 
# state--lead_state_1_day_out--2_day_out--3_day_out--4_day_out--5_day_out--CMO
#   1               0              4                                        1
#   1               0              0           4                            1 
#   1               0              0           0         4                  1 
#   1               0              0           0         0          4       1
# This is making the assumption that patients that transition from 
# highly invasive care to non-invasive care and then died were on CMO 

df<-df %>% 
  mutate(onedayout=lead(states, n=1L),
         twodayout=lead(states, n=2L),
         threedayout=lead(states, n=3L),
         fourdayout=lead(states, n=4L),
         fivedayout=lead(states, n=5L),
         sixdayout=lead(states, n=6L),
         sevendayout=lead(states, n=7L),
         eightdayout=lead(states, n=8L),
         ninedayout=lead(states, n=9L),
         tendayout=lead(states, n=10L))

df<-df %>% 
  mutate(CMO_additional_flag=
           case_when((states==1)&(onedayout==0)&(twodayout==4)~1,
                     (states==1)&(onedayout==0)&(twodayout==0)&(threedayout==4)~1,
                     (states==1)&(onedayout==0)&(twodayout==0)&(threedayout==0)&(fourdayout==4)~1,
                     (states==1)&(onedayout==0)&(twodayout==0)&(threedayout==0)&(fourdayout==0)&(fivedayout==4)~1,
                     (states==1)&(onedayout==0)&(twodayout==0)&(threedayout==0)&(fourdayout==0)&(fivedayout==0)&(sixdayout==4)~1,
                     (states==1)&(onedayout==0)&(twodayout==0)&(threedayout==0)&(fourdayout==0)&(fivedayout==0)&(sixdayout==0)&(sevendayout==4)~1,
                     (states==1)&(onedayout==0)&(twodayout==0)&(threedayout==0)&(fourdayout==0)&(fivedayout==0)&(sixdayout==0)&(sevendayout==0)&(eightdayout==4)~1,
                     (states==1)&(onedayout==0)&(twodayout==0)&(threedayout==0)&(fourdayout==0)&(fivedayout==0)&(sixdayout==0)&(sevendayout==0)&(eightdayout==0)&(ninedayout==4)~1,
                     (states==1)&(onedayout==0)&(twodayout==0)&(threedayout==0)&(fourdayout==0)&(fivedayout==0)&(sixdayout==0)&(sevendayout==0)&(eightdayout==0)&(ninedayout==0)&(tendayout==4)~1))

# Drop additional columns created to be organized
df$onedayout<-NULL
df$twodayout<-NULL
df$threedayout<-NULL
df$fourdayout<-NULL
df$fivedayout<-NULL
df$sixdayout<-NULL
df$sevendayout<-NULL
df$eightdayout<-NULL
df$ninedayout<-NULL
df$tendayout<-NULL

# Add in the end state when it is dead (4) 
df<-df %>% 
  mutate(A= ifelse(states==4, 4,CMO_additional_flag))

# Filling in CMO cases according to above assumption on post-HIC days
A <- c(df$A)
index.1<-which(df$A %in% c(1)) # define location for 1s in A
index.14<-which(df$A %in% c(1,4)) # define location for 1s and 4s in A
loc.1<-which(index.14 %in% index.1) # location of 1s in  index.14
loc.4<-loc.1+1 # location of 4s relative to 1s in index.14
start.i<-((index.14[loc.1])+1) # starting index for replacing with 2
end.i<-((index.14[loc.4])-1) # ending index for replacing with 2 in index
fill.v<-sort(c(start.i, end.i))# sequence of indexes to fill-in with # 2
fill.m<-matrix(fill.v,nrow = (length(fill.v)/2),ncol = 2, byrow=TRUE) # create matrix of 
list.1<-apply(fill.m, MARGIN=1,FUN=function(x) seq(x[1],x[2])) # create a list with indexes to replace
list.2<-unlist(list.1) # unlist list to use as the indexes for replacement
df$A[list.2] <- 2 # replace indexed location with 2


rm(index.1)
rm(index.14)
rm(loc.1)
rm(loc.4)
rm(start.i)
rm(end.i)
rm(fill.v)
rm(fill.m)
rm(list.2)
rm(list.1)
rm(A)

# Update states & cmo flag with new CMO definition
df<-df %>%mutate(states= ifelse(A %in% 2, 2, states),
                 cmo_flag=ifelse(A %in% 2, 1, cmo_flag))
df$A<-NULL
df$CMO_additional_flag<-NULL

df[2038,10] <-1 # this is a mismatch in logic  *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#

################################################################################
# UNIQUE HIC - NO RE-ENTRIES INTO HIC #########################################
################################################################################

index.1<-which(df$states %in% c(1)) # index all the rows with 1
df<- df %>% mutate(col1=streak_run(states, k=1000)) # create a streak
df.collect<-df %>% .[c(index.1),] # collect all the rows with 1`
key<-df.collect %>% group_by(icustay_id) %>% count(col1) %>% filter(col1%in%1 & n!=1) # keep rentries into HIC  
key<-key[,1] # save ids for re-entries into HIC
df<-anti_join(df, key)# keep only those which do not re-enter HIC
rm(df.collect)
rm(key)

#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#

################################################################################
# Halved analysis                                                              #
################################################################################
# Divide cohort based on groups and demographics ###############################
################################################################################

# Obtain keys for sofa groups
sofa<-df %>% filter(icudayseq_asc == 0)

# Create df keys for each quantile sofa severity group
key.low<-sofa %>% filter(sofa_group %in% c("Q1","Q2")) %>% select(icustay_id)
key.high<-sofa %>% filter(sofa_group %in% c("Q3","Q4")) %>% select(icustay_id)

# Create df for each quantile sofa severity group
df.low<-semi_join(df, key.low)
df.high<-semi_join(df, key.high)

# Drop keys and sofa table
rm(key.low)
rm(key.high)
rm(sofa)

# Compute demographics for each quantile sofa group

#Q1#########################
table(df.low$death_flag)
length(unique(df.low$icustay_id))
#Q2#########################
table(df.high$death_flag)
length(unique(df.high$icustay_id))

# How many days on average did they spend in each unit

#Q1#########################
table(df.low$states)[1:3]
sum(table(df.low$states)[1:3]) # total number of days in ICU
sum(table(df.low$states)) # total number of days passed
#Q2#########################
table(df.high$states)[1:3]
sum(table(df.high$states)[1:3])# total number of days in ICU
sum(table(df.high$states)) # total number of days passed

# Obtain the last day metrics for each quantile sofa group
df.low.lastrow<-df.low %>% group_by(icustay_id) %>% filter(icudayseq_asc %in% max(icudayseq_asc))
df.high.lastrow<-df.high %>% group_by(icustay_id) %>% filter(icudayseq_asc %in% max(icudayseq_asc))

# Last row statistics for each quantile sofa group

#Q1#########################
summary(df.low.lastrow$icudayseq_asc)
summary(df.low.lastrow$sofa_last)
summary(df.low.lastrow$admission_age)
table(df.low.lastrow$gender)
(table(df.low.lastrow$gender)/length(unique(df.low$icustay_id)))*100
#Q2#########################
summary(df.high.lastrow$icudayseq_asc)
summary(df.high.lastrow$sofa_last)
summary(df.high.lastrow$admission_age)
table(df.high.lastrow$gender)
(table(df.high.lastrow$gender)/length(unique(df.high$icustay_id)))*100

rm(df.low.lastrow)
rm(df.high.lastrow)
rm(index.1)

#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#

##                                                                            ##
################################################################################
## Create data frames for transition matrixe                                   # 
################################################################################
##                                                                            ##

example.df<-df.low
example.df<-example.df %>% select(icustay_id, icudayseq_asc, states)
colnames(example.df)<- c("id", "day", "states")

f1 <- function(data, n){
  ids <- data %>%
    mutate(stateslead = lead(states, default = last(states))) %>%
    group_by(grp = rleid(states == 1)) %>% 
    filter(n() == n, states == 1, stateslead != 1) %>%     
    group_by(id) %>%     
    filter(n() == 1) %>%
    pull(id)
  
  data %>%
    filter(id %in% ids) %>%
    group_by(id) %>% 
    filter(cumsum(states) > 0)
}

# https://stackoverflow.com/questions/55432875/select-rows-of-ids-that-have-a-pattern-without-losing-the-other-rows/55434003?noredirect=1#comment97588830_55434003
# aknowledgement to akrun for code assistance for f1

df1.low<-f1(example.df, 1)
#
df1.low<-df1.low %>% mutate(seq=lead(states))
df1.low<- df1.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
tm1.low<-table(df1.low$states, df1.low$seq)
tmA1.low<-tm1.low/rowSums(tm1.low)
# fixing matrix to be a square 
tmA1.low<- cbind(tmA1.low, c(0,0,0,0,0))
colnames(tmA1.low)<-(c("nhit","cmo","discharge","death","hit"))
tmA1.low<-setcolorder(as.data.table(tmA1.low), c(1,5,2,3,4))
tmA1.low<-as.matrix(tmA1.low)
# markov object
mo1.low<-new("markovchain",transitionMatrix=(tmA1.low), name="MarkovChain Q1")
mo2.low<-mo1.low^2
mo3.low<-mo1.low^3
mo4.low<-mo1.low^4
mo5.low<-mo1.low^5
mo6.low<-mo1.low^6
mo7.low<-mo1.low^7
mo8.low<-mo1.low^8
mo9.low<-mo1.low^9
mo10.low<-mo1.low^10
mo11.low<-mo1.low^11
mo12.low<-mo1.low^12
mo13.low<-mo1.low^13
mo14.low<-mo1.low^14
mo15.low<-mo1.low^15
mo16.low<-mo1.low^16
mo17.low<-mo1.low^17
mo18.low<-mo1.low^18
mo19.low<-mo1.low^19
mo20.low<-mo1.low^20
mo21.low<-mo1.low^21
mo22.low<-mo1.low^22
mo23.low<-mo1.low^23
mo24.low<-mo1.low^24
mo25.low<-mo1.low^25
mo26.low<-mo1.low^26
mo27.low<-mo1.low^27
mo28.low<-mo1.low^28
mo29.low<-mo1.low^29
mo30.low<-mo1.low^30





df1.low<-f1(example.df, 1)
#
df1.low<-df1.low %>% mutate(seq=lead(states))
df1.low<- df1.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
tm1.low<-table(df1.low$states, df1.low$seq)
tmA1.low<-tm1.low/rowSums(tm1.low)
tmA1.low<- cbind(tmA1.low, c(0,0,0,0,0))
colnames(tmA1.low)<-(c("nhit","cmo","discharge","death","hit"))
tmA1.low<-setcolorder(as.data.table(tmA1.low), c(1,5,2,3,4))
tmA1.low<-as.matrix(tmA1.low)
mo1.low<-new("markovchain",transitionMatrix=(tmA1.low), name="MarkovChain Q1")

df2.low<-f1(example.df, 2)
df2.low<- df2.low %>% mutate(col1=lead(states))
df2.low<-df2.low[!(df2.low$states==1 & df2.low$col1==1),]
df2.low$col1<-NULL
#
df2.low<-df2.low %>% mutate(seq=lead(states))
df2.low<- df2.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
tm2.low<-table(df2.low$states, df2.low$seq)
tmA2.low<-tm2.low/rowSums(tm2.low)

df3.low<-f1(example.df, 3)
df3.low<- df3.low %>% mutate(col1=lead(states))
df3.low<-df3.low[!(df3.low$states==1 & df3.low$col1==1),]
df3.low$col1<-NULL
#
df3.low<-df3.low %>% mutate(seq=lead(states))
df3.low<- df3.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
tm3.low<-table(df3.low$states, df3.low$seq)
tmA3.low<-tm3.low/rowSums(tm3.low)

df4.low<-f1(example.df, 4)
df4.low<- df4.low %>% mutate(col1=lead(states))
df4.low<-df4.low[!(df4.low$states==1 & df4.low$col1==1),]
df4.low$col1<-NULL
#
df4.low<-df4.low %>% mutate(seq=lead(states))
df4.low<- df4.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
tm4.low<-table(df4.low$states, df4.low$seq)
tmA4.low<-tm4.low/rowSums(tm4.low)

df5.low<-f1(example.df, 5)
df5.low<- df5.low %>% mutate(col1=lead(states))
df5.low<-df5.low[!(df5.low$states==1 & df5.low$col1==1),]
df5.low$col1<-NULL
#
df5.low<-df5.low %>% mutate(seq=lead(states))
df5.low<- df5.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
tm5.low<-table(df5.low$states, df5.low$seq)
tmA5.low<-tm5.low/rowSums(tm5.low)

df6.low<-f1(example.df, 6)
df6.low<- df6.low %>% mutate(col1=lead(states))
df6.low<-df6.low[!(df6.low$states==1 & df6.low$col1==1),]
df6.low$col1<-NULL
#
df6.low<-df6.low %>% mutate(seq=lead(states))
df6.low<- df6.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
tm6.low<-table(df6.low$states, df6.low$seq)
tmA6.low<-tm6.low/rowSums(tm6.low)

df7.low<-f1(example.df, 7)
df7.low<- df7.low %>% mutate(col1=lead(states))
df7.low<-df7.low[!(df7.low$states==1 & df7.low$col1==1),]
df7.low$col1<-NULL
#
df7.low<-df7.low %>% mutate(seq=lead(states))
df7.low<- df7.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
tm7.low<-table(df7.low$states, df7.low$seq)
tmA7.low<-tm7.low/rowSums(tm7.low)

df8.low<-f1(example.df, 8)
df8.low<- df8.low %>% mutate(col1=lead(states))
df8.low<-df8.low[!(df8.low$states==1 & df8.low$col1==1),]
df8.low$col1<-NULL
#
df8.low<-df8.low %>% mutate(seq=lead(states))
df8.low<- df8.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
tm8.low<-table(df8.low$states, df8.low$seq)
tmA8.low<-tm8.low/rowSums(tm8.low)




# library(purrr)
# out1 <- map(1:3, f1, data = example.df)

#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#

################################################################################
# Quartile analysis                                                            #
################################################################################
# Divide cohort based on groups and demographics ###############################
################################################################################

# Obtain keys for sofa groups
sofa<-df %>% filter(icudayseq_asc == 0)

# Create df keys for each quantile sofa severity group
key.q1<-sofa %>% filter(sofa_group %in% "Q1") %>% select(icustay_id)
key.q2<-sofa %>% filter(sofa_group %in% "Q2") %>% select(icustay_id)
key.q3<-sofa %>% filter(sofa_group %in% "Q3") %>% select(icustay_id)
key.q4<-sofa %>% filter(sofa_group %in% "Q4") %>% select(icustay_id)

# Create df for each quantile sofa severity group
df.q1<-semi_join(df, key.q1)
df.q2<-semi_join(df, key.q2)
df.q3<-semi_join(df, key.q3)
df.q4<-semi_join(df, key.q4)

# Drop keys and sofa table
rm(key.q1)
rm(key.q2)
rm(key.q3)
rm(key.q4)
rm(sofa)

# Compute demographics for each quantile sofa group

#Q1#########################
table(df.q1$death_flag)
length(unique(df.q1$icustay_id))
#Q2#########################
table(df.q2$death_flag)
length(unique(df.q2$icustay_id))
#Q3#########################
table(df.q3$death_flag)
length(unique(df.q3$icustay_id))
#Q4#########################
table(df.q4$death_flag)
length(unique(df.q4$icustay_id))

# How many days on average did they spend in each unit

#Q1#########################
table(df.q1$states)[1:3]
sum(table(df.q1$states)[1:3]) # total number of days in ICU
sum(table(df.q1$states)) # total number of days passed
#Q2#########################
table(df.q2$states)[1:3]
sum(table(df.q2$states)[1:3])# total number of days in ICU
sum(table(df.q2$states)) # total number of days passed
#Q3#########################
table(df.q3$states)[1:3]
sum(table(df.q3$states)[1:3])# total number of days in ICU
sum(table(df.q3$states)) # total number of days passed
#Q4#########################
table(df.q4$states)[1:3]
sum(table(df.q4$states)[1:3])# total number of days in ICU
sum(table(df.q4$states)) # total number of days passed


# Obtain the last day metrics for each quantile sofa group
df.q1.lastrow<-df.q1 %>% group_by(icustay_id) %>% filter(icudayseq_asc %in% max(icudayseq_asc))
df.q2.lastrow<-df.q2 %>% group_by(icustay_id) %>% filter(icudayseq_asc %in% max(icudayseq_asc))
df.q3.lastrow<-df.q3 %>% group_by(icustay_id) %>% filter(icudayseq_asc %in% max(icudayseq_asc))
df.q4.lastrow<-df.q4 %>% group_by(icustay_id) %>% filter(icudayseq_asc %in% max(icudayseq_asc))

# Last row statistics for each quantile sofa group

#Q1#########################
summary(df.q1.lastrow$icudayseq_asc)
summary(df.q1.lastrow$sofa_last)
summary(df.q1.lastrow$admission_age)
table(df.q1.lastrow$gender)
(table(df.q1.lastrow$gender)/length(unique(df.q1$icustay_id)))*100
#Q2#########################
summary(df.q2.lastrow$icudayseq_asc)
summary(df.q2.lastrow$sofa_last)
summary(df.q2.lastrow$admission_age)
table(df.q2.lastrow$gender)
(table(df.q2.lastrow$gender)/length(unique(df.q2$icustay_id)))*100
#Q3#########################
summary(df.q3.lastrow$icudayseq_asc)
summary(df.q3.lastrow$sofa_last)
summary(df.q3.lastrow$admission_age)
table(df.q3.lastrow$gender)
(table(df.q3.lastrow$gender)/length(unique(df.q3$icustay_id)))*100
#Q4#########################
summary(df.q4.lastrow$icudayseq_asc)
summary(df.q4.lastrow$sofa_last)
summary(df.q4.lastrow$admission_age)
table(df.q4.lastrow$gender)
(table(df.q4.lastrow$gender)/length(unique(df.q4$icustay_id)))*100

rm(df.q1.lastrow)
rm(df.q2.lastrow)
rm(df.q3.lastrow)
rm(df.q4.lastrow)

#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#



#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#



##                                                                            ##   
################################################################################
################################################################################
# Quartile I transition matrix for treatment options                           #
################################################################################
################################################################################
##                                                                            ##

# ONE DAY OF HIC 

#find index of 1s then say if the next index is not 1 keep that row 
index.1<-which(df.q1$states %in% c(1)) 
index.2<-index.1+1
hic.ma<- matrix(c(df.q1$states[index.1],df.q1$states[index.1+1]), nrow=length(index.1), ncol=2)
hic.df<-as.data.frame(hic.ma) %>% filter(V1!=V2)
tm.1<-table(hic.df$V1, hic.df$V2)

# TWO DAYs OF HIC 
index.3<-index.1+2
hic.ma<-cbind(hic.ma, matrix(df.q1$states[index.3], nrow=length(df.q1$states[index.3])))
#
hic.ma2<-hic.ma[hic.ma[,3] != 1, ]
hic.ma2<-hic.ma2[hic.ma2[,2] == 1, ]
hic.ma2<-hic.ma2[,-1]
tm.2<-table(hic.ma2[,1], hic.ma2[,2])

# THREE DAYs OF HIC 
index.4<-index.1+3
hic.ma<-cbind(hic.ma, matrix(df.q1$states[index.4], nrow=length(df.q1$states[index.4])))
#
hic.ma3<-hic.ma[hic.ma[,4] != 1, ]
hic.ma3<-hic.ma3[hic.ma3[,3] == 1, ]
hic.ma3<-hic.ma3[hic.ma3[,2] == 1, ]
hic.ma3<-hic.ma3[,-c(1,2)]
hic.ma3[complete.cases(hic.ma3), ]
tm.3<-table(hic.ma3[,1], hic.ma3[,2])

# FOUR DAYs OF HIC 
index.5<-index.1+4
hic.ma<-cbind(hic.ma, matrix(df.q1$states[index.5], nrow=length(df.q1$states[index.5])))
#
hic.ma4<-hic.ma[hic.ma[,5] != 1, ]
hic.ma4<-hic.ma4[hic.ma4[,4] == 1, ]
hic.ma4<-hic.ma4[hic.ma4[,3] == 1, ]
hic.ma4<-hic.ma4[hic.ma4[,2] == 1, ]
hic.ma4<-hic.ma4[,-c(1,2,3)]
hic.ma4[complete.cases(hic.ma4), ]
tm.4<-table(hic.ma4[,1], hic.ma4[,2])

# FIVE DAYs OF HIC
index.6<-index.1+5
hic.ma<-cbind(hic.ma, matrix(df.q1$states[index.6], nrow=length(df.q1$states[index.6])))
#
hic.ma5<-hic.ma[hic.ma[,6] != 1, ]
hic.ma5<-hic.ma5[hic.ma5[,5] == 1, ]
hic.ma5<-hic.ma5[hic.ma5[,4] == 1, ]
hic.ma5<-hic.ma5[hic.ma5[,3] == 1, ]
hic.ma5<-hic.ma5[hic.ma5[,2] == 1, ]
hic.ma5<-hic.ma5[,-c(1,2,3,4)]
hic.ma5[complete.cases(hic.ma5), ]
tm.5<-table(hic.ma5[,1], hic.ma5[,2])

# SIX DAYs OF HIC 
index.7<-index.1+6
hic.ma<-cbind(hic.ma, matrix(df.q1$states[index.7], nrow=length(df.q1$states[index.7])))
#
hic.ma6<-hic.ma[hic.ma[,7] != 1, ]
hic.ma6<-hic.ma6[hic.ma6[,6] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,5] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,4] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,3] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,2] == 1, ]
hic.ma6<-hic.ma6[,-c(1,2,3,4,5)]
hic.ma6[complete.cases(hic.ma6), ]
tm.6<-table(hic.ma6[,1], hic.ma6[,2])

# SEVEN DAYs OF HIC 
index.8<-index.1+7
hic.ma<-cbind(hic.ma, matrix(df.q1$states[index.8], nrow=length(df.q1$states[index.8])))
#
hic.ma7<-hic.ma[hic.ma[,8] != 1, ]
hic.ma7<-hic.ma7[hic.ma7[,7] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,6] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,5] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,4] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,3] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,2] == 1, ]
hic.ma7<-hic.ma7[,-c(1,2,3,4,5,6)]
hic.ma7[complete.cases(hic.ma7), ]
tm.7<-table(hic.ma7[,1], hic.ma7[,2])

# EIGHT DAYs OF HIC 
index.9<-index.1+8
hic.ma<-cbind(hic.ma, matrix(df.q1$states[index.9], nrow=length(df.q1$states[index.9])))
#
hic.ma8<-hic.ma[hic.ma[,9] != 1, ]
hic.ma8<-hic.ma8[hic.ma8[,8] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,7] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,6] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,5] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,4] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,3] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,2] == 1, ]
hic.ma8<-hic.ma8[,-c(1,2,3,4,5,6,7)]
hic.ma8[complete.cases(hic.ma8), ]
tm.8<-table(hic.ma8[,1], hic.ma8[,2])

# NINE DAYs OF HIC 
index.10<-index.1+9
hic.ma<-cbind(hic.ma, matrix(df.q1$states[index.10], nrow=length(df.q1$states[index.10])))
#
hic.ma9<-hic.ma[hic.ma[,10] != 1, ]
hic.ma9<-hic.ma9[hic.ma9[,9] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,8] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,7] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,6] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,5] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,4] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,3] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,2] == 1, ]
hic.ma9<-hic.ma9[,-c(1,2,3,4,5,6,7,8)]
hic.ma9[complete.cases(hic.ma9), ]
tm.9<-table(hic.ma9[,1], hic.ma9[,2])

# TEN DAYs OF HIC 
index.11<-index.1+10
hic.ma<-cbind(hic.ma, matrix(df.q1$states[index.11], nrow=length(df.q1$states[index.11])))
#
hic.ma10<-hic.ma[hic.ma[,11] != 1, ]
hic.ma10<-hic.ma10[hic.ma10[,10] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,9] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,8] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,7] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,6] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,5] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,4] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,3] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,2] == 1, ]
hic.ma10<-hic.ma10[,-c(1,2,3,4,5,6,7,8,9)]
hic.ma10[complete.cases(hic.ma10), ]
tm.10<-table(hic.ma10[,1], hic.ma10[,2])

# ELEVEN DAYs OF HIC 
index.12<-index.1+11
hic.ma<-cbind(hic.ma, matrix(df.q1$states[index.12], nrow=length(df.q1$states[index.12])))
#
hic.ma11<-hic.ma[hic.ma[,12] != 1, ]
hic.ma11<-hic.ma11[hic.ma11[,11] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,10] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,9] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,8] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,7] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,6] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,5] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,4] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,3] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,2] == 1, ]
hic.ma11<-hic.ma11[,-c(1,2,3,4,5,6,7,8,9,10)]
hic.ma11[complete.cases(hic.ma11), ]
tm.11<-table(hic.ma11[,1], hic.ma11[,2])

# TWELVE DAYs OF HIC 
index.13<-index.1+12
hic.ma<-cbind(hic.ma, matrix(df.q1$states[index.13], nrow=length(df.q1$states[index.13])))
#
hic.ma12<-hic.ma[hic.ma[,13] != 1, ]
hic.ma12<-hic.ma12[hic.ma12[,12] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,11] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,10] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,9] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,8] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,7] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,6] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,5] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,4] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,3] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,2] == 1, ]
hic.ma12<-hic.ma12[,-c(1,2,3,4,5,6,7,8,9,10,11)]
hic.ma12[complete.cases(hic.ma12), ]
tm.12<-table(hic.ma12[,1], hic.ma12[,2])

# THIRTEEN DAYs OF HIC 
index.14<-index.1+13
hic.ma<-cbind(hic.ma, matrix(df.q1$states[index.14], nrow=length(df.q1$states[index.14])))
#
hic.ma13<-hic.ma[hic.ma[,14] != 1, ]
hic.ma13<-hic.ma13[hic.ma13[,13] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,12] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,11] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,10] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,9] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,8] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,7] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,6] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,5] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,4] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,3] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,2] == 1, ]
hic.ma13<-hic.ma13[,-c(1,2,3,4,5,6,7,8,9,10,11,12)]
hic.ma13[complete.cases(hic.ma13), ]
tm.13<-table(hic.ma13[,1], hic.ma13[,2])

# FOURTEEN DAYs OF HIC 
index.15<-index.1+14
hic.ma<-cbind(hic.ma, matrix(df.q1$states[index.15], nrow=length(df.q1$states[index.15])))
#
hic.ma14<-hic.ma[hic.ma[,15] != 1, ]
hic.ma14<-hic.ma14[hic.ma14[,14] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,13] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,12] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,11] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,10] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,9] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,8] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,7] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,6] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,5] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,4] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,3] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,2] == 1, ]
hic.ma14<-hic.ma14[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13)]
hic.ma14[complete.cases(hic.ma14), ]
tm.14<-table(hic.ma14[,1], hic.ma14[,2])

# FIFTEEN DAYs OF HIC 
index.16<-index.1+15
hic.ma<-cbind(hic.ma, matrix(df.q1$states[index.16], nrow=length(df.q1$states[index.16])))
#
hic.ma15<-hic.ma[hic.ma[,16] != 1, ]
hic.ma15<-hic.ma15[hic.ma15[,15] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,14] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,13] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,12] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,11] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,10] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,9] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,8] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,7] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,6] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,5] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,4] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,3] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,2] == 1, ]
hic.ma15<-hic.ma15[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
hic.ma15[complete.cases(hic.ma15), ]
tm.15<-table(hic.ma15[,1], hic.ma15[,2])

# SIXTEEN DAYs OF HIC 
index.17<-index.1+16
hic.ma<-cbind(hic.ma, matrix(df.q1$states[index.17], nrow=length(df.q1$states[index.17])))
#
hic.ma16<-hic.ma[hic.ma[,17] != 1, ]
hic.ma16<-hic.ma16[hic.ma16[,16] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,15] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,14] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,13] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,12] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,11] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,10] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,9] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,8] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,7] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,6] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,5] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,4] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,3] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,2] == 1, ]
hic.ma16<-hic.ma16[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
hic.ma16[complete.cases(hic.ma16), ]
tm.16<-table(hic.ma16[,1], hic.ma16[,2])

#------------------------------------------------------------------------------
# With more data we can consider more days but for now we have less than 10 
# patients at this point 
# SEVENTEEN DAYs OF HIC 
# EIGTHTEEN DAYs OF HIC 
# NINETEEN DAYs OF HIC 
# TWENTY DAYs OF HIC 
# TWENTYONE DAYs OF HIC 
# TWENTYTWO DAYs OF HIC 
# TWENTYTHREE DAYs OF HIC 
# TWENTYFOUR DAYs OF HIC 
# TWENTYFIVE DAYs OF HIC 
# TWENTYSIX DAYs OF HIC 
# TWENTYSEVEN DAYs OF HIC 
# TWENTYEIGHT DAYs OF HIC 
# TWENTYNINE DAYs OF HIC 
# THIRTY DAYs OF HIC 
#-------------------------------------------------------------------------------

rm(index.1)
rm(index.2)
rm(index.3)
rm(index.4)
rm(index.5)
rm(index.6)
rm(index.7)
rm(index.8)
rm(index.9)
rm(index.10)
rm(index.11)
rm(index.12)
rm(index.13)
rm(index.14)
rm(index.15)
rm(index.16)
rm(index.17)
rm(hic.ma)
rm(hic.ma2)
rm(hic.ma3)
rm(hic.ma4)
rm(hic.ma5)
rm(hic.ma6)
rm(hic.ma7)
rm(hic.ma8)
rm(hic.ma9)
rm(hic.ma10)
rm(hic.ma11)
rm(hic.ma12)
rm(hic.ma13)
rm(hic.ma14)
rm(hic.ma15)
rm(hic.ma16)

##                                                                            ##   
################################################################################
################################################################################
# Quartile II transition matrix for treatment options                          #
################################################################################
################################################################################
##                                                                            ##

# ONE DAY OF HIC 

#find index of 1s then say if the next index is not 1 keep that row 
index.1<-which(df.q2$states %in% c(1)) 
index.2<-index.1+1
hic.ma<- matrix(c(df.q2$states[index.1],df.q2$states[index.1+1]), nrow=length(index.1), ncol=2)
hic.df<-as.data.frame(hic.ma) %>% filter(V1!=V2)
q2tm.1<-table(hic.df$V1, hic.df$V2)

# TWO DAYs OF HIC 
index.3<-index.1+2
hic.ma<-cbind(hic.ma, matrix(df.q2$states[index.3], nrow=length(df.q2$states[index.3])))
#
hic.ma2<-hic.ma[hic.ma[,3] != 1, ]
hic.ma2<-hic.ma2[hic.ma2[,2] == 1, ]
hic.ma2<-hic.ma2[,-1]
q2tm.2<-table(hic.ma2[,1], hic.ma2[,2])

# THREE DAYs OF HIC 
index.4<-index.1+3
hic.ma<-cbind(hic.ma, matrix(df.q2$states[index.4], nrow=length(df.q2$states[index.4])))
#
hic.ma3<-hic.ma[hic.ma[,4] != 1, ]
hic.ma3<-hic.ma3[hic.ma3[,3] == 1, ]
hic.ma3<-hic.ma3[hic.ma3[,2] == 1, ]
hic.ma3<-hic.ma3[,-c(1,2)]
hic.ma3[complete.cases(hic.ma3), ]
q2tm.3<-table(hic.ma3[,1], hic.ma3[,2])

# FOUR DAYs OF HIC 
index.5<-index.1+4
hic.ma<-cbind(hic.ma, matrix(df.q2$states[index.5], nrow=length(df.q2$states[index.5])))
#
hic.ma4<-hic.ma[hic.ma[,5] != 1, ]
hic.ma4<-hic.ma4[hic.ma4[,4] == 1, ]
hic.ma4<-hic.ma4[hic.ma4[,3] == 1, ]
hic.ma4<-hic.ma4[hic.ma4[,2] == 1, ]
hic.ma4<-hic.ma4[,-c(1,2,3)]
hic.ma4[complete.cases(hic.ma4), ]
q2tm.4<-table(hic.ma4[,1], hic.ma4[,2])

# FIVE DAYs OF HIC
index.6<-index.1+5
hic.ma<-cbind(hic.ma, matrix(df.q2$states[index.6], nrow=length(df.q2$states[index.6])))
#
hic.ma5<-hic.ma[hic.ma[,6] != 1, ]
hic.ma5<-hic.ma5[hic.ma5[,5] == 1, ]
hic.ma5<-hic.ma5[hic.ma5[,4] == 1, ]
hic.ma5<-hic.ma5[hic.ma5[,3] == 1, ]
hic.ma5<-hic.ma5[hic.ma5[,2] == 1, ]
hic.ma5<-hic.ma5[,-c(1,2,3,4)]
hic.ma5[complete.cases(hic.ma5), ]
q2tm.5<-table(hic.ma5[,1], hic.ma5[,2])

# SIX DAYs OF HIC 
index.7<-index.1+6
hic.ma<-cbind(hic.ma, matrix(df.q2$states[index.7], nrow=length(df.q2$states[index.7])))
#
hic.ma6<-hic.ma[hic.ma[,7] != 1, ]
hic.ma6<-hic.ma6[hic.ma6[,6] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,5] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,4] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,3] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,2] == 1, ]
hic.ma6<-hic.ma6[,-c(1,2,3,4,5)]
hic.ma6[complete.cases(hic.ma6), ]
q2tm.6<-table(hic.ma6[,1], hic.ma6[,2])

# SEVEN DAYs OF HIC 
index.8<-index.1+7
hic.ma<-cbind(hic.ma, matrix(df.q2$states[index.8], nrow=length(df.q2$states[index.8])))
#
hic.ma7<-hic.ma[hic.ma[,8] != 1, ]
hic.ma7<-hic.ma7[hic.ma7[,7] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,6] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,5] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,4] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,3] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,2] == 1, ]
hic.ma7<-hic.ma7[,-c(1,2,3,4,5,6)]
hic.ma7[complete.cases(hic.ma7), ]
q2tm.7<-table(hic.ma7[,1], hic.ma7[,2])

# EIGHT DAYs OF HIC 
index.9<-index.1+8
hic.ma<-cbind(hic.ma, matrix(df.q2$states[index.9], nrow=length(df.q2$states[index.9])))
#
hic.ma8<-hic.ma[hic.ma[,9] != 1, ]
hic.ma8<-hic.ma8[hic.ma8[,8] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,7] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,6] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,5] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,4] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,3] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,2] == 1, ]
hic.ma8<-hic.ma8[,-c(1,2,3,4,5,6,7)]
hic.ma8[complete.cases(hic.ma8), ]
q2tm.8<-table(hic.ma8[,1], hic.ma8[,2])

# NINE DAYs OF HIC 
index.10<-index.1+9
hic.ma<-cbind(hic.ma, matrix(df.q2$states[index.10], nrow=length(df.q2$states[index.10])))
#
hic.ma9<-hic.ma[hic.ma[,10] != 1, ]
hic.ma9<-hic.ma9[hic.ma9[,9] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,8] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,7] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,6] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,5] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,4] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,3] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,2] == 1, ]
hic.ma9<-hic.ma9[,-c(1,2,3,4,5,6,7,8)]
hic.ma9[complete.cases(hic.ma9), ]
q2tm.9<-table(hic.ma9[,1], hic.ma9[,2])

# TEN DAYs OF HIC 
index.11<-index.1+10
hic.ma<-cbind(hic.ma, matrix(df.q2$states[index.11], nrow=length(df.q2$states[index.11])))
#
hic.ma10<-hic.ma[hic.ma[,11] != 1, ]
hic.ma10<-hic.ma10[hic.ma10[,10] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,9] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,8] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,7] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,6] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,5] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,4] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,3] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,2] == 1, ]
hic.ma10<-hic.ma10[,-c(1,2,3,4,5,6,7,8,9)]
hic.ma10[complete.cases(hic.ma10), ]
q2tm.10<-table(hic.ma10[,1], hic.ma10[,2])

# ELEVEN DAYs OF HIC 
index.12<-index.1+11
hic.ma<-cbind(hic.ma, matrix(df.q2$states[index.12], nrow=length(df.q2$states[index.12])))
#
hic.ma11<-hic.ma[hic.ma[,12] != 1, ]
hic.ma11<-hic.ma11[hic.ma11[,11] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,10] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,9] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,8] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,7] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,6] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,5] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,4] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,3] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,2] == 1, ]
hic.ma11<-hic.ma11[,-c(1,2,3,4,5,6,7,8,9,10)]
hic.ma11[complete.cases(hic.ma11), ]
q2tm.11<-table(hic.ma11[,1], hic.ma11[,2])

# TWELVE DAYs OF HIC 
index.13<-index.1+12
hic.ma<-cbind(hic.ma, matrix(df.q2$states[index.13], nrow=length(df.q2$states[index.13])))
#
hic.ma12<-hic.ma[hic.ma[,13] != 1, ]
hic.ma12<-hic.ma12[hic.ma12[,12] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,11] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,10] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,9] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,8] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,7] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,6] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,5] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,4] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,3] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,2] == 1, ]
hic.ma12<-hic.ma12[,-c(1,2,3,4,5,6,7,8,9,10,11)]
hic.ma12[complete.cases(hic.ma12), ]
q2tm.12<-table(hic.ma12[,1], hic.ma12[,2])

# THIRTEEN DAYs OF HIC 
index.14<-index.1+13
hic.ma<-cbind(hic.ma, matrix(df.q2$states[index.14], nrow=length(df.q2$states[index.14])))
#
hic.ma13<-hic.ma[hic.ma[,14] != 1, ]
hic.ma13<-hic.ma13[hic.ma13[,13] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,12] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,11] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,10] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,9] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,8] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,7] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,6] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,5] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,4] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,3] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,2] == 1, ]
hic.ma13<-hic.ma13[,-c(1,2,3,4,5,6,7,8,9,10,11,12)]
hic.ma13[complete.cases(hic.ma13), ]
q2tm.13<-table(hic.ma13[,1], hic.ma13[,2])

# FOURTEEN DAYs OF HIC 
index.15<-index.1+14
hic.ma<-cbind(hic.ma, matrix(df.q2$states[index.15], nrow=length(df.q2$states[index.15])))
#
hic.ma14<-hic.ma[hic.ma[,15] != 1, ]
hic.ma14<-hic.ma14[hic.ma14[,14] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,13] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,12] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,11] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,10] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,9] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,8] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,7] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,6] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,5] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,4] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,3] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,2] == 1, ]
hic.ma14<-hic.ma14[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13)]
hic.ma14[complete.cases(hic.ma14), ]
q2tm.14<-table(hic.ma14[,1], hic.ma14[,2])

# FIFTEEN DAYs OF HIC 
index.16<-index.1+15
hic.ma<-cbind(hic.ma, matrix(df.q2$states[index.16], nrow=length(df.q2$states[index.16])))
#
hic.ma15<-hic.ma[hic.ma[,16] != 1, ]
hic.ma15<-hic.ma15[hic.ma15[,15] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,14] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,13] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,12] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,11] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,10] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,9] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,8] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,7] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,6] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,5] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,4] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,3] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,2] == 1, ]
hic.ma15<-hic.ma15[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
hic.ma15[complete.cases(hic.ma15), ]
q2tm.15<-table(hic.ma15[,1], hic.ma15[,2])

# SIXTEEN DAYs OF HIC 
index.17<-index.1+16
hic.ma<-cbind(hic.ma, matrix(df.q2$states[index.17], nrow=length(df.q2$states[index.17])))
#
hic.ma16<-hic.ma[hic.ma[,17] != 1, ]
hic.ma16<-hic.ma16[hic.ma16[,16] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,15] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,14] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,13] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,12] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,11] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,10] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,9] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,8] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,7] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,6] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,5] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,4] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,3] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,2] == 1, ]
hic.ma16<-hic.ma16[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
hic.ma16[complete.cases(hic.ma16), ]
q2tm.16<-table(hic.ma16[,1], hic.ma16[,2])

#------------------------------------------------------------------------------
# With more data we can consider more days but for now we have less than 10 
# patients at this point 
# SEVENTEEN DAYs OF HIC 
# EIGTHTEEN DAYs OF HIC 
# NINETEEN DAYs OF HIC 
# TWENTY DAYs OF HIC 
# TWENTYONE DAYs OF HIC 
# TWENTYTWO DAYs OF HIC 
# TWENTYTHREE DAYs OF HIC 
# TWENTYFOUR DAYs OF HIC 
# TWENTYFIVE DAYs OF HIC 
# TWENTYSIX DAYs OF HIC 
# TWENTYSEVEN DAYs OF HIC 
# TWENTYEIGHT DAYs OF HIC 
# TWENTYNINE DAYs OF HIC 
# THIRTY DAYs OF HIC 
#-------------------------------------------------------------------------------

rm(index.1)
rm(index.2)
rm(index.3)
rm(index.4)
rm(index.5)
rm(index.6)
rm(index.7)
rm(index.8)
rm(index.9)
rm(index.10)
rm(index.11)
rm(index.12)
rm(index.13)
rm(index.14)
rm(index.15)
rm(index.16)
rm(index.17)
rm(hic.ma)
rm(hic.ma2)
rm(hic.ma3)
rm(hic.ma4)
rm(hic.ma5)
rm(hic.ma6)
rm(hic.ma7)
rm(hic.ma8)
rm(hic.ma9)
rm(hic.ma10)
rm(hic.ma11)
rm(hic.ma12)
rm(hic.ma13)
rm(hic.ma14)
rm(hic.ma15)
rm(hic.ma16)

##                                                                            ##   
################################################################################
################################################################################
# Quartile III transition matrix for treatment options                         #
################################################################################
################################################################################
##                                                                            ##

# ONE DAY OF HIC 

#find index of 1s then say if the next index is not 1 keep that row 
index.1<-which(df.q3$states %in% c(1)) 
index.2<-index.1+1
hic.ma<- matrix(c(df.q3$states[index.1],df.q3$states[index.1+1]), nrow=length(index.1), ncol=2)
hic.df<-as.data.frame(hic.ma) %>% filter(V1!=V2)
q3tm.1<-table(hic.df$V1, hic.df$V2)

# TWO DAYs OF HIC 
index.3<-index.1+2
hic.ma<-cbind(hic.ma, matrix(df.q3$states[index.3], nrow=length(df.q3$states[index.3])))
#
hic.ma2<-hic.ma[hic.ma[,3] != 1, ]
hic.ma2<-hic.ma2[hic.ma2[,2] == 1, ]
hic.ma2<-hic.ma2[,-1]
q3tm.2<-table(hic.ma2[,1], hic.ma2[,2])

# THREE DAYs OF HIC 
index.4<-index.1+3
hic.ma<-cbind(hic.ma, matrix(df.q3$states[index.4], nrow=length(df.q3$states[index.4])))
#
hic.ma3<-hic.ma[hic.ma[,4] != 1, ]
hic.ma3<-hic.ma3[hic.ma3[,3] == 1, ]
hic.ma3<-hic.ma3[hic.ma3[,2] == 1, ]
hic.ma3<-hic.ma3[,-c(1,2)]
hic.ma3[complete.cases(hic.ma3), ]
q3tm.3<-table(hic.ma3[,1], hic.ma3[,2])

# FOUR DAYs OF HIC 
index.5<-index.1+4
hic.ma<-cbind(hic.ma, matrix(df.q3$states[index.5], nrow=length(df.q3$states[index.5])))
#
hic.ma4<-hic.ma[hic.ma[,5] != 1, ]
hic.ma4<-hic.ma4[hic.ma4[,4] == 1, ]
hic.ma4<-hic.ma4[hic.ma4[,3] == 1, ]
hic.ma4<-hic.ma4[hic.ma4[,2] == 1, ]
hic.ma4<-hic.ma4[,-c(1,2,3)]
hic.ma4[complete.cases(hic.ma4), ]
q3tm.4<-table(hic.ma4[,1], hic.ma4[,2])

# FIVE DAYs OF HIC
index.6<-index.1+5
hic.ma<-cbind(hic.ma, matrix(df.q3$states[index.6], nrow=length(df.q3$states[index.6])))
#
hic.ma5<-hic.ma[hic.ma[,6] != 1, ]
hic.ma5<-hic.ma5[hic.ma5[,5] == 1, ]
hic.ma5<-hic.ma5[hic.ma5[,4] == 1, ]
hic.ma5<-hic.ma5[hic.ma5[,3] == 1, ]
hic.ma5<-hic.ma5[hic.ma5[,2] == 1, ]
hic.ma5<-hic.ma5[,-c(1,2,3,4)]
hic.ma5[complete.cases(hic.ma5), ]
q3tm.5<-table(hic.ma5[,1], hic.ma5[,2])

# SIX DAYs OF HIC 
index.7<-index.1+6
hic.ma<-cbind(hic.ma, matrix(df.q3$states[index.7], nrow=length(df.q3$states[index.7])))
#
hic.ma6<-hic.ma[hic.ma[,7] != 1, ]
hic.ma6<-hic.ma6[hic.ma6[,6] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,5] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,4] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,3] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,2] == 1, ]
hic.ma6<-hic.ma6[,-c(1,2,3,4,5)]
hic.ma6[complete.cases(hic.ma6), ]
q3tm.6<-table(hic.ma6[,1], hic.ma6[,2])

# SEVEN DAYs OF HIC 
index.8<-index.1+7
hic.ma<-cbind(hic.ma, matrix(df.q3$states[index.8], nrow=length(df.q3$states[index.8])))
#
hic.ma7<-hic.ma[hic.ma[,8] != 1, ]
hic.ma7<-hic.ma7[hic.ma7[,7] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,6] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,5] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,4] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,3] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,2] == 1, ]
hic.ma7<-hic.ma7[,-c(1,2,3,4,5,6)]
hic.ma7[complete.cases(hic.ma7), ]
q3tm.7<-table(hic.ma7[,1], hic.ma7[,2])

# EIGHT DAYs OF HIC 
index.9<-index.1+8
hic.ma<-cbind(hic.ma, matrix(df.q3$states[index.9], nrow=length(df.q3$states[index.9])))
#
hic.ma8<-hic.ma[hic.ma[,9] != 1, ]
hic.ma8<-hic.ma8[hic.ma8[,8] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,7] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,6] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,5] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,4] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,3] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,2] == 1, ]
hic.ma8<-hic.ma8[,-c(1,2,3,4,5,6,7)]
hic.ma8[complete.cases(hic.ma8), ]
q3tm.8<-table(hic.ma8[,1], hic.ma8[,2])

# NINE DAYs OF HIC 
index.10<-index.1+9
hic.ma<-cbind(hic.ma, matrix(df.q3$states[index.10], nrow=length(df.q3$states[index.10])))
#
hic.ma9<-hic.ma[hic.ma[,10] != 1, ]
hic.ma9<-hic.ma9[hic.ma9[,9] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,8] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,7] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,6] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,5] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,4] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,3] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,2] == 1, ]
hic.ma9<-hic.ma9[,-c(1,2,3,4,5,6,7,8)]
hic.ma9[complete.cases(hic.ma9), ]
q3tm.9<-table(hic.ma9[,1], hic.ma9[,2])

# TEN DAYs OF HIC 
index.11<-index.1+10
hic.ma<-cbind(hic.ma, matrix(df.q3$states[index.11], nrow=length(df.q3$states[index.11])))
#
hic.ma10<-hic.ma[hic.ma[,11] != 1, ]
hic.ma10<-hic.ma10[hic.ma10[,10] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,9] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,8] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,7] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,6] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,5] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,4] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,3] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,2] == 1, ]
hic.ma10<-hic.ma10[,-c(1,2,3,4,5,6,7,8,9)]
hic.ma10[complete.cases(hic.ma10), ]
q3tm.10<-table(hic.ma10[,1], hic.ma10[,2])

# ELEVEN DAYs OF HIC 
index.12<-index.1+11
hic.ma<-cbind(hic.ma, matrix(df.q3$states[index.12], nrow=length(df.q3$states[index.12])))
#
hic.ma11<-hic.ma[hic.ma[,12] != 1, ]
hic.ma11<-hic.ma11[hic.ma11[,11] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,10] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,9] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,8] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,7] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,6] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,5] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,4] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,3] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,2] == 1, ]
hic.ma11<-hic.ma11[,-c(1,2,3,4,5,6,7,8,9,10)]
hic.ma11[complete.cases(hic.ma11), ]
q3tm.11<-table(hic.ma11[,1], hic.ma11[,2])

# TWELVE DAYs OF HIC 
index.13<-index.1+12
hic.ma<-cbind(hic.ma, matrix(df.q3$states[index.13], nrow=length(df.q3$states[index.13])))
#
hic.ma12<-hic.ma[hic.ma[,13] != 1, ]
hic.ma12<-hic.ma12[hic.ma12[,12] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,11] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,10] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,9] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,8] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,7] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,6] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,5] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,4] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,3] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,2] == 1, ]
hic.ma12<-hic.ma12[,-c(1,2,3,4,5,6,7,8,9,10,11)]
hic.ma12[complete.cases(hic.ma12), ]
q3tm.12<-table(hic.ma12[,1], hic.ma12[,2])

# THIRTEEN DAYs OF HIC 
index.14<-index.1+13
hic.ma<-cbind(hic.ma, matrix(df.q3$states[index.14], nrow=length(df.q3$states[index.14])))
#
hic.ma13<-hic.ma[hic.ma[,14] != 1, ]
hic.ma13<-hic.ma13[hic.ma13[,13] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,12] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,11] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,10] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,9] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,8] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,7] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,6] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,5] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,4] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,3] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,2] == 1, ]
hic.ma13<-hic.ma13[,-c(1,2,3,4,5,6,7,8,9,10,11,12)]
hic.ma13[complete.cases(hic.ma13), ]
q3tm.13<-table(hic.ma13[,1], hic.ma13[,2])

# FOURTEEN DAYs OF HIC 
index.15<-index.1+14
hic.ma<-cbind(hic.ma, matrix(df.q3$states[index.15], nrow=length(df.q3$states[index.15])))
#
hic.ma14<-hic.ma[hic.ma[,15] != 1, ]
hic.ma14<-hic.ma14[hic.ma14[,14] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,13] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,12] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,11] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,10] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,9] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,8] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,7] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,6] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,5] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,4] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,3] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,2] == 1, ]
hic.ma14<-hic.ma14[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13)]
hic.ma14[complete.cases(hic.ma14), ]
q3tm.14<-table(hic.ma14[,1], hic.ma14[,2])

# FIFTEEN DAYs OF HIC 
index.16<-index.1+15
hic.ma<-cbind(hic.ma, matrix(df.q3$states[index.16], nrow=length(df.q3$states[index.16])))
#
hic.ma15<-hic.ma[hic.ma[,16] != 1, ]
hic.ma15<-hic.ma15[hic.ma15[,15] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,14] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,13] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,12] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,11] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,10] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,9] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,8] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,7] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,6] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,5] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,4] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,3] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,2] == 1, ]
hic.ma15<-hic.ma15[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
hic.ma15[complete.cases(hic.ma15), ]
q3tm.15<-table(hic.ma15[,1], hic.ma15[,2])

# SIXTEEN DAYs OF HIC 
index.17<-index.1+16
hic.ma<-cbind(hic.ma, matrix(df.q3$states[index.17], nrow=length(df.q3$states[index.17])))
#
hic.ma16<-hic.ma[hic.ma[,17] != 1, ]
hic.ma16<-hic.ma16[hic.ma16[,16] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,15] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,14] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,13] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,12] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,11] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,10] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,9] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,8] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,7] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,6] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,5] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,4] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,3] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,2] == 1, ]
hic.ma16<-hic.ma16[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
hic.ma16[complete.cases(hic.ma16), ]
q3tm.16<-table(hic.ma16[,1], hic.ma16[,2])

#------------------------------------------------------------------------------
# With more data we can consider more days but for now we have less than 10 
# patients at this point 
# SEVENTEEN DAYs OF HIC 
# EIGTHTEEN DAYs OF HIC 
# NINETEEN DAYs OF HIC 
# TWENTY DAYs OF HIC 
# TWENTYONE DAYs OF HIC 
# TWENTYTWO DAYs OF HIC 
# TWENTYTHREE DAYs OF HIC 
# TWENTYFOUR DAYs OF HIC 
# TWENTYFIVE DAYs OF HIC 
# TWENTYSIX DAYs OF HIC 
# TWENTYSEVEN DAYs OF HIC 
# TWENTYEIGHT DAYs OF HIC 
# TWENTYNINE DAYs OF HIC 
# THIRTY DAYs OF HIC 
#-------------------------------------------------------------------------------

rm(index.1)
rm(index.2)
rm(index.3)
rm(index.4)
rm(index.5)
rm(index.6)
rm(index.7)
rm(index.8)
rm(index.9)
rm(index.10)
rm(index.11)
rm(index.12)
rm(index.13)
rm(index.14)
rm(index.15)
rm(index.16)
rm(index.17)
rm(hic.ma)
rm(hic.ma2)
rm(hic.ma3)
rm(hic.ma4)
rm(hic.ma5)
rm(hic.ma6)
rm(hic.ma7)
rm(hic.ma8)
rm(hic.ma9)
rm(hic.ma10)
rm(hic.ma11)
rm(hic.ma12)
rm(hic.ma13)
rm(hic.ma14)
rm(hic.ma15)
rm(hic.ma16)

##                                                                            ##   
################################################################################
################################################################################
# Quartile IV transition matrix for treatment options                          #
################################################################################
################################################################################
##                                                                            ##

# ONE DAY OF HIC 

#find index of 1s then say if the next index is not 1 keep that row 
index.1<-which(df.q4$states %in% c(1)) 
index.2<-index.1+1
hic.ma<- matrix(c(df.q4$states[index.1],df.q4$states[index.1+1]), nrow=length(index.1), ncol=2)
hic.df<-as.data.frame(hic.ma) %>% filter(V1!=V2)
q4tm.1<-table(hic.df$V1, hic.df$V2)

# TWO DAYs OF HIC 
index.3<-index.1+2
hic.ma<-cbind(hic.ma, matrix(df.q4$states[index.3], nrow=length(df.q4$states[index.3])))
#
hic.ma2<-hic.ma[hic.ma[,3] != 1, ]
hic.ma2<-hic.ma2[hic.ma2[,2] == 1, ]
hic.ma2<-hic.ma2[,-1]
q4tm.2<-table(hic.ma2[,1], hic.ma2[,2])

# THREE DAYs OF HIC 
index.4<-index.1+3
hic.ma<-cbind(hic.ma, matrix(df.q4$states[index.4], nrow=length(df.q4$states[index.4])))
#
hic.ma3<-hic.ma[hic.ma[,4] != 1, ]
hic.ma3<-hic.ma3[hic.ma3[,3] == 1, ]
hic.ma3<-hic.ma3[hic.ma3[,2] == 1, ]
hic.ma3<-hic.ma3[,-c(1,2)]
hic.ma3[complete.cases(hic.ma3), ]
q4tm.3<-table(hic.ma3[,1], hic.ma3[,2])

# FOUR DAYs OF HIC 
index.5<-index.1+4
hic.ma<-cbind(hic.ma, matrix(df.q4$states[index.5], nrow=length(df.q4$states[index.5])))
#
hic.ma4<-hic.ma[hic.ma[,5] != 1, ]
hic.ma4<-hic.ma4[hic.ma4[,4] == 1, ]
hic.ma4<-hic.ma4[hic.ma4[,3] == 1, ]
hic.ma4<-hic.ma4[hic.ma4[,2] == 1, ]
hic.ma4<-hic.ma4[,-c(1,2,3)]
hic.ma4[complete.cases(hic.ma4), ]
q4tm.4<-table(hic.ma4[,1], hic.ma4[,2])

# FIVE DAYs OF HIC
index.6<-index.1+5
hic.ma<-cbind(hic.ma, matrix(df.q4$states[index.6], nrow=length(df.q4$states[index.6])))
#
hic.ma5<-hic.ma[hic.ma[,6] != 1, ]
hic.ma5<-hic.ma5[hic.ma5[,5] == 1, ]
hic.ma5<-hic.ma5[hic.ma5[,4] == 1, ]
hic.ma5<-hic.ma5[hic.ma5[,3] == 1, ]
hic.ma5<-hic.ma5[hic.ma5[,2] == 1, ]
hic.ma5<-hic.ma5[,-c(1,2,3,4)]
hic.ma5[complete.cases(hic.ma5), ]
q4tm.5<-table(hic.ma5[,1], hic.ma5[,2])

# SIX DAYs OF HIC 
index.7<-index.1+6
hic.ma<-cbind(hic.ma, matrix(df.q4$states[index.7], nrow=length(df.q4$states[index.7])))
#
hic.ma6<-hic.ma[hic.ma[,7] != 1, ]
hic.ma6<-hic.ma6[hic.ma6[,6] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,5] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,4] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,3] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,2] == 1, ]
hic.ma6<-hic.ma6[,-c(1,2,3,4,5)]
hic.ma6[complete.cases(hic.ma6), ]
q4tm.6<-table(hic.ma6[,1], hic.ma6[,2])

# SEVEN DAYs OF HIC 
index.8<-index.1+7
hic.ma<-cbind(hic.ma, matrix(df.q4$states[index.8], nrow=length(df.q4$states[index.8])))
#
hic.ma7<-hic.ma[hic.ma[,8] != 1, ]
hic.ma7<-hic.ma7[hic.ma7[,7] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,6] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,5] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,4] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,3] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,2] == 1, ]
hic.ma7<-hic.ma7[,-c(1,2,3,4,5,6)]
hic.ma7[complete.cases(hic.ma7), ]
q4tm.7<-table(hic.ma7[,1], hic.ma7[,2])

# EIGHT DAYs OF HIC 
index.9<-index.1+8
hic.ma<-cbind(hic.ma, matrix(df.q4$states[index.9], nrow=length(df.q4$states[index.9])))
#
hic.ma8<-hic.ma[hic.ma[,9] != 1, ]
hic.ma8<-hic.ma8[hic.ma8[,8] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,7] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,6] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,5] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,4] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,3] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,2] == 1, ]
hic.ma8<-hic.ma8[,-c(1,2,3,4,5,6,7)]
hic.ma8[complete.cases(hic.ma8), ]
q4tm.8<-table(hic.ma8[,1], hic.ma8[,2])

# NINE DAYs OF HIC 
index.10<-index.1+9
hic.ma<-cbind(hic.ma, matrix(df.q4$states[index.10], nrow=length(df.q4$states[index.10])))
#
hic.ma9<-hic.ma[hic.ma[,10] != 1, ]
hic.ma9<-hic.ma9[hic.ma9[,9] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,8] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,7] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,6] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,5] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,4] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,3] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,2] == 1, ]
hic.ma9<-hic.ma9[,-c(1,2,3,4,5,6,7,8)]
hic.ma9[complete.cases(hic.ma9), ]
q4tm.9<-table(hic.ma9[,1], hic.ma9[,2])

# TEN DAYs OF HIC 
index.11<-index.1+10
hic.ma<-cbind(hic.ma, matrix(df.q4$states[index.11], nrow=length(df.q4$states[index.11])))
#
hic.ma10<-hic.ma[hic.ma[,11] != 1, ]
hic.ma10<-hic.ma10[hic.ma10[,10] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,9] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,8] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,7] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,6] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,5] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,4] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,3] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,2] == 1, ]
hic.ma10<-hic.ma10[,-c(1,2,3,4,5,6,7,8,9)]
hic.ma10[complete.cases(hic.ma10), ]
q4tm.10<-table(hic.ma10[,1], hic.ma10[,2])

# ELEVEN DAYs OF HIC 
index.12<-index.1+11
hic.ma<-cbind(hic.ma, matrix(df.q4$states[index.12], nrow=length(df.q4$states[index.12])))
#
hic.ma11<-hic.ma[hic.ma[,12] != 1, ]
hic.ma11<-hic.ma11[hic.ma11[,11] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,10] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,9] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,8] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,7] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,6] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,5] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,4] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,3] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,2] == 1, ]
hic.ma11<-hic.ma11[,-c(1,2,3,4,5,6,7,8,9,10)]
hic.ma11[complete.cases(hic.ma11), ]
q4tm.11<-table(hic.ma11[,1], hic.ma11[,2])

# TWELVE DAYs OF HIC 
index.13<-index.1+12
hic.ma<-cbind(hic.ma, matrix(df.q4$states[index.13], nrow=length(df.q4$states[index.13])))
#
hic.ma12<-hic.ma[hic.ma[,13] != 1, ]
hic.ma12<-hic.ma12[hic.ma12[,12] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,11] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,10] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,9] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,8] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,7] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,6] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,5] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,4] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,3] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,2] == 1, ]
hic.ma12<-hic.ma12[,-c(1,2,3,4,5,6,7,8,9,10,11)]
hic.ma12[complete.cases(hic.ma12), ]
q4tm.12<-table(hic.ma12[,1], hic.ma12[,2])

# THIRTEEN DAYs OF HIC 
index.14<-index.1+13
hic.ma<-cbind(hic.ma, matrix(df.q4$states[index.14], nrow=length(df.q4$states[index.14])))
#
hic.ma13<-hic.ma[hic.ma[,14] != 1, ]
hic.ma13<-hic.ma13[hic.ma13[,13] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,12] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,11] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,10] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,9] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,8] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,7] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,6] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,5] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,4] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,3] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,2] == 1, ]
hic.ma13<-hic.ma13[,-c(1,2,3,4,5,6,7,8,9,10,11,12)]
hic.ma13[complete.cases(hic.ma13), ]
q4tm.13<-table(hic.ma13[,1], hic.ma13[,2])

# FOURTEEN DAYs OF HIC 
index.15<-index.1+14
hic.ma<-cbind(hic.ma, matrix(df.q4$states[index.15], nrow=length(df.q4$states[index.15])))
#
hic.ma14<-hic.ma[hic.ma[,15] != 1, ]
hic.ma14<-hic.ma14[hic.ma14[,14] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,13] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,12] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,11] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,10] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,9] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,8] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,7] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,6] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,5] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,4] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,3] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,2] == 1, ]
hic.ma14<-hic.ma14[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13)]
hic.ma14[complete.cases(hic.ma14), ]
q4tm.14<-table(hic.ma14[,1], hic.ma14[,2])

# FIFTEEN DAYs OF HIC 
index.16<-index.1+15
hic.ma<-cbind(hic.ma, matrix(df.q4$states[index.16], nrow=length(df.q4$states[index.16])))
#
hic.ma15<-hic.ma[hic.ma[,16] != 1, ]
hic.ma15<-hic.ma15[hic.ma15[,15] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,14] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,13] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,12] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,11] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,10] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,9] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,8] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,7] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,6] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,5] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,4] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,3] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,2] == 1, ]
hic.ma15<-hic.ma15[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
hic.ma15[complete.cases(hic.ma15), ]
q4tm.15<-table(hic.ma15[,1], hic.ma15[,2])

# SIXTEEN DAYs OF HIC 
index.17<-index.1+16
hic.ma<-cbind(hic.ma, matrix(df.q4$states[index.17], nrow=length(df.q4$states[index.17])))
#
hic.ma16<-hic.ma[hic.ma[,17] != 1, ]
hic.ma16<-hic.ma16[hic.ma16[,16] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,15] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,14] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,13] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,12] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,11] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,10] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,9] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,8] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,7] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,6] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,5] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,4] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,3] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,2] == 1, ]
hic.ma16<-hic.ma16[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
hic.ma16[complete.cases(hic.ma16), ]
q4tm.16<-table(hic.ma16[,1], hic.ma16[,2])

#------------------------------------------------------------------------------
# With more data we can consider more days but for now we have less than 10 
# patients at this point 
# SEVENTEEN DAYs OF HIC 
# EIGTHTEEN DAYs OF HIC 
# NINETEEN DAYs OF HIC 
# TWENTY DAYs OF HIC 
# TWENTYONE DAYs OF HIC 
# TWENTYTWO DAYs OF HIC 
# TWENTYTHREE DAYs OF HIC 
# TWENTYFOUR DAYs OF HIC 
# TWENTYFIVE DAYs OF HIC 
# TWENTYSIX DAYs OF HIC 
# TWENTYSEVEN DAYs OF HIC 
# TWENTYEIGHT DAYs OF HIC 
# TWENTYNINE DAYs OF HIC 
# THIRTY DAYs OF HIC 
#-------------------------------------------------------------------------------

rm(index.1)
rm(index.2)
rm(index.3)
rm(index.4)
rm(index.5)
rm(index.6)
rm(index.7)
rm(index.8)
rm(index.9)
rm(index.10)
rm(index.11)
rm(index.12)
rm(index.13)
rm(index.14)
rm(index.15)
rm(index.16)
rm(index.17)
rm(hic.ma)
rm(hic.ma2)
rm(hic.ma3)
rm(hic.ma4)
rm(hic.ma5)
rm(hic.ma6)
rm(hic.ma7)
rm(hic.ma8)
rm(hic.ma9)
rm(hic.ma10)
rm(hic.ma11)
rm(hic.ma12)
rm(hic.ma13)
rm(hic.ma14)
rm(hic.ma15)
rm(hic.ma16)


# Given that there is not a lot of data in the quartiles for the very severe
# and at presetn we are not using time varying severyity in the analysis 
# we coallate the quartiles into halves 

rm(df.q1)
rm(df.q2)
rm(df.q3)
rm(df.q4)

#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#

#                                                                              #
################################################################################
# TRANSITION MATRIX ############################################################
################################################################################
#                                                                              #

# QUARTILE I
q1tmA.1<-tm.1/rowSums(tm.1)
q1tmA.2<-tm.2/rowSums(tm.2)
q1tmA.3<-tm.3/rowSums(tm.3)
q1tmA.4<-tm.4/rowSums(tm.4)
q1tmA.5<-tm.5/rowSums(tm.5)
q1tmA.6<-tm.6/rowSums(tm.6)
q1tmA.7<-tm.7/rowSums(tm.7)
q1tmA.8<-tm.8/rowSums(tm.8)
q1tmA.9<-tm.9/rowSums(tm.9)
q1tmA.10<-tm.10/rowSums(tm.10)
q1tmA.11<-tm.11/rowSums(tm.11)
q1tmA.12<-tm.12/rowSums(tm.12)
q1tmA.13<-tm.13/rowSums(tm.13)
q1tmA.14<-tm.14/rowSums(tm.14)
q1tmA.15<-tm.15/rowSums(tm.15)

# QUARTILE II
q2tmA.1<-q2tm.1/rowSums(q2tm.1)
q2tmA.2<-q2tm.2/rowSums(q2tm.2)
q2tmA.3<-q2tm.3/rowSums(q2tm.3)
q2tmA.4<-q2tm.4/rowSums(q2tm.4)
q2tmA.5<-q2tm.5/rowSums(q2tm.5)
q2tmA.6<-q2tm.6/rowSums(q2tm.6)
q2tmA.7<-q2tm.7/rowSums(q2tm.7)
q2tmA.8<-q2tm.8/rowSums(q2tm.8)
q2tmA.9<-q2tm.9/rowSums(q2tm.9)
q2tmA.10<-q2tm.10/rowSums(q2tm.10)
q2tmA.11<-q2tm.11/rowSums(q2tm.11)
q2tmA.12<-q2tm.12/rowSums(q2tm.12)
q2tmA.13<-q2tm.13/rowSums(q2tm.13)
q2tmA.14<-q2tm.14/rowSums(q2tm.14)
q2tmA.15<-q2tm.15/rowSums(q2tm.15)

# QUARTILE III
q3tmA.1<-q3tm.1/rowSums(q3tm.1)
q3tmA.2<-q3tm.2/rowSums(q3tm.2)
q3tmA.3<-q3tm.3/rowSums(q3tm.3)
q3tmA.4<-q3tm.4/rowSums(q3tm.4)
q3tmA.5<-q3tm.5/rowSums(q3tm.5)
q3tmA.6<-q3tm.6/rowSums(q3tm.6)
q3tmA.7<-q3tm.7/rowSums(q3tm.7)
q3tmA.8<-q3tm.8/rowSums(q3tm.8)
q3tmA.9<-q3tm.9/rowSums(q3tm.9)
q3tmA.10<-q3tm.10/rowSums(q3tm.10)
q3tmA.11<-q3tm.11/rowSums(q3tm.11)
q3tmA.12<-q3tm.12/rowSums(q3tm.12)
q3tmA.13<-q3tm.13/rowSums(q3tm.13)
q3tmA.14<-q3tm.14/rowSums(q3tm.14)
q3tmA.15<-q3tm.15/rowSums(q3tm.15)

# QUARTILE IV
q4tmA.1<-q4tm.1/rowSums(q4tm.1)
q4tmA.2<-q4tm.2/rowSums(q4tm.2)
q4tmA.3<-q4tm.3/rowSums(q4tm.3)
q4tmA.4<-q4tm.4/rowSums(q4tm.4)
q4tmA.5<-q4tm.5/rowSums(q4tm.5)
q4tmA.6<-q4tm.6/rowSums(q4tm.6)
q4tmA.7<-q4tm.7/rowSums(q4tm.7)
q4tmA.8<-q4tm.8/rowSums(q4tm.8)
q4tmA.9<-q4tm.9/rowSums(q4tm.9)
q4tmA.10<-q4tm.10/rowSums(q4tm.10)
q4tmA.11<-q4tm.11/rowSums(q4tm.11)
q4tmA.12<-q4tm.12/rowSums(q4tm.12)
q4tmA.13<-q4tm.13/rowSums(q4tm.13)
q4tmA.14<-q4tm.14/rowSums(q4tm.14)
q4tmA.15<-q4tm.15/rowSums(q4tm.15)

rm(tm.1)
rm(tm.2)
rm(tm.3)
rm(tm.4)
rm(tm.5)
rm(tm.6)
rm(tm.7)
rm(tm.8)
rm(tm.9)
rm(tm.10)
rm(tm.11)
rm(tm.12)
rm(tm.13)
rm(tm.14)
rm(tm.15)
rm(tm.16)

rm(q2tm.1)
rm(q2tm.2)
rm(q2tm.3)
rm(q2tm.4)
rm(q2tm.5)
rm(q2tm.6)
rm(q2tm.7)
rm(q2tm.8)
rm(q2tm.9)
rm(q2tm.10)
rm(q2tm.11)
rm(q2tm.12)
rm(q2tm.13)
rm(q2tm.14)
rm(q2tm.15)
rm(q2tm.16)

rm(q3tm.1)
rm(q3tm.2)
rm(q3tm.3)
rm(q3tm.4)
rm(q3tm.5)
rm(q3tm.6)
rm(q3tm.7)
rm(q3tm.8)
rm(q3tm.9)
rm(q3tm.10)
rm(q3tm.11)
rm(q3tm.12)
rm(q3tm.13)
rm(q3tm.14)
rm(q3tm.15)
rm(q3tm.16)

rm(q4tm.1)
rm(q4tm.2)
rm(q4tm.3)
rm(q4tm.4)
rm(q4tm.5)
rm(q4tm.6)
rm(q4tm.7)
rm(q4tm.8)
rm(q4tm.9)
rm(q4tm.10)
rm(q4tm.11)
rm(q4tm.12)
rm(q4tm.13)
rm(q4tm.14)
rm(q4tm.15)
rm(q4tm.16)

#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#

################################################################################
## MARKOV OBJECT ###############################################################
################################################################################

# Q1 object

test<-as.matrix.data.frame(q1tmA.1)

MOq1.1<-new("markovchain",transitionMatrix=as.matrix.data.frame(q1tmA.1), name="MarkovChain Q1")




#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#//////////////////////////////////////////////////////////////////////////////#

################################################################################
# Halved analysis                                                              #
################################################################################
# Divide cohort based on groups and demographics ###############################
################################################################################

# Obtain keys for sofa groups
sofa<-df %>% filter(icudayseq_asc == 0)

# Create df keys for each quantile sofa severity group
key.low<-sofa %>% filter(sofa_group %in% c("Q1","Q2")) %>% select(icustay_id)
key.high<-sofa %>% filter(sofa_group %in% c("Q3","Q4")) %>% select(icustay_id)

# Create df for each quantile sofa severity group
df.low<-semi_join(df, key.low)
df.high<-semi_join(df, key.high)


# Drop keys and sofa table
rm(key.low)
rm(key.high)
rm(sofa)

# Compute demographics for each quantile sofa group

#Q1#########################
table(df.low$death_flag)
length(unique(df.low$icustay_id))
#Q2#########################
table(df.high$death_flag)
length(unique(df.high$icustay_id))


# How many days on average did they spend in each unit

#Q1#########################
table(df.low$states)[1:3]
sum(table(df.low$states)[1:3]) # total number of days in ICU
sum(table(df.low$states)) # total number of days passed
#Q2#########################
table(df.high$states)[1:3]
sum(table(df.high$states)[1:3])# total number of days in ICU
sum(table(df.high$states)) # total number of days passed

# Obtain the last day metrics for each quantile sofa group
df.low.lastrow<-df.low %>% group_by(icustay_id) %>% filter(icudayseq_asc %in% max(icudayseq_asc))
df.high.lastrow<-df.high %>% group_by(icustay_id) %>% filter(icudayseq_asc %in% max(icudayseq_asc))

# Last row statistics for each quantile sofa group

#Q1#########################
summary(df.low.lastrow$icudayseq_asc)
summary(df.low.lastrow$sofa_last)
summary(df.low.lastrow$admission_age)
table(df.low.lastrow$gender)
(table(df.low.lastrow$gender)/length(unique(df.low$icustay_id)))*100
#Q2#########################
summary(df.high.lastrow$icudayseq_asc)
summary(df.high.lastrow$sofa_last)
summary(df.high.lastrow$admission_age)
table(df.high.lastrow$gender)
(table(df.high.lastrow$gender)/length(unique(df.high$icustay_id)))*100

rm(df.low.lastrow)
rm(df.high.lastrow)

#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#


##                                                                            ##   
################################################################################
################################################################################
# LOW SEVERITY transition matrix for treatment options                         #
################################################################################
################################################################################
##                                                                            ##

# ONE DAY OF HIC 

#find index of 1s then say if the next index is not 1 keep that row 
index.1<-which(df.low$states %in% c(1)) 
index.2<-index.1+1
hic.ma<- matrix(c(df.low$states[index.1],df.low$states[index.1+1]), nrow=length(index.1), ncol=2)
hic.df<-as.data.frame(hic.ma) %>% filter(V1!=V2)
lowtm.1<-table(hic.df$V1, hic.df$V2)

# TWO DAYs OF HIC 
index.3<-index.1+2
hic.ma<-cbind(hic.ma, matrix(df.low$states[index.3], nrow=length(df.low$states[index.3])))
#
hic.ma2<-hic.ma[hic.ma[,3] != 1, ]
hic.ma2<-hic.ma2[hic.ma2[,2] == 1, ]
hic.ma2<-hic.ma2[,-1]
lowtm.2<-table(hic.ma2[,1], hic.ma2[,2])

# THREE DAYs OF HIC 
index.4<-index.1+3
hic.ma<-cbind(hic.ma, matrix(df.low$states[index.4], nrow=length(df.low$states[index.4])))
#
hic.ma3<-hic.ma[hic.ma[,4] != 1, ]
hic.ma3<-hic.ma3[hic.ma3[,3] == 1, ]
hic.ma3<-hic.ma3[hic.ma3[,2] == 1, ]
hic.ma3<-hic.ma3[,-c(1,2)]
hic.ma3[complete.cases(hic.ma3), ]
lowtm.3<-table(hic.ma3[,1], hic.ma3[,2])

# FOUR DAYs OF HIC 
index.5<-index.1+4
hic.ma<-cbind(hic.ma, matrix(df.low$states[index.5], nrow=length(df.low$states[index.5])))
#
hic.ma4<-hic.ma[hic.ma[,5] != 1, ]
hic.ma4<-hic.ma4[hic.ma4[,4] == 1, ]
hic.ma4<-hic.ma4[hic.ma4[,3] == 1, ]
hic.ma4<-hic.ma4[hic.ma4[,2] == 1, ]
hic.ma4<-hic.ma4[,-c(1,2,3)]
hic.ma4[complete.cases(hic.ma4), ]
lowtm.4<-table(hic.ma4[,1], hic.ma4[,2])

# FIVE DAYs OF HIC
index.6<-index.1+5
hic.ma<-cbind(hic.ma, matrix(df.low$states[index.6], nrow=length(df.low$states[index.6])))
#
hic.ma5<-hic.ma[hic.ma[,6] != 1, ]
hic.ma5<-hic.ma5[hic.ma5[,5] == 1, ]
hic.ma5<-hic.ma5[hic.ma5[,4] == 1, ]
hic.ma5<-hic.ma5[hic.ma5[,3] == 1, ]
hic.ma5<-hic.ma5[hic.ma5[,2] == 1, ]
hic.ma5<-hic.ma5[,-c(1,2,3,4)]
hic.ma5[complete.cases(hic.ma5), ]
lowtm.5<-table(hic.ma5[,1], hic.ma5[,2])

# SIX DAYs OF HIC 
index.7<-index.1+6
hic.ma<-cbind(hic.ma, matrix(df.low$states[index.7], nrow=length(df.low$states[index.7])))
#
hic.ma6<-hic.ma[hic.ma[,7] != 1, ]
hic.ma6<-hic.ma6[hic.ma6[,6] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,5] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,4] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,3] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,2] == 1, ]
hic.ma6<-hic.ma6[,-c(1,2,3,4,5)]
hic.ma6[complete.cases(hic.ma6), ]
lowtm.6<-table(hic.ma6[,1], hic.ma6[,2])

# SEVEN DAYs OF HIC 
index.8<-index.1+7
hic.ma<-cbind(hic.ma, matrix(df.low$states[index.8], nrow=length(df.low$states[index.8])))
#
hic.ma7<-hic.ma[hic.ma[,8] != 1, ]
hic.ma7<-hic.ma7[hic.ma7[,7] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,6] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,5] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,4] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,3] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,2] == 1, ]
hic.ma7<-hic.ma7[,-c(1,2,3,4,5,6)]
hic.ma7[complete.cases(hic.ma7), ]
lowtm.7<-table(hic.ma7[,1], hic.ma7[,2])

# EIGHT DAYs OF HIC 
index.9<-index.1+8
hic.ma<-cbind(hic.ma, matrix(df.low$states[index.9], nrow=length(df.low$states[index.9])))
#
hic.ma8<-hic.ma[hic.ma[,9] != 1, ]
hic.ma8<-hic.ma8[hic.ma8[,8] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,7] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,6] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,5] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,4] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,3] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,2] == 1, ]
hic.ma8<-hic.ma8[,-c(1,2,3,4,5,6,7)]
hic.ma8[complete.cases(hic.ma8), ]
lowtm.8<-table(hic.ma8[,1], hic.ma8[,2])

# NINE DAYs OF HIC 
index.10<-index.1+9
hic.ma<-cbind(hic.ma, matrix(df.low$states[index.10], nrow=length(df.low$states[index.10])))
#
hic.ma9<-hic.ma[hic.ma[,10] != 1, ]
hic.ma9<-hic.ma9[hic.ma9[,9] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,8] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,7] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,6] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,5] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,4] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,3] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,2] == 1, ]
hic.ma9<-hic.ma9[,-c(1,2,3,4,5,6,7,8)]
hic.ma9[complete.cases(hic.ma9), ]
lowtm.9<-table(hic.ma9[,1], hic.ma9[,2])

# TEN DAYs OF HIC 
index.11<-index.1+10
hic.ma<-cbind(hic.ma, matrix(df.low$states[index.11], nrow=length(df.low$states[index.11])))
#
hic.ma10<-hic.ma[hic.ma[,11] != 1, ]
hic.ma10<-hic.ma10[hic.ma10[,10] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,9] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,8] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,7] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,6] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,5] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,4] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,3] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,2] == 1, ]
hic.ma10<-hic.ma10[,-c(1,2,3,4,5,6,7,8,9)]
hic.ma10[complete.cases(hic.ma10), ]
lowtm.10<-table(hic.ma10[,1], hic.ma10[,2])

# ELEVEN DAYs OF HIC 
index.12<-index.1+11
hic.ma<-cbind(hic.ma, matrix(df.low$states[index.12], nrow=length(df.low$states[index.12])))
#
hic.ma11<-hic.ma[hic.ma[,12] != 1, ]
hic.ma11<-hic.ma11[hic.ma11[,11] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,10] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,9] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,8] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,7] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,6] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,5] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,4] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,3] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,2] == 1, ]
hic.ma11<-hic.ma11[,-c(1,2,3,4,5,6,7,8,9,10)]
hic.ma11[complete.cases(hic.ma11), ]
lowtm.11<-table(hic.ma11[,1], hic.ma11[,2])

# TWELVE DAYs OF HIC 
index.13<-index.1+12
hic.ma<-cbind(hic.ma, matrix(df.low$states[index.13], nrow=length(df.low$states[index.13])))
#
hic.ma12<-hic.ma[hic.ma[,13] != 1, ]
hic.ma12<-hic.ma12[hic.ma12[,12] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,11] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,10] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,9] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,8] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,7] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,6] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,5] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,4] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,3] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,2] == 1, ]
hic.ma12<-hic.ma12[,-c(1,2,3,4,5,6,7,8,9,10,11)]
hic.ma12[complete.cases(hic.ma12), ]
lowtm.12<-table(hic.ma12[,1], hic.ma12[,2])

# THIRTEEN DAYs OF HIC 
index.14<-index.1+13
hic.ma<-cbind(hic.ma, matrix(df.low$states[index.14], nrow=length(df.low$states[index.14])))
#
hic.ma13<-hic.ma[hic.ma[,14] != 1, ]
hic.ma13<-hic.ma13[hic.ma13[,13] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,12] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,11] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,10] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,9] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,8] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,7] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,6] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,5] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,4] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,3] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,2] == 1, ]
hic.ma13<-hic.ma13[,-c(1,2,3,4,5,6,7,8,9,10,11,12)]
hic.ma13[complete.cases(hic.ma13), ]
lowtm.13<-table(hic.ma13[,1], hic.ma13[,2])

# FOURTEEN DAYs OF HIC 
index.15<-index.1+14
hic.ma<-cbind(hic.ma, matrix(df.low$states[index.15], nrow=length(df.low$states[index.15])))
#
hic.ma14<-hic.ma[hic.ma[,15] != 1, ]
hic.ma14<-hic.ma14[hic.ma14[,14] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,13] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,12] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,11] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,10] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,9] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,8] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,7] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,6] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,5] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,4] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,3] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,2] == 1, ]
hic.ma14<-hic.ma14[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13)]
hic.ma14[complete.cases(hic.ma14), ]
lowtm.14<-table(hic.ma14[,1], hic.ma14[,2])

# FIFTEEN DAYs OF HIC 
index.16<-index.1+15
hic.ma<-cbind(hic.ma, matrix(df.low$states[index.16], nrow=length(df.low$states[index.16])))
#
hic.ma15<-hic.ma[hic.ma[,16] != 1, ]
hic.ma15<-hic.ma15[hic.ma15[,15] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,14] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,13] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,12] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,11] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,10] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,9] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,8] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,7] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,6] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,5] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,4] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,3] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,2] == 1, ]
hic.ma15<-hic.ma15[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
hic.ma15[complete.cases(hic.ma15), ]
lowtm.15<-table(hic.ma15[,1], hic.ma15[,2])

# SIXTEEN DAYs OF HIC 
index.17<-index.1+16
hic.ma<-cbind(hic.ma, matrix(df.low$states[index.17], nrow=length(df.low$states[index.17])))
#
hic.ma16<-hic.ma[hic.ma[,17] != 1, ]
hic.ma16<-hic.ma16[hic.ma16[,16] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,15] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,14] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,13] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,12] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,11] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,10] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,9] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,8] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,7] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,6] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,5] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,4] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,3] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,2] == 1, ]
hic.ma16<-hic.ma16[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
hic.ma16[complete.cases(hic.ma16), ]
lowtm.16<-table(hic.ma16[,1], hic.ma16[,2])

# SEVENTEEN DAYs OF HIC 
index.18<-index.1+17
hic.ma<-cbind(hic.ma, matrix(df.low$states[index.18], nrow=length(df.low$states[index.18])))
#
hic.ma17<-hic.ma[hic.ma[,18] != 1, ]
hic.ma17<-hic.ma17[hic.ma17[,17] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,16] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,15] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,14] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,13] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,12] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,11] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,10] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,9] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,8] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,7] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,6] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,5] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,4] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,3] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,2] == 1, ]
hic.ma17<-hic.ma17[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
hic.ma17[complete.cases(hic.ma17), ]
lowtm.17<-table(hic.ma17[,1], hic.ma17[,2])


#------------------------------------------------------------------------------
# With more data we can consider more days but for now we have less than 10 
# patients at this point 
# SEVENTEEN DAYs OF HIC 
# EIGTHTEEN DAYs OF HIC 
# NINETEEN DAYs OF HIC 
# TWENTY DAYs OF HIC 
# TWENTYONE DAYs OF HIC 
# TWENTYTWO DAYs OF HIC 
# TWENTYTHREE DAYs OF HIC 
# TWENTYFOUR DAYs OF HIC 
# TWENTYFIVE DAYs OF HIC 
# TWENTYSIX DAYs OF HIC 
# TWENTYSEVEN DAYs OF HIC 
# TWENTYEIGHT DAYs OF HIC 
# TWENTYNINE DAYs OF HIC 
# THIRTY DAYs OF HIC 
#-------------------------------------------------------------------------------

rm(index.1)
rm(index.2)
rm(index.3)
rm(index.4)
rm(index.5)
rm(index.6)
rm(index.7)
rm(index.8)
rm(index.9)
rm(index.10)
rm(index.11)
rm(index.12)
rm(index.13)
rm(index.14)
rm(index.15)
rm(index.16)
rm(index.17)
rm(hic.ma)
rm(hic.ma2)
rm(hic.ma3)
rm(hic.ma4)
rm(hic.ma5)
rm(hic.ma6)
rm(hic.ma7)
rm(hic.ma8)
rm(hic.ma9)
rm(hic.ma10)
rm(hic.ma11)
rm(hic.ma12)
rm(hic.ma13)
rm(hic.ma14)
rm(hic.ma15)
rm(hic.ma16)

##                                                                            ##   
################################################################################
################################################################################
# HIGH SEVERITY transition matrix for treatment options                        #
################################################################################
################################################################################
##                                                                            ##

# ONE DAY OF HIC 

#find index of 1s then say if the next index is not 1 keep that row 
index.1<-which(df.high$states %in% c(1)) 
index.2<-index.1+1
hic.ma<- matrix(c(df.high$states[index.1],df.high$states[index.1+1]), nrow=length(index.1), ncol=2)
hic.df<-as.data.frame(hic.ma) %>% filter(V1!=V2)
hightm.1<-table(hic.df$V1, hic.df$V2)

# TWO DAYs OF HIC 
index.3<-index.1+2
hic.ma<-cbind(hic.ma, matrix(df.high$states[index.3], nrow=length(df.high$states[index.3])))
#
hic.ma2<-hic.ma[hic.ma[,3] != 1, ]
hic.ma2<-hic.ma2[hic.ma2[,2] == 1, ]
hic.ma2<-hic.ma2[,-1]
hightm.2<-table(hic.ma2[,1], hic.ma2[,2])

# THREE DAYs OF HIC 
index.4<-index.1+3
hic.ma<-cbind(hic.ma, matrix(df.high$states[index.4], nrow=length(df.high$states[index.4])))
#
hic.ma3<-hic.ma[hic.ma[,4] != 1, ]
hic.ma3<-hic.ma3[hic.ma3[,3] == 1, ]
hic.ma3<-hic.ma3[hic.ma3[,2] == 1, ]
hic.ma3<-hic.ma3[,-c(1,2)]
hic.ma3[complete.cases(hic.ma3), ]
hightm.3<-table(hic.ma3[,1], hic.ma3[,2])

# FOUR DAYs OF HIC 
index.5<-index.1+4
hic.ma<-cbind(hic.ma, matrix(df.high$states[index.5], nrow=length(df.high$states[index.5])))
#
hic.ma4<-hic.ma[hic.ma[,5] != 1, ]
hic.ma4<-hic.ma4[hic.ma4[,4] == 1, ]
hic.ma4<-hic.ma4[hic.ma4[,3] == 1, ]
hic.ma4<-hic.ma4[hic.ma4[,2] == 1, ]
hic.ma4<-hic.ma4[,-c(1,2,3)]
hic.ma4[complete.cases(hic.ma4), ]
hightm.4<-table(hic.ma4[,1], hic.ma4[,2])

# FIVE DAYs OF HIC
index.6<-index.1+5
hic.ma<-cbind(hic.ma, matrix(df.high$states[index.6], nrow=length(df.high$states[index.6])))
#
hic.ma5<-hic.ma[hic.ma[,6] != 1, ]
hic.ma5<-hic.ma5[hic.ma5[,5] == 1, ]
hic.ma5<-hic.ma5[hic.ma5[,4] == 1, ]
hic.ma5<-hic.ma5[hic.ma5[,3] == 1, ]
hic.ma5<-hic.ma5[hic.ma5[,2] == 1, ]
hic.ma5<-hic.ma5[,-c(1,2,3,4)]
hic.ma5[complete.cases(hic.ma5), ]
hightm.5<-table(hic.ma5[,1], hic.ma5[,2])

# SIX DAYs OF HIC 
index.7<-index.1+6
hic.ma<-cbind(hic.ma, matrix(df.high$states[index.7], nrow=length(df.high$states[index.7])))
#
hic.ma6<-hic.ma[hic.ma[,7] != 1, ]
hic.ma6<-hic.ma6[hic.ma6[,6] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,5] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,4] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,3] == 1, ]
hic.ma6<-hic.ma6[hic.ma6[,2] == 1, ]
hic.ma6<-hic.ma6[,-c(1,2,3,4,5)]
hic.ma6[complete.cases(hic.ma6), ]
hightm.6<-table(hic.ma6[,1], hic.ma6[,2])

# SEVEN DAYs OF HIC 
index.8<-index.1+7
hic.ma<-cbind(hic.ma, matrix(df.high$states[index.8], nrow=length(df.high$states[index.8])))
#
hic.ma7<-hic.ma[hic.ma[,8] != 1, ]
hic.ma7<-hic.ma7[hic.ma7[,7] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,6] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,5] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,4] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,3] == 1, ]
hic.ma7<-hic.ma7[hic.ma7[,2] == 1, ]
hic.ma7<-hic.ma7[,-c(1,2,3,4,5,6)]
hic.ma7[complete.cases(hic.ma7), ]
hightm.7<-table(hic.ma7[,1], hic.ma7[,2])

# EIGHT DAYs OF HIC 
index.9<-index.1+8
hic.ma<-cbind(hic.ma, matrix(df.high$states[index.9], nrow=length(df.high$states[index.9])))
#
hic.ma8<-hic.ma[hic.ma[,9] != 1, ]
hic.ma8<-hic.ma8[hic.ma8[,8] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,7] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,6] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,5] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,4] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,3] == 1, ]
hic.ma8<-hic.ma8[hic.ma8[,2] == 1, ]
hic.ma8<-hic.ma8[,-c(1,2,3,4,5,6,7)]
hic.ma8[complete.cases(hic.ma8), ]
hightm.8<-table(hic.ma8[,1], hic.ma8[,2])

# NINE DAYs OF HIC 
index.10<-index.1+9
hic.ma<-cbind(hic.ma, matrix(df.high$states[index.10], nrow=length(df.high$states[index.10])))
#
hic.ma9<-hic.ma[hic.ma[,10] != 1, ]
hic.ma9<-hic.ma9[hic.ma9[,9] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,8] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,7] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,6] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,5] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,4] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,3] == 1, ]
hic.ma9<-hic.ma9[hic.ma9[,2] == 1, ]
hic.ma9<-hic.ma9[,-c(1,2,3,4,5,6,7,8)]
hic.ma9[complete.cases(hic.ma9), ]
hightm.9<-table(hic.ma9[,1], hic.ma9[,2])

# TEN DAYs OF HIC 
index.11<-index.1+10
hic.ma<-cbind(hic.ma, matrix(df.high$states[index.11], nrow=length(df.high$states[index.11])))
#
hic.ma10<-hic.ma[hic.ma[,11] != 1, ]
hic.ma10<-hic.ma10[hic.ma10[,10] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,9] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,8] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,7] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,6] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,5] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,4] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,3] == 1, ]
hic.ma10<-hic.ma10[hic.ma10[,2] == 1, ]
hic.ma10<-hic.ma10[,-c(1,2,3,4,5,6,7,8,9)]
hic.ma10[complete.cases(hic.ma10), ]
hightm.10<-table(hic.ma10[,1], hic.ma10[,2])

# ELEVEN DAYs OF HIC 
index.12<-index.1+11
hic.ma<-cbind(hic.ma, matrix(df.high$states[index.12], nrow=length(df.high$states[index.12])))
#
hic.ma11<-hic.ma[hic.ma[,12] != 1, ]
hic.ma11<-hic.ma11[hic.ma11[,11] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,10] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,9] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,8] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,7] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,6] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,5] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,4] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,3] == 1, ]
hic.ma11<-hic.ma11[hic.ma11[,2] == 1, ]
hic.ma11<-hic.ma11[,-c(1,2,3,4,5,6,7,8,9,10)]
hic.ma11[complete.cases(hic.ma11), ]
hightm.11<-table(hic.ma11[,1], hic.ma11[,2])

# TWELVE DAYs OF HIC 
index.13<-index.1+12
hic.ma<-cbind(hic.ma, matrix(df.high$states[index.13], nrow=length(df.high$states[index.13])))
#
hic.ma12<-hic.ma[hic.ma[,13] != 1, ]
hic.ma12<-hic.ma12[hic.ma12[,12] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,11] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,10] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,9] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,8] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,7] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,6] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,5] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,4] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,3] == 1, ]
hic.ma12<-hic.ma12[hic.ma12[,2] == 1, ]
hic.ma12<-hic.ma12[,-c(1,2,3,4,5,6,7,8,9,10,11)]
hic.ma12[complete.cases(hic.ma12), ]
hightm.12<-table(hic.ma12[,1], hic.ma12[,2])

# THIRTEEN DAYs OF HIC 
index.14<-index.1+13
hic.ma<-cbind(hic.ma, matrix(df.high$states[index.14], nrow=length(df.high$states[index.14])))
#
hic.ma13<-hic.ma[hic.ma[,14] != 1, ]
hic.ma13<-hic.ma13[hic.ma13[,13] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,12] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,11] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,10] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,9] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,8] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,7] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,6] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,5] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,4] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,3] == 1, ]
hic.ma13<-hic.ma13[hic.ma13[,2] == 1, ]
hic.ma13<-hic.ma13[,-c(1,2,3,4,5,6,7,8,9,10,11,12)]
hic.ma13[complete.cases(hic.ma13), ]
hightm.13<-table(hic.ma13[,1], hic.ma13[,2])

# FOURTEEN DAYs OF HIC 
index.15<-index.1+14
hic.ma<-cbind(hic.ma, matrix(df.high$states[index.15], nrow=length(df.high$states[index.15])))
#
hic.ma14<-hic.ma[hic.ma[,15] != 1, ]
hic.ma14<-hic.ma14[hic.ma14[,14] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,13] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,12] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,11] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,10] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,9] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,8] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,7] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,6] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,5] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,4] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,3] == 1, ]
hic.ma14<-hic.ma14[hic.ma14[,2] == 1, ]
hic.ma14<-hic.ma14[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13)]
hic.ma14[complete.cases(hic.ma14), ]
hightm.14<-table(hic.ma14[,1], hic.ma14[,2])

# FIFTEEN DAYs OF HIC 
index.16<-index.1+15
hic.ma<-cbind(hic.ma, matrix(df.high$states[index.16], nrow=length(df.high$states[index.16])))
#
hic.ma15<-hic.ma[hic.ma[,16] != 1, ]
hic.ma15<-hic.ma15[hic.ma15[,15] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,14] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,13] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,12] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,11] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,10] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,9] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,8] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,7] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,6] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,5] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,4] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,3] == 1, ]
hic.ma15<-hic.ma15[hic.ma15[,2] == 1, ]
hic.ma15<-hic.ma15[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
hic.ma15[complete.cases(hic.ma15), ]
hightm.15<-table(hic.ma15[,1], hic.ma15[,2])

# SIXTEEN DAYs OF HIC 
index.17<-index.1+16
hic.ma<-cbind(hic.ma, matrix(df.high$states[index.17], nrow=length(df.high$states[index.17])))
#
hic.ma16<-hic.ma[hic.ma[,17] != 1, ]
hic.ma16<-hic.ma16[hic.ma16[,16] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,15] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,14] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,13] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,12] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,11] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,10] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,9] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,8] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,7] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,6] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,5] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,4] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,3] == 1, ]
hic.ma16<-hic.ma16[hic.ma16[,2] == 1, ]
hic.ma16<-hic.ma16[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
hic.ma16[complete.cases(hic.ma16), ]
hightm.16<-table(hic.ma16[,1], hic.ma16[,2])

# SEVENTEEN DAYs OF HIC 
index.18<-index.1+17
hic.ma<-cbind(hic.ma, matrix(df.high$states[index.18], nrow=length(df.high$states[index.18])))
#
hic.ma17<-hic.ma[hic.ma[,18] != 1, ]
hic.ma17<-hic.ma17[hic.ma17[,17] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,16] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,15] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,14] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,13] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,12] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,11] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,10] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,9] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,8] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,7] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,6] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,5] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,4] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,3] == 1, ]
hic.ma17<-hic.ma17[hic.ma17[,2] == 1, ]
hic.ma17<-hic.ma17[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
hic.ma17[complete.cases(hic.ma17), ]
hightm.17<-table(hic.ma17[,1], hic.ma17[,2])


#------------------------------------------------------------------------------
# With more data we can consider more days but for now we have less than 10 
# patients at this point 
# EIGTHTEEN DAYs OF HIC 
# NINETEEN DAYs OF HIC 
# TWENTY DAYs OF HIC 
# TWENTYONE DAYs OF HIC 
# TWENTYTWO DAYs OF HIC 
# TWENTYTHREE DAYs OF HIC 
# TWENTYFOUR DAYs OF HIC 
# TWENTYFIVE DAYs OF HIC 
# TWENTYSIX DAYs OF HIC 
# TWENTYSEVEN DAYs OF HIC 
# TWENTYEIGHT DAYs OF HIC 
# TWENTYNINE DAYs OF HIC 
# THIRTY DAYs OF HIC 
#-------------------------------------------------------------------------------

rm(index.1)
rm(index.2)
rm(index.3)
rm(index.4)
rm(index.5)
rm(index.6)
rm(index.7)
rm(index.8)
rm(index.9)
rm(index.10)
rm(index.11)
rm(index.12)
rm(index.13)
rm(index.14)
rm(index.15)
rm(index.16)
rm(index.17)
rm(hic.ma)
rm(hic.ma2)
rm(hic.ma3)
rm(hic.ma4)
rm(hic.ma5)
rm(hic.ma6)
rm(hic.ma7)
rm(hic.ma8)
rm(hic.ma9)
rm(hic.ma10)
rm(hic.ma11)
rm(hic.ma12)
rm(hic.ma13)
rm(hic.ma14)
rm(hic.ma15)
rm(hic.ma16)


# Given that there is not a lot of data in the quartiles for the very severe
# and at presetn we are not using time varying severyity in the analysis 
# we coallate the quartiles into halves 

rm(df.q1)
rm(df.q2)
rm(df.q3)
rm(df.q4)

#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#




################################################################################
## TRANSITION MATRIX ###########################################################
################################################################################
# Create a lead and a lag column for states
# this is to prepare the dataframe for the Multi State Model
# I create a lead and lag state to create the tstart and tstop columns
# I also converted NA in these columns to 99 for the ifelse statesments to work
df.low<-df.low %>% 
  mutate(lag_states= lag(states),
         lead_states= lead(states), 
         lag_states = replace_na(lag_states, 99),
         lead_states = replace_na(lead_states, 99),
         tstart= ifelse(states != lag_states, icudayseq_asc, NA),
         tstop= ifelse(states != lead_states, icudayseq_asc, NA))

df.high<-df.high %>% 
  mutate(lag_states= lag(states),
         lead_states= lead(states), 
         lag_states = replace_na(lag_states, 99),
         lead_states = replace_na(lead_states, 99),
         tstart= ifelse(states != lag_states, icudayseq_asc, NA),
         tstop= ifelse(states != lead_states, icudayseq_asc, NA))


# TRANSITION MATRIX LOW

df.low<- df.low %>% mutate(seq= ifelse(states %in% c(3,4), NA, lead_states),
                         seq= ifelse(seq %in% NA, states, lead_states)) 

dflow.loop<-c()
tm.list<-c()
tma.list<-c()
low.tma<-c()
for (i in 1:30){
  dflow.loop[i]<-list(df.low %>% filter(icudayseq_asc %in% c(0:i-1)))
  tm.list[i]<-list(table(dflow.loop[[i]][["states"]], dflow.loop[[i]][["seq"]]))
  tma.list[i]<-list(tm.list[[i]]/rowSums(tm.list[[i]]))
  low.tma[i]<-list(matrix(tma.list[[i]], ncol=(dim(tma.list[[i]])[2]), byrow=FALSE))
}


# TRANSITION MATRIX HIGH

df.high<- df.high %>% mutate(seq= ifelse(states %in% c(3,4), NA, lead_states),
                         seq= ifelse(seq %in% NA, states, lead_states)) 

dfhigh.loop<-c()
tm.list<-c()
tma.list<-c()
high.tma<-c()
for (i in 1:30){
  dfhigh.loop[i]<-list(df.high %>% filter(icudayseq_asc %in% c(0:i-1)))
  tm.list[i]<-list(table(dfhigh.loop[[i]][["states"]], dfhigh.loop[[i]][["seq"]]))
  tma.list[i]<-list(tm.list[[i]]/rowSums(tm.list[[i]]))
  high.tma[i]<-list(matrix(tma.list[[i]], ncol=(dim(tma.list[[i]])[2]), byrow=FALSE))
}

rm(dflow.loop)
rm(dfhigh.loop)
rm(tm.list)
rm(tmlow.list)
rm(tmhigh.list)
rm(tma.list)
rm(tmalow.list)
rm(tmahigh.list)


# low.tma,  high.tma,   are the transition matrices for each strata
# where we have a list of matrices for 0-1 days, 0-2 days, 0-3 days etc

#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#






#############################################################################################
# Quartile analysis                                                            #
################################################################################
# Divide cohort based on groups and demographics ###############################
################################################################################

# Obtain keys for sofa groups
sofa<-df %>% filter(icudayseq_asc == 0)

# Create df keys for each quantile sofa severity group
key.q1<-sofa %>% filter(sofa_group %in% "Q1") %>% select(icustay_id)
key.q2<-sofa %>% filter(sofa_group %in% "Q2") %>% select(icustay_id)
key.q3<-sofa %>% filter(sofa_group %in% "Q3") %>% select(icustay_id)
key.q4<-sofa %>% filter(sofa_group %in% "Q4") %>% select(icustay_id)

# Create df for each quantile sofa severity group
df.q1<-semi_join(df, key.q1)
df.q2<-semi_join(df, key.q2)
df.q3<-semi_join(df, key.q3)
df.q4<-semi_join(df, key.q4)

# Drop keys and sofa table
rm(key.q1)
rm(key.q2)
rm(key.q3)
rm(key.q4)
rm(sofa)

# Compute demographics for each quantile sofa group

#Q1#########################
table(df.q1$death_flag)
length(unique(df.q1$icustay_id))
#Q2#########################
table(df.q2$death_flag)
length(unique(df.q2$icustay_id))
#Q3#########################
table(df.q3$death_flag)
length(unique(df.q3$icustay_id))
#Q4#########################
table(df.q4$death_flag)
length(unique(df.q4$icustay_id))

# How many days on average did they spend in each unit

#Q1#########################
table(df.q1$states)[1:3]
sum(table(df.q1$states)[1:3]) # total number of days in ICU
sum(table(df.q1$states)) # total number of days passed
#Q2#########################
table(df.q2$states)[1:3]
sum(table(df.q2$states)[1:3])# total number of days in ICU
sum(table(df.q2$states)) # total number of days passed
#Q3#########################
table(df.q3$states)[1:3]
sum(table(df.q3$states)[1:3])# total number of days in ICU
sum(table(df.q3$states)) # total number of days passed
#Q4#########################
table(df.q4$states)[1:3]
sum(table(df.q4$states)[1:3])# total number of days in ICU
sum(table(df.q4$states)) # total number of days passed


# Obtain the last day metrics for each quantile sofa group
df.q1.lastrow<-df.q1 %>% group_by(icustay_id) %>% filter(icudayseq_asc %in% max(icudayseq_asc))
df.q2.lastrow<-df.q2 %>% group_by(icustay_id) %>% filter(icudayseq_asc %in% max(icudayseq_asc))
df.q3.lastrow<-df.q3 %>% group_by(icustay_id) %>% filter(icudayseq_asc %in% max(icudayseq_asc))
df.q4.lastrow<-df.q4 %>% group_by(icustay_id) %>% filter(icudayseq_asc %in% max(icudayseq_asc))

# Last row statistics for each quantile sofa group

#Q1#########################
summary(df.q1.lastrow$icudayseq_asc)
summary(df.q1.lastrow$sofa_last)
summary(df.q1.lastrow$admission_age)
table(df.q1.lastrow$gender)
(table(df.q1.lastrow$gender)/length(unique(df.q1$icustay_id)))*100
#Q2#########################
summary(df.q2.lastrow$icudayseq_asc)
summary(df.q2.lastrow$sofa_last)
summary(df.q2.lastrow$admission_age)
table(df.q2.lastrow$gender)
(table(df.q2.lastrow$gender)/length(unique(df.q2$icustay_id)))*100
#Q3#########################
summary(df.q3.lastrow$icudayseq_asc)
summary(df.q3.lastrow$sofa_last)
summary(df.q3.lastrow$admission_age)
table(df.q3.lastrow$gender)
(table(df.q3.lastrow$gender)/length(unique(df.q3$icustay_id)))*100
#Q4#########################
summary(df.q4.lastrow$icudayseq_asc)
summary(df.q4.lastrow$sofa_last)
summary(df.q4.lastrow$admission_age)
table(df.q4.lastrow$gender)
(table(df.q4.lastrow$gender)/length(unique(df.q4$icustay_id)))*100

rm(df.q1.lastrow)
rm(df.q2.lastrow)
rm(df.q3.lastrow)
rm(df.q4.lastrow)

#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#

# ONE DAY OF HIC 
hic<-df %>% filter(((states%in%1) != (lead1)))# filter states transition for patients that receive 1 day of Highly invasive care
index.1<-which(hic.one$states %in% c(1)) # define location for 1s in A
index<- (c(index.1,index.1+1)) # concatanation of indexes of interest
hic.tm<- matrix(c(hic$states[index.1],hic$states[index.1+1]), nrow=length(index.1), ncol=2)
hic.tm<- as.data.frame(hic.tm)
tm.1<-table(hic.tm$V1, hic.tm$V2)







################################################################################
## TRANSITION MATRIX ###########################################################
################################################################################
# Create a lead and a lag column for states
# this is to prepare the dataframe for the Multi State Model
# I create a lead and lag state to create the tstart and tstop columns
# I also converted NA in these columns to 99 for the ifelse statesments to work
df.q1<-df.q1 %>% 
  mutate(lag_states= lag(states),
         lead_states= lead(states), 
         lag_states = replace_na(lag_states, 99),
         lead_states = replace_na(lead_states, 99),
         tstart= ifelse(states != lag_states, icudayseq_asc, NA),
         tstop= ifelse(states != lead_states, icudayseq_asc, NA))

df.q2<-df.q2 %>% 
  mutate(lag_states= lag(states),
         lead_states= lead(states), 
         lag_states = replace_na(lag_states, 99),
         lead_states = replace_na(lead_states, 99),
         tstart= ifelse(states != lag_states, icudayseq_asc, NA),
         tstop= ifelse(states != lead_states, icudayseq_asc, NA))

df.q3<-df.q3 %>% 
  mutate(lag_states= lag(states),
         lead_states= lead(states), 
         lag_states = replace_na(lag_states, 99),
         lead_states = replace_na(lead_states, 99),
         tstart= ifelse(states != lag_states, icudayseq_asc, NA),
         tstop= ifelse(states != lead_states, icudayseq_asc, NA))

df.q4<-df.q4 %>% 
  mutate(lag_states= lag(states),
         lead_states= lead(states), 
         lag_states = replace_na(lag_states, 99),
         lead_states = replace_na(lead_states, 99),
         tstart= ifelse(states != lag_states, icudayseq_asc, NA),
         tstop= ifelse(states != lead_states, icudayseq_asc, NA))


# TRANSITION MATRIX Q1

df.q1<- df.q1 %>% mutate(seq= ifelse(states %in% c(3,4), NA, lead_states),
                         seq= ifelse(seq %in% NA, states, lead_states)) 

dfq1.loop<-c()
tm.list<-c()
tma.list<-c()
q1.tma<-c()
for (i in 1:30){
  dfq1.loop[i]<-list(df.q1 %>% filter(icudayseq_asc %in% c(0:i-1)))
  tm.list[i]<-list(table(dfq1.loop[[i]][["states"]], dfq1.loop[[i]][["seq"]]))
  tma.list[i]<-list(tm.list[[i]]/rowSums(tm.list[[i]]))
  q1.tma[i]<-list(matrix(tma.list[[i]], ncol=(dim(tma.list[[i]])[2]), byrow=FALSE))
}


# TRANSITION MATRIX Q2

df.q2<- df.q2 %>% mutate(seq= ifelse(states %in% c(3,4), NA, lead_states),
                         seq= ifelse(seq %in% NA, states, lead_states)) 

dfq2.loop<-c()
tm.list<-c()
tma.list<-c()
q2.tma<-c()
for (i in 1:30){
  dfq2.loop[i]<-list(df.q2 %>% filter(icudayseq_asc %in% c(0:i-1)))
  tm.list[i]<-list(table(dfq2.loop[[i]][["states"]], dfq2.loop[[i]][["seq"]]))
  tma.list[i]<-list(tm.list[[i]]/rowSums(tm.list[[i]]))
  q2.tma[i]<-list(matrix(tma.list[[i]], ncol=(dim(tma.list[[i]])[2]), byrow=FALSE))
}


# TRANSITION MATRIX Q3

df.q3<- df.q3 %>% mutate(seq= ifelse(states %in% c(3,4), NA, lead_states),
                         seq= ifelse(seq %in% NA, states, lead_states)) 

dfq3.loop<-c()
tm.list<-c()
tma.list<-c()
q3.tma<-c()
for (i in 1:30){
  dfq3.loop[i]<-list(df.q3 %>% filter(icudayseq_asc %in% c(0:i-1)))
  tm.list[i]<-list(table(dfq3.loop[[i]][["states"]], dfq3.loop[[i]][["seq"]]))
  tma.list[i]<-list(tm.list[[i]]/rowSums(tm.list[[i]]))
  q3.tma[i]<-list(matrix(tma.list[[i]], ncol=(dim(tma.list[[i]])[2]), byrow=FALSE))
}

# TRANSITION MATRIX Q4

df.q4<- df.q4 %>% mutate(seq= ifelse(states %in% c(3,4), NA, lead_states),
                         seq= ifelse(seq %in% NA, states, lead_states)) 

dfq4.loop<-c()
tm.list<-c()
tma.list<-c()
q4.tma<-c()
for (i in 1:30){
  dfq4.loop[i]<-list(df.q4 %>% filter(icudayseq_asc %in% c(0:i-1)))
  tm.list[i]<-list(table(dfq4.loop[[i]][["states"]], dfq4.loop[[i]][["seq"]]))
  tma.list[i]<-list(tm.list[[i]]/rowSums(tm.list[[i]]))
  q4.tma[i]<-list(matrix(tma.list[[i]], ncol=(dim(tma.list[[i]])[2]), byrow=FALSE))
}

rm(dfq1.loop)
rm(dfq2.loop)
rm(dfq3.loop)
rm(dfq4.loop)
rm(tm.list)
rm(tma.list)

# q1.tma,  q2.tma,  q3.tma,  q4.tma are the transition matrices for each strata
# where we have a list of matrices for 0-1 days, 0-2 days, 0-3 days etc

#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#

################################################################################
## MARKOV OBJECT ###############################################################
################################################################################

# Q1 object

MOq1.1<-new("markovchain",transitionMatrix=q1.tma[[1]], name="MarkovChain Q1")
MOq1.2<-new("markovchain",transitionMatrix=q1.tma[[2]], name="MarkovChain Q1")
MOq1.3<-new("markovchain",transitionMatrix=q1.tma[[3]], name="MarkovChain Q1")
MOq1.4<-new("markovchain",transitionMatrix=q1.tma[[4]], name="MarkovChain Q1")
MOq1.5<-new("markovchain",transitionMatrix=q1.tma[[5]], name="MarkovChain Q1")
MOq1.6<-new("markovchain",transitionMatrix=q1.tma[[6]], name="MarkovChain Q1")
MOq1.7<-new("markovchain",transitionMatrix=q1.tma[[7]], name="MarkovChain Q1")
MOq1.8<-new("markovchain",transitionMatrix=q1.tma[[8]], name="MarkovChain Q1")
MOq1.9<-new("markovchain",transitionMatrix=q1.tma[[9]], name="MarkovChain Q1")
MOq1.10<-new("markovchain",transitionMatrix=q1.tma[[10]], name="MarkovChain Q1")
MOq1.11<-new("markovchain",transitionMatrix=q1.tma[[11]], name="MarkovChain Q1")
MOq1.12<-new("markovchain",transitionMatrix=q1.tma[[12]], name="MarkovChain Q1")
MOq1.13<-new("markovchain",transitionMatrix=q1.tma[[13]], name="MarkovChain Q1")
MOq1.14<-new("markovchain",transitionMatrix=q1.tma[[14]], name="MarkovChain Q1")
MOq1.15<-new("markovchain",transitionMatrix=q1.tma[[15]], name="MarkovChain Q1")
MOq1.16<-new("markovchain",transitionMatrix=q1.tma[[16]], name="MarkovChain Q1")
MOq1.17<-new("markovchain",transitionMatrix=q1.tma[[17]], name="MarkovChain Q1")
MOq1.18<-new("markovchain",transitionMatrix=q1.tma[[18]], name="MarkovChain Q1")
MOq1.19<-new("markovchain",transitionMatrix=q1.tma[[19]], name="MarkovChain Q1")
MOq1.20<-new("markovchain",transitionMatrix=q1.tma[[20]], name="MarkovChain Q1")
MOq1.21<-new("markovchain",transitionMatrix=q1.tma[[21]], name="MarkovChain Q1")
MOq1.22<-new("markovchain",transitionMatrix=q1.tma[[22]], name="MarkovChain Q1")
MOq1.23<-new("markovchain",transitionMatrix=q1.tma[[23]], name="MarkovChain Q1")
MOq1.24<-new("markovchain",transitionMatrix=q1.tma[[24]], name="MarkovChain Q1")
MOq1.25<-new("markovchain",transitionMatrix=q1.tma[[25]], name="MarkovChain Q1")
MOq1.26<-new("markovchain",transitionMatrix=q1.tma[[26]], name="MarkovChain Q1")
MOq1.27<-new("markovchain",transitionMatrix=q1.tma[[27]], name="MarkovChain Q1")
MOq1.28<-new("markovchain",transitionMatrix=q1.tma[[28]], name="MarkovChain Q1")
MOq1.29<-new("markovchain",transitionMatrix=q1.tma[[29]], name="MarkovChain Q1")
MOq1.30<-new("markovchain",transitionMatrix=q1.tma[[30]], name="MarkovChain Q1")

# Q2 object

MOq2.1<-new("markovchain",transitionMatrix=q2.tma[[1]], name="MarkovChain Q2")
MOq2.2<-new("markovchain",transitionMatrix=q2.tma[[2]], name="MarkovChain Q2")
MOq2.3<-new("markovchain",transitionMatrix=q2.tma[[3]], name="MarkovChain Q2")
MOq2.4<-new("markovchain",transitionMatrix=q2.tma[[4]], name="MarkovChain Q2")
MOq2.5<-new("markovchain",transitionMatrix=q2.tma[[5]], name="MarkovChain Q2")
MOq2.6<-new("markovchain",transitionMatrix=q2.tma[[6]], name="MarkovChain Q2")
MOq2.7<-new("markovchain",transitionMatrix=q2.tma[[7]], name="MarkovChain Q2")
MOq2.8<-new("markovchain",transitionMatrix=q2.tma[[8]], name="MarkovChain Q2")
MOq2.9<-new("markovchain",transitionMatrix=q2.tma[[9]], name="MarkovChain Q2")
MOq2.10<-new("markovchain",transitionMatrix=q2.tma[[10]], name="MarkovChain Q2")
MOq2.11<-new("markovchain",transitionMatrix=q2.tma[[11]], name="MarkovChain Q2")
MOq2.12<-new("markovchain",transitionMatrix=q2.tma[[12]], name="MarkovChain Q2")
MOq2.13<-new("markovchain",transitionMatrix=q2.tma[[13]], name="MarkovChain Q2")
MOq2.14<-new("markovchain",transitionMatrix=q2.tma[[14]], name="MarkovChain Q2")
MOq2.15<-new("markovchain",transitionMatrix=q2.tma[[15]], name="MarkovChain Q2")
MOq2.16<-new("markovchain",transitionMatrix=q2.tma[[16]], name="MarkovChain Q2")
MOq2.17<-new("markovchain",transitionMatrix=q2.tma[[17]], name="MarkovChain Q2")
MOq2.18<-new("markovchain",transitionMatrix=q2.tma[[18]], name="MarkovChain Q2")
MOq2.19<-new("markovchain",transitionMatrix=q2.tma[[19]], name="MarkovChain Q2")
MOq2.20<-new("markovchain",transitionMatrix=q2.tma[[20]], name="MarkovChain Q2")
MOq2.21<-new("markovchain",transitionMatrix=q2.tma[[21]], name="MarkovChain Q2")
MOq2.22<-new("markovchain",transitionMatrix=q2.tma[[22]], name="MarkovChain Q2")
MOq2.23<-new("markovchain",transitionMatrix=q2.tma[[23]], name="MarkovChain Q2")
MOq2.24<-new("markovchain",transitionMatrix=q2.tma[[24]], name="MarkovChain Q2")
MOq2.25<-new("markovchain",transitionMatrix=q2.tma[[25]], name="MarkovChain Q2")
MOq2.26<-new("markovchain",transitionMatrix=q2.tma[[26]], name="MarkovChain Q2")
MOq2.27<-new("markovchain",transitionMatrix=q2.tma[[27]], name="MarkovChain Q2")
MOq2.28<-new("markovchain",transitionMatrix=q2.tma[[28]], name="MarkovChain Q2")
MOq2.29<-new("markovchain",transitionMatrix=q2.tma[[29]], name="MarkovChain Q2")
MOq2.30<-new("markovchain",transitionMatrix=q2.tma[[30]], name="MarkovChain Q2")

# Q3 object

MOq3.1<-new("markovchain",transitionMatrix=q3.tma[[1]], name="MarkovChain Q3")
MOq3.2<-new("markovchain",transitionMatrix=q3.tma[[2]], name="MarkovChain Q3")
MOq3.3<-new("markovchain",transitionMatrix=q3.tma[[3]], name="MarkovChain Q3")
MOq3.4<-new("markovchain",transitionMatrix=q3.tma[[4]], name="MarkovChain Q3")
MOq3.5<-new("markovchain",transitionMatrix=q3.tma[[5]], name="MarkovChain Q3")
MOq3.6<-new("markovchain",transitionMatrix=q3.tma[[6]], name="MarkovChain Q3")
MOq3.7<-new("markovchain",transitionMatrix=q3.tma[[7]], name="MarkovChain Q3")
MOq3.8<-new("markovchain",transitionMatrix=q3.tma[[8]], name="MarkovChain Q3")
MOq3.9<-new("markovchain",transitionMatrix=q3.tma[[9]], name="MarkovChain Q3")
MOq3.10<-new("markovchain",transitionMatrix=q3.tma[[10]], name="MarkovChain Q3")
MOq3.11<-new("markovchain",transitionMatrix=q3.tma[[11]], name="MarkovChain Q3")
MOq3.12<-new("markovchain",transitionMatrix=q3.tma[[12]], name="MarkovChain Q3")
MOq3.13<-new("markovchain",transitionMatrix=q3.tma[[13]], name="MarkovChain Q3")
MOq3.14<-new("markovchain",transitionMatrix=q3.tma[[14]], name="MarkovChain Q3")
MOq3.15<-new("markovchain",transitionMatrix=q3.tma[[15]], name="MarkovChain Q3")
MOq3.16<-new("markovchain",transitionMatrix=q3.tma[[16]], name="MarkovChain Q3")
MOq3.17<-new("markovchain",transitionMatrix=q3.tma[[17]], name="MarkovChain Q3")
MOq3.18<-new("markovchain",transitionMatrix=q3.tma[[18]], name="MarkovChain Q3")
MOq3.19<-new("markovchain",transitionMatrix=q3.tma[[19]], name="MarkovChain Q3")
MOq3.20<-new("markovchain",transitionMatrix=q3.tma[[20]], name="MarkovChain Q3")
MOq3.21<-new("markovchain",transitionMatrix=q3.tma[[21]], name="MarkovChain Q3")
MOq3.22<-new("markovchain",transitionMatrix=q3.tma[[22]], name="MarkovChain Q3")
MOq3.23<-new("markovchain",transitionMatrix=q3.tma[[23]], name="MarkovChain Q3")
MOq3.24<-new("markovchain",transitionMatrix=q3.tma[[24]], name="MarkovChain Q3")
MOq3.25<-new("markovchain",transitionMatrix=q3.tma[[25]], name="MarkovChain Q3")
MOq3.26<-new("markovchain",transitionMatrix=q3.tma[[26]], name="MarkovChain Q3")
MOq3.27<-new("markovchain",transitionMatrix=q3.tma[[27]], name="MarkovChain Q3")
MOq3.28<-new("markovchain",transitionMatrix=q3.tma[[28]], name="MarkovChain Q3")
MOq3.29<-new("markovchain",transitionMatrix=q3.tma[[29]], name="MarkovChain Q3")
MOq3.30<-new("markovchain",transitionMatrix=q3.tma[[30]], name="MarkovChain Q3")

# Q4 object

MOq4.1<-new("markovchain",transitionMatrix=q4.tma[[1]], name="MarkovChain Q4")
MOq4.2<-new("markovchain",transitionMatrix=q4.tma[[2]], name="MarkovChain Q4")
MOq4.3<-new("markovchain",transitionMatrix=q4.tma[[3]], name="MarkovChain Q4")
MOq4.4<-new("markovchain",transitionMatrix=q4.tma[[4]], name="MarkovChain Q4")
MOq4.5<-new("markovchain",transitionMatrix=q4.tma[[5]], name="MarkovChain Q4")
MOq4.6<-new("markovchain",transitionMatrix=q4.tma[[6]], name="MarkovChain Q4")
MOq4.7<-new("markovchain",transitionMatrix=q4.tma[[7]], name="MarkovChain Q4")
MOq4.8<-new("markovchain",transitionMatrix=q4.tma[[8]], name="MarkovChain Q4")
MOq4.9<-new("markovchain",transitionMatrix=q4.tma[[9]], name="MarkovChain Q4")
MOq4.10<-new("markovchain",transitionMatrix=q4.tma[[10]], name="MarkovChain Q4")
MOq4.11<-new("markovchain",transitionMatrix=q4.tma[[11]], name="MarkovChain Q4")
MOq4.12<-new("markovchain",transitionMatrix=q4.tma[[12]], name="MarkovChain Q4")
MOq4.13<-new("markovchain",transitionMatrix=q4.tma[[13]], name="MarkovChain Q4")
MOq4.14<-new("markovchain",transitionMatrix=q4.tma[[14]], name="MarkovChain Q4")
MOq4.15<-new("markovchain",transitionMatrix=q4.tma[[15]], name="MarkovChain Q4")
MOq4.16<-new("markovchain",transitionMatrix=q4.tma[[16]], name="MarkovChain Q4")
MOq4.17<-new("markovchain",transitionMatrix=q4.tma[[17]], name="MarkovChain Q4")
MOq4.18<-new("markovchain",transitionMatrix=q4.tma[[18]], name="MarkovChain Q4")
MOq4.19<-new("markovchain",transitionMatrix=q4.tma[[19]], name="MarkovChain Q4")
MOq4.20<-new("markovchain",transitionMatrix=q4.tma[[20]], name="MarkovChain Q4")
MOq4.21<-new("markovchain",transitionMatrix=q4.tma[[21]], name="MarkovChain Q4")
MOq4.22<-new("markovchain",transitionMatrix=q4.tma[[22]], name="MarkovChain Q4")
MOq4.23<-new("markovchain",transitionMatrix=q4.tma[[23]], name="MarkovChain Q4")
MOq4.24<-new("markovchain",transitionMatrix=q4.tma[[24]], name="MarkovChain Q4")
MOq4.25<-new("markovchain",transitionMatrix=q4.tma[[25]], name="MarkovChain Q4")
MOq4.26<-new("markovchain",transitionMatrix=q4.tma[[26]], name="MarkovChain Q4")
MOq4.27<-new("markovchain",transitionMatrix=q4.tma[[27]], name="MarkovChain Q4")
MOq4.28<-new("markovchain",transitionMatrix=q4.tma[[28]], name="MarkovChain Q4")
MOq4.29<-new("markovchain",transitionMatrix=q4.tma[[29]], name="MarkovChain Q4")
MOq4.30<-new("markovchain",transitionMatrix=q4.tma[[30]], name="MarkovChain Q4")








################################################################################
## MARKOV OBJECT ###############################################################
################################################################################


# We can recreate transitions for individuals using the rmarkovchain 
# NOTE TO SELF
#  when not prototyping will have to set a seed due to stocastic nature
individual <- rmarkovchain(n = 10, 
                           object = dtmcA, 
                           t0 = "ICU",
                           include.t0 = TRUE,
                           parallel = FALSE)
individual[1:10]

# I boot strap this function to create a matrix of multiple individuals.
# This creates a wide matrix

B <- 10000
N <- 30
start.time <- Sys.time()
long_rmc <- replicate(B, {
  X <- rmarkovchain(n = N, 
                    object = dtmcA, 
                    t0 = "ICU",
                    include.t0 = FALSE,
                    parallel = TRUE)})
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken




#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#

# Create a lead and a lag column for states
# this is to prepare the dataframe for the Multi State Model
# I create a lead and lag state to create the tstart and tstop columnsmy
# I also converted NA in these columns to 99 for the ifelse statesments to work
df<-df %>% 
  mutate(lag_states= lag(states),
         lead_states= lead(states), 
         lag_states = replace_na(lag_states, 99),
         lead_states = replace_na(lead_states, 99),
         tstart= ifelse(states != lag_states, icudayseq_asc, NA),
         tstop= ifelse(states != lead_states, icudayseq_asc, NA))




# https://www.coursera.org/lecture/foundations-marketing-analytics/how-to-compute-a-transition-matrix-in-r-recital-1-jD6xC

# TRANSITION MATRIX

mysequence<-df$states
createSequenceMatrix(mysequence)

df<- df %>% mutate(seq= ifelse(states %in% c(3,4), NA, lead_states),
                   seq= ifelse(seq %in% NA, states, lead_states)) 

transition<- df %>% select(states, seq) %>% filter(!is.na(seq))
transition<- df %>% mutate(states= as.factor(states), 
                           seq= as.factor(seq))
tm<-table(transition$states, transition$seq)
tmA<-tm/rowSums(tm)
# attributes(tmA)$class <- "matrix"  # problem is that it creates NA 
# tmA[tmA == 'NaN'] <- 0 # problem is that it creates NA

# Given the above problem I write the matrix by hand and define the absorbing
# states by using the intersection of the from-to with a probability of 1
tmA <- matrix(c(0.56735751, 0.1658031, 0.002590674, 0.2577720, 0.006476684,
                0.04582022, 0.8674362, 0.001748863, 0.0664568, 0.018537950,
                0.00000000, 0.0000000, 0.000000000, 0.5000000, 0.500000000,
                0.00000000, 0.0000000, 0.000000000, 1.0000000, 0.000000000,
                0.00000000, 0.0000000, 0.000000000, 0.0000000, 1.000000000)
              ,nrow = 5, byrow = TRUE)

# I use markovchain to define the markov object with the above transition matrix
dtmcA <- new("markovchain",
             transitionMatrix=tmA, 
             states=c("ICU","Invasive","CMO","Discharged", "Death"), 
             name="MarkovChain A") 

dtmcA # this is the markov object

# There are multiple ways to plot the markov object or the transition matrix
# some of these wayss are doen below, though it is incomplete for the diagram
# library
plot(dtmcA, main = "Transition probability matrix for DBM")
library(diagram)

# To plot the transition matrix I reduce the decimals for visual appeal
tmA <- matrix(c(0.567, 0.166, 0.0026, 0.258, 0.006,
                0.046, 0.867, 0.002, 0.066, 0.019,
                0, 0, 0, 0.500, 0.500,
                0, 0, 0, 1, 0,
                0, 0, 0, 0, 1)
              ,nrow = 5, byrow = TRUE)

plotmat(t(tmA),
     relsize = 0.75,
     pos=NULL,
     curve=NULL,
     name=c("1","2","3","4","5"),
     absent=0,
     lwd=0.1,
     lcol="#440D54",
     box.size = 0.1,
     box.type = "circle",
     box.prop = 0.75,
     box.col="#AADFDF25",
     dr=0.1,
     dtext=1,
     self.lwd=2,
     self.cex=0.5,
     arr.type="simple",
     endhead=FALSE,
     box.cex=0.9,
     cex.txt=0.6,
     main="Test",
     cex.main=1,
     latex=FALSE,
     shadow.size = 0.0,
     txt.xadj=1, 
     txt.yadj=-1
)

# We can recreate transitions for individuals using the rmarkovchain 
# NOTE TO SELF
#  when not prototyping will have to set a seed due to stocastic nature
individual <- rmarkovchain(n = 10, 
                               object = dtmcA, 
                               t0 = "ICU",
                               include.t0 = TRUE,
                               parallel = FALSE)
individual[1:10]

# I boot strap this function to create a matrix of multiple individuals.
# This creates a wide matrix

B <- 10000
N <- 30
start.time <- Sys.time()
long_rmc <- replicate(B, {
  X <- rmarkovchain(n = N, 
                    object = dtmcA, 
                    t0 = "ICU",
                    include.t0 = FALSE,
                    parallel = TRUE)})
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


# We can transpose the matrix, transform to a dataframe and then gather with 
# dplyr to later evaluate simualted survival curves 

wide_rmc<-t(long_rmc)

# Now I turn this matrix into a data frame
wide_rmc_df<- as.data.frame(wide_rmc)
id<- 1:nrow(wide_rmc_df)
wide_rmc_df<-cbind(id=id, wide_rmc_df)
long_rmc_df<-gather(wide_rmc_df, key=time, value=states, -1)

long_rmc_df<- long_rmc_df %>% 
  group_by(id) %>% 
  arrange(id) %>% 
  mutate(time= as.numeric(str_replace_all(time, "V", "")),
         days= time-1,
         time=NULL)


> heart.msm<-msm(states ~ icudayseq_asc, subject=icustay_id, data=df, qmatrix=tmA, death= c(4,5))


################################################################################
# Re structuring the data frame for survival analysis

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

# Then I remove the leadcolumn to be orderly
df1$leadtstop<- NULL

# To use the survival function the start and stop time can not be the same
# so first I add 1 to when that condition occurs
df1<- df1 %>% mutate(tstop= ifelse(tstart==tstop, tstop+1, tstop))

# Then Surv does not transition follow-ups ie stop time 7 can not follow with
# the next moment 8 in start time it has to be stop time 7-> start time 7 nline
# example: not allowed 0->7, 8->9 this is considered a gap so the data must be
# adjusted to read 0->7, 7->9

df1<-df1 %>% mutate(lagstop= lag(tstop),
                    tstart= ifelse(tstart>lagstop, lagstop, tstart)) %>% 
  select(-lagstop)

# The above process works except it did create one unwanted NA so I convert it 
# to its original number which was 0

df1<-df1 %>% 
  mutate(tstart = replace_na(tstart, 1))




################################################################################
################################################################################
################################################################################
## Extra- prototyping different approaches


# Package 'markovchain'
noofVisitsDist(dtmcA,2,"ICU")



# One way I'm working on to do the simulations is to leverage the transition 
# matrix and an initial state. However I encounter an error when trying to do
# several days by powering the marcov object since the probabilities grow 
# beyond one (work in progress or to be abdonded not sure)

initialState <- c(0, 1, 0, 0, 0)
simDays <- initialState * (dtmcA)
simDays
simDays/rowSums(simDays)



mcWP <- new("markovchain", states = c("rainy", "nice", "snowy"),
            transitionMatrix = matrix(c(0.5, 0.25, 0.25,
                                        0, 1, 0,
                                        0,0,1), byrow = T, nrow = 3))

W0 <- t(as.matrix(c(0, 1, 0)))
W1 <- W0 * mcWP^6; W1


canonicForm(dtmcA)
detectCores()



# Prototyping-experiment

# Set up a group status for the model -this will later be SOFA
df1$grp<- 1

sfit4 <- survfit(Surv(tstart, tstop, states) ~ grp, df1, id=icustay_id)
sfit4$transitions
layout(matrix(1:2,1,2), widths=2:1)
oldpar <- par(mar=c(5.1, 4.1, 1,.1))
plot(sfit4, col=rep(1:4,each=1), lwd=2, lty=1, xmax=30,
     xlab="Days", ylab="Current state") #for first 30 days
axis(1, xtime, xtime)
text(c(40, 40, 40, 40), c(.51, .13, .32, .01),
     c("statex", "statey", "statez", "stateb"), col=1:4)
par(mar=c(5.1, .1, 1, .1))
state5()
par(oldpar)

summary(sfit4)

##########sfit4$transitions#####################################################
ctab<-table(table(df1$icustay_id))
ctab

with(df1, table(table(icustay_id, states)))

etab <- table(df1$states, useNA="ifany")
etab

# I add in a column for group to proto-type the process. 
# Can do later with the SOFA groups (first and last)
df1$grp<- 1


df1<- df1 %>% mutate(states= as.factor(states))

sfit4 <- survfit(Surv(tstart, tstop, states) ~ grp, df1, id=icustay_id)
sfit4$transitions
layout(matrix(1:2,1,2), widths=2:1)
oldpar <- par(mar=c(5.1, 4.1, 1,.1))
plot(sfit4, col=rep(1:4,each=1), lwd=2, lty=1, xmax=30,
     xlab="Days", ylab="Current state")
axis(1, xtime, xtime)
text(c(40, 40, 40, 40), c(.51, .13, .32, .01),
     c("statex", "statey", "statez", "stateb"), col=1:4)
par(mar=c(5.1, .1, 1, .1))
state5()
par(oldpar)


test<-dplyr::pull(df, states)
ty<-markovchainFit(test) 
ty
#this created transition matrix bu tdidnt use the absorbing states
#so i continued to try and find a way to do that
# it may be possible by creating a matrix that has the states
# but to do this  i think i need a list which has each individual id transition
# which starts at the test part

id<- 1:nrow(df)
df<-cbind(id=id, df)

test<- df%>% select(icustay_id, states, icudayseq_asc)
test<-test %>% mutate(icudayseq_asc= as.factor(icudayseq_asc))
test2<-spread(test, icudayseq_asc, states)
test3<-as.data.frame(test2) #create as a dataframe to remove the spread "group" on test2
test3<-test3 %>% select(-(icustay_id))


DWmc <-new("markovchain",
           transitionMatrix = test3,
           states = c("0","1","2","3","4"),
           name = "Drunkard's Walk")



l<-plyr::dlply(test,.(icustay_id),)
b<-lapply(l, `[`, c('states'))
c<-as.matrix(b)
library(stringi)
m1 <- stri_list2matrix(b, byrow=TRUE, fill=NA)



mutat(tstop2=ifelse(is.na(tstop), nrow(tstop)+1,))

df$tstop[,]

# This is close, but there are still many NAs now I need to collapse these rows 
# without losing the data's sanity. To do this I will create a key to preserve
# the state and I will create a dataframe for only the msm information 
# then I will join
df2<-df1 %>% select(id, icustay_id, tstart, tstop)
df1<-df1 %>% select(id, icustay_id, states)
df2<-df2 %>% group_by(id) %>% summarise_each(funs(sum(., na.rm = TRUE)))

test<-left_join(df2, df1)

test<-df1 %>% group_by(id) %>% summarise_all(funs(last))

# Create the dummy variables for the states
df<- df %>% mutate(icu_state= ifelse(states==0, "1", "0"), 
                   invasive_state= ifelse(states==1, "1", "0"),
                   cmo_state= ifelse(states==2, "1", "0"),
                   home_state= ifelse(states==3, "1", "0"),
                   death_state= ifelse(states==4, "1", "0")


# Univariate cox regression model for 
res.cox <- coxph(Surv(icudayseq_asc, death_flag) ~ 
                   sofa_last,
                   data=df)

#
summary(res.cox)


fit<- survfit(Surv(icudayseq_asc, death_flag) ~ 
                sofa_last,
              data=df)

ggsurvplot(fit, data = df,
           ggtheme = theme_minimal())

risk = function(model, newdata, time) {
  as.numeric(1-summary(survfit(model, newdata = newdata, se.fit = F, conf.int = F), times = time)$surv)
}







# simulate discrete Markov chains according to transition matrix P
run.mc.sim <- function( P, num.iters = 50 ) {
  
  # number of possible states
  num.states <- nrow(P)
  
  # stores the states X_t through time
  states     <- numeric(num.iters)
  
  # initialize variable for first state 
  states[1]    <- 1
  
  for(t in 2:num.iters) {
    
    # probability vector to simulate next state X_{t+1}
    p  <- P[states[t-1], ]
    
    ## draw from multinomial and determine state
    states[t] <-  which(rmultinom(1, 1, p) == 1)
  }
  return(states)
}


num.chains     <- 5
num.iterations <- 50

# each column stores the sequence of states for a single chains
chain.states  <- matrix(NA, ncol=num.chains, nrow=num.iterations)

P <- (matrix(c(0.56735751, 0.1658031, 0.002590674, 0.2577720, 0.006476684,
               0.04582022, 0.8674362, 0.001748863, 0.0664568, 0.018537950,
               0.00000000, 0.0000000, 0.000000000, 0.5000000, 0.500000000,
               0.00000000, 0.0000000, 0.000000000, 1.0000000, 0.000000000,
               0.00000000, 0.0000000, 0.000000000, 0.0000000, 1.000000000)
             ,nrow = 5, byrow = TRUE))

# simulate chains
for(c in seq_len(num.chains)){
  chain.states[,c] <- run.mc.sim(P)
}

matplot(chain.states, type='l', lty=1, col=1:5, ylim=c(0,4), ylab='state', xlab='time')



predict(object = dtmcA, newdata = c("Invasive"), n.ahead = 50)


plot(dtmcA, main="Weather Markov Chain")




# # new("markovchain",transitionMatrix=q1.tma[[30]], name="MarkovChain Q1") 
# 
# 
# mcobj.q1<-list()
# for (i in 1:30){
#   mcobj.q1[i]<-new("markovchain",transitionMatrix=q1.tma[[i]], name="MarkovChain Q1")
# }
# lapply(q1.tma, , )




# 
# 
# # filter states transition for patients that receive 1 day of Highly invasive care
# hic<-df.q1 %>% filter(((states%in%1) | (lag1%in%c(0,2,3,4))))
# 
# # define location for 1s in A
# index.1<-which(hic$states %in% c(1)) 
# # join indexes of interest
# index<- (c(index.1,index.1+1))
# # create matrix of transitions
# hic.tm<- matrix(c(hic$states[index.1],hic$states[index.1+1]), nrow=length(index.1), ncol=2)
# # create transition matrix for one day of hic
# tm.1<-table(hic.tm[,1], hic.tm[,2])
# 
# # TWO DAYS OF HIC 
# 
# # filter states transition for patients that receive 2 dayS of Highly invasive care
# hic<-df.q1 %>% filter(((states%in%1) == (lead1)))
# hic<-hic %>% filter(((lead1%in%1) != (lead2)))
# 
# 
# # define location for 1s in A
# index.1<-which(hic$states %in% c(1)) 
# # join indexes of interest
# index<- (c(index.1,index.1+1))
# # create matrix of transitions
# hic.tm<- matrix(c(hic$states[index.1],hic$states[index.1+1]), nrow=length(index.1), ncol=2)
# # create transition matrix for one day of hic
# tm.1<-table(hic.tm[,1], hic.tm[,2])

# #______________________________________________________________________________#
# #______________________________________________________________________________#
# #______________________________________________________________________________#
# #______________________________________________________________________________#
# 
# #
# ################################################################################
# # LEAD STATES                                                                  #
# ################################################################################
# #
# 
# # The leads below will be used to define transition matrices later on
# # I could have used a loop but I opted for hard code due to interpretability
# 
# df<-df %>% mutate(lag1=lag(states),
#                   lag2=lag(states, n=2),
#                   lag3=lag(states, n=3),
#                   lag4=lag(states, n=4),
#                   lag5=lag(states, n=5),
#                   lag6=lag(states, n=6),
#                   lag7=lag(states, n=7),
#                   lag8=lag(states, n=8),
#                   lag9=lag(states, n=9),
#                   lag10=lag(states, n=10),
#                   lag11=lag(states, n=11),
#                   lag12=lag(states, n=12),
#                   lag13=lag(states, n=13),
#                   lag14=lag(states, n=14),
#                   lag15=lag(states, n=15),
#                   lag16=lag(states, n=16),
#                   lag17=lag(states, n=17),
#                   lag18=lag(states, n=18),
#                   lag19=lag(states, n=19),
#                   lag20=lag(states, n=20),
#                   lag21=lag(states, n=21),
#                   lag22=lag(states, n=22),
#                   lag23=lag(states, n=23),
#                   lag24=lag(states, n=24),
#                   lag25=lag(states, n=25),
#                   lag26=lag(states, n=26),
#                   lag27=lag(states, n=27),
#                   lag28=lag(states, n=28),
#                   lag29=lag(states, n=29),
#                   lag30=lag(states, n=30))



# col.q1<-df.q1 %>% select(icustay_id,icudayseq_asc,states)
# colnames(col.q1) <- c("id", "day", "states")
# 
# f1 <- function(data, n){
#   ids <- data %>%
#     mutate(stateslead = lead(states, default = last(states))) %>%
#     group_by(grp = rleid(states == 1)) %>% 
#     filter(n() == n, states == 1, stateslead != 1) %>%     
#     group_by(id) %>%     
#     filter(n() == 1) %>%
#     pull(id)
#   
#   data %>%
#     filter(id %in% id) %>%
#     group_by(id) %>% 
#     filter(cumsum(states) > 0)
# }
# 
# test<-col.q1 %>% group_by(rleid(states == 1))
# 
# ids <- col.q1 %>%
#   mutate(stateslead = lead(states, default = last(states))) 
# 
# ids<-ids %>% group_by(grp = rleid(states == 1))
# ids<-ids %>%filter(n() == n , states == 1 & stateslead != 1)
# 
# 
# 
# index.1<-which(col.q1$states %in% c(1))
# col.q1 %>% splice(index.1,  states[index.1+1])
# 
# 
# filter(col.q1, )
# 
# #this makes sure that they only leave hic & not re-enter
# index.1<-which(col.q1$states %in% c(1)) # index all the rows with 1
# col.q1<- col.q1 %>% mutate(col1=streak_run(states, k=1000)) # create a streak
# test<-col.q1 %>% .[c(index.1),] # collect all the rows with 1`
# cow<-test %>% group_by(id) %>% count(col1) %>% filter(col1%in%1 & n!=1)
# cow<-cow %>% filter(col1 != 1)
# key<-cow[,1]
# 
# 
# 
# 
# 
# test <- test %>% group_by(id) %>% filter(col1%in%max(col1))
# 
# test$column1<-sequence(rle(as.character(test$day))$lengths)
# 
# 
# 
# 
# install.packages("runner")
# library(runner)





# There is very limited data in the following data 
# df2.low<-f1(example.df, 2)
# df2.low<- df2.low %>% mutate(col1=lead(states))
# df2.low<-df2.low[!(df2.low$states==1 & df2.low$col1==1),]
# df2.low$col1<-NULL
# #
# df2.low<-df2.low %>% mutate(seq=lead(states))
# df2.low<- df2.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm2.low<-table(df2.low$states, df2.low$seq)
# tmA2.low<-tm2.low/rowSums(tm2.low)
# # fixing matrix to be a square 
# tmA2.low<- cbind(tmA2.low, c(0,0,0,0,0))
# colnames(tmA2.low)<-(c("nhit","cmo","discharge","death","hit"))
# tmA2.low<-setcolorder(as.data.table(tmA2.low), c(1,5,2,3,4))
# tmA2.low<-as.matrix(tmA2.low)
# # markov object
# mo2.low<-new("markovchain",transitionMatrix=(tmA2.low), name="MarkovChain Q1")
# 
# mo3.low<-mo2.low^2
# mo4.low<-mo2.low^3
# mo5.low<-mo2.low^4
# 
# df6.low<-f1(example.df, 6)
# df6.low<- df6.low %>% mutate(col1=lead(states))
# df6.low<-df6.low[!(df6.low$states==1 & df6.low$col1==1),]
# df6.low$col1<-NULL
# # 
# df6.low<-df6.low %>% mutate(seq=lead(states))
# df6.low<- df6.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm6.low<-table(df6.low$states, df6.low$seq)
# tmA6.low<-tm6.low/rowSums(tm6.low)
# # fixing to be square
# tmA6.low<- cbind(tmA6.low, c(0,0,0,0,0))
# colnames(tmA6.low)<-(c("nhit","cmo","discharge","death","hit"))
# tmA6.low<-setcolorder(as.data.table(tmA6.low), c(1,5,2,3,4))
# tmA6.low<-as.matrix(tmA6.low)
# # markov object
# mo6.low<-new("markovchain",transitionMatrix=(tmA6.low), name="MarkovChain Q1")
# 
# mo7.low<-mo6.low^2
# 
# df8.low<-f1(example.df, 8)
# df8.low<- df8.low %>% mutate(col1=lead(states))
# df8.low<-df8.low[!(df8.low$states==1 & df8.low$col1==1),]
# df8.low$col1<-NULL
# #
# df8.low<-df8.low %>% mutate(seq=lead(states))
# df8.low<- df8.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm8.low<-table(df8.low$states, df8.low$seq)
# tmA8.low<-tm8.low/rowSums(tm8.low)
# tmA8.low


# here is max


#  There is not enough data for the following days
# df3.low<-f1(example.df, 3)
# df3.low<- df3.low %>% mutate(col1=lead(states))
# df3.low<-df3.low[!(df3.low$states==1 & df3.low$col1==1),]
# df3.low$col1<-NULL
# #
# df3.low<-df3.low %>% mutate(seq=lead(states))
# df3.low<- df3.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm3.low<-table(df3.low$states, df3.low$seq)
# tmA3.low<-tm3.low/rowSums(tm3.low)
# #
# tmA3.low<- cbind(tmA3.low, c(0,0,0,0))
# colnames(tmA3.low)<-(c("nhit","discharge","death","hit"))
# tmA3.low<-setcolorder(as.data.table(tmA3.low), c(1,4,2,3))
# tmA3.low<-as.matrix(tmA3.low)
# # markov object
# mo3.low<-new("markovchain",transitionMatrix=(tmA3.low), name="MarkovChain Q1")
# #not enough data
# df4.low<-f1(example.df, 4)
# df4.low<- df4.low %>% mutate(col1=lead(states))
# df4.low<-df4.low[!(df4.low$states==1 & df4.low$col1==1),]
# df4.low$col1<-NULL
# #
# df4.low<-df4.low %>% mutate(seq=lead(states))
# df4.low<- df4.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm4.low<-table(df4.low$states, df4.low$seq)
# tmA4.low<-tm4.low/rowSums(tm4.low)
# # not enough data
# df5.low<-f1(example.df, 5)
# df5.low<- df5.low %>% mutate(col1=lead(states))
# df5.low<-df5.low[!(df5.low$states==1 & df5.low$col1==1),]
# df5.low$col1<-NULL
# #
# df5.low<-df5.low %>% mutate(seq=lead(states))
# df5.low<- df5.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm5.low<-table(df5.low$states, df5.low$seq)
# tmA5.low<-tm5.low/rowSums(tm5.low)
# tmA5.low
# # not enough data
# df7.low<-f1(example.df, 7)
# df7.low<- df7.low %>% mutate(col1=lead(states))
# df7.low<-df7.low[!(df7.low$states==1 & df7.low$col1==1),]
# df7.low$col1<-NULL
# #
# df7.low<-df7.low %>% mutate(seq=lead(states))
# df7.low<- df7.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm7.low<-table(df7.low$states, df7.low$seq)
# tmA7.low<-tm7.low/rowSums(tm7.low)
# tmA7.low
# # not enough data
# df9.low<-f1(example.df, 9)
# df9.low<- df9.low %>% mutate(col1=lead(states))
# df9.low<-df9.low[!(df9.low$states==1 & df9.low$col1==1),]
# df9.low$col1<-NULL
# #
# df9.low<-df9.low %>% mutate(seq=lead(states))
# df9.low<- df9.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm9.low<-table(df9.low$states, df9.low$seq)
# tmA9.low<-tm9.low/rowSums(tm9.low)
# 
# df10.low<-f1(example.df, 10)
# df10.low<- df10.low %>% mutate(col1=lead(states))
# df10.low<-df10.low[!(df10.low$states==1 & df10.low$col1==1),]
# df10.low$col1<-NULL
# #
# df10.low<-df10.low %>% mutate(seq=lead(states))
# df10.low<- df10.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm10.low<-table(df10.low$states, df10.low$seq)
# tmA10.low<-tm10.low/rowSums(tm10.low)
# 
# df11.low<-f1(example.df, 11)
# df11.low<- df11.low %>% mutate(col1=lead(states))
# df11.low<-df11.low[!(df11.low$states==1 & df11.low$col1==1),]
# df11.low$col1<-NULL
# #
# df11.low<-df11.low %>% mutate(seq=lead(states))
# df11.low<- df11.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm11.low<-table(df11.low$states, df11.low$seq)
# tmA11.low<-tm11.low/rowSums(tm11.low)
# 
# df12.low<-f1(example.df, 12)
# df12.low<- df12.low %>% mutate(col1=lead(states))
# df12.low<-df12.low[!(df12.low$states==1 & df12.low$col1==1),]
# df12.low$col1<-NULL
# #
# df12.low<-df12.low %>% mutate(seq=lead(states))
# df12.low<- df12.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm12.low<-table(df12.low$states, df12.low$seq)
# tmA12.low<-tm12.low/rowSums(tm12.low)
# 
# df13.low<-f1(example.df, 13)
# df13.low<- df13.low %>% mutate(col1=lead(states))
# df13.low<-df13.low[!(df13.low$states==1 & df13.low$col1==1),]
# df13.low$col1<-NULL
# #
# df13.low<-df13.low %>% mutate(seq=lead(states))
# df13.low<- df13.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm13.low<-table(df13.low$states, df13.low$seq)
# tmA13.low<-tm13.low/rowSums(tm13.low)
# 
# df14.low<-f1(example.df, 14)
# df14.low<- df14.low %>% mutate(col1=lead(states))
# df14.low<-df14.low[!(df14.low$states==1 & df14.low$col1==1),]
# df14.low$col1<-NULL
# #
# df14.low<-df14.low %>% mutate(seq=lead(states))
# df14.low<- df14.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm14.low<-table(df14.low$states, df14.low$seq)
# tmA14.low<-tm14.low/rowSums(tm14.low)
# 
# df15.low<-f1(example.df, 15)
# df15.low<- df15.low %>% mutate(col1=lead(states))
# df15.low<-df15.low[!(df15.low$states==1 & df15.low$col1==1),]
# df15.low$col1<-NULL
# #
# df15.low<-df15.low %>% mutate(seq=lead(states))
# df15.low<- df15.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm15.low<-table(df15.low$states, df15.low$seq)
# tmA15.low<-tm15.low/rowSums(tm15.low)
# 
# df16.low<-f1(example.df, 16)
# df16.low<- df16.low %>% mutate(col1=lead(states))
# df16.low<-df16.low[!(df16.low$states==1 & df16.low$col1==1),]
# df16.low$col1<-NULL
# #
# df16.low<-df16.low %>% mutate(seq=lead(states))
# df16.low<- df16.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm16.low<-table(df16.low$states, df16.low$seq)
# tmA16.low<-tm16.low/rowSums(tm16.low)
# 
# df17.low<-f1(example.df, 17)
# df17.low<- df17.low %>% mutate(col1=lead(states))
# df17.low<-df17.low[!(df17.low$states==1 & df17.low$col1==1),]
# df17.low$col1<-NULL
# #
# df17.low<-df17.low %>% mutate(seq=lead(states))
# df17.low<- df17.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm17.low<-table(df17.low$states, df17.low$seq)
# tmA17.low<-tm17.low/rowSums(tm17.low)
# 
# df18.low<-f1(example.df, 18)
# df18.low<- df18.low %>% mutate(col1=lead(states))
# df18.low<-df18.low[!(df18.low$states==1 & df18.low$col1==1),]
# df18.low$col1<-NULL
# #
# df18.low<-df18.low %>% mutate(seq=lead(states))
# df18.low<- df18.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm18.low<-table(df18.low$states, df18.low$seq)
# tmA18.low<-tm18.low/rowSums(tm18.low)
# 
# df19.low<-f1(example.df, 19)
# df19.low<- df19.low %>% mutate(col1=lead(states))
# df19.low<-df19.low[!(df19.low$states==1 & df19.low$col1==1),]
# df19.low$col1<-NULL
# #
# df19.low<-df19.low %>% mutate(seq=lead(states))
# df19.low<- df19.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm19.low<-table(df19.low$states, df19.low$seq)
# tmA19.low<-tm19.low/rowSums(tm19.low)
# 
# df20.low<-f1(example.df, 20)
# df20.low<- df20.low %>% mutate(col1=lead(states))
# df20.low<-df20.low[!(df20.low$states==1 & df20.low$col1==1),]
# df20.low$col1<-NULL
# #
# df20.low<-df20.low %>% mutate(seq=lead(states))
# df20.low<- df20.low %>% mutate(seq= ifelse(seq %in% NA, states, seq)) 
# tm20.low<-table(df20.low$states, df20.low$seq)
# tmA20.low<-tm20.low/rowSums(tm20.low)
