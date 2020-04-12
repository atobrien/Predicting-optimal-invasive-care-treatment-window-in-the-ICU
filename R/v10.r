# Work in progress
# Author: Anthony T.O'Brien Villate. MD/MPH
# Multistate models & decision analysis for ICU aggressive care

# 0. Load libraries ------------------------------------------------------------
library("tidyverse")
library("dplyr")
library("stringr")
library("runner")
library("msm")
library("lubridate")
library("parallel")
library("markovchain")
library("purrr")
library("data.table")
library("survival")
library("survminer")

# Definitions 
# States
# 0 non aggrssive 
# 1 aggressive icu 
# 2 cmo
# 3 home
# 4 death
options(scipen=999) 


# 1. Select cirrhosis cohort ---------------------------------------------------

df<-read.csv("C:/Users/Me/Desktop/mimic_timecohort_20190304.csv")

# convert icustay_id to factor
df <- df %>% mutate(icustay_id = as.factor(icustay_id))

# Replace NA in flags with 0
df <- 
  df %>% 
  mutate(death_flag = replace_na(death_flag, 0),
         cv_flag = replace_na(cv_flag, 0),
         crrt_flag = replace_na(crrt_flag, 0),
         vasopres_flag = replace_na(vasopres_flag, 0),
         mechvent_flag = replace_na(mechvent_flag, 0),
         cmo_flag = replace_na(cmo_flag, 0))

# Select icustays with more than 1 day stay, only first time admission,
# >=18years and with a sofa score


df <- 
  df %>% 
  group_by(icustay_id) %>% 
  filter(length(icustay_id)>1 & 
           admission_age>=18)  

# select patients that received mechanical ventilation
df<-df %>% inner_join(y=df %>% filter(mechvent_flag %in% 1) %>% distinct(icustay_id) )

# Create quartiles for each sofa score 
# sofa goes from 0-24 so: quantile(seq(1:24))
df <- 
  df %>% 
  mutate(sofa_group = case_when(sofa_last >= 0 & sofa_last <= 6 ~1,
                                sofa_last >= 7 & sofa_last <= 12 ~ 2,
                                sofa_last >= 13 & sofa_last <= 18 ~ 3,
                                sofa_last >= 19 & sofa_last <= 24 ~ 4))



# 2. Redefine CMO --------------------------------------------------------------

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


# add in group factor for number of days of icu aggressive care received
# also only include individuals who have spent greater or equal to 2 days

df<-
  df %>% 
  left_join((df %>% 
               group_by(icustay_id) %>% 
               filter(states %in% 1) %>% 
               tally() %>% 
               select(icustay_id, treatment_grp=n)),
            by='icustay_id') %>% 
  group_by(icustay_id)%>% 
  filter(n()>=2)

# 3 Check if there are people entering more than once---------------------------

nrow((inner_join(x=df %>% filter(first_icu_stay %in% 'True'),
                 y=df %>% filter(first_icu_stay %in% 'False'), 
                 by='icustay_id')))

# 4. Assumption on state logic--------------------------------------------------
# this is a mismatch in logic can't go from 4->3
df<-df %>% 
  group_by(icustay_id) %>% 
  mutate(states= 
           ifelse(states %in% 4 
                  & lead(states) %in% c(unique(df$states)[1:4]), 1,states)) 

#5. Select individuals which received max 30 days of ac-------------------------

lmsm<-
  df %>% 
  select(icustay_id, 
         day=icudayseq_asc, 
         states, 
         sofa_score=sofa_last,
         sofa=sofa_group) %>% 
  group_by(icustay_id) %>% 
  mutate(flag= ifelse(states%in%1,1,0),
         ac_group= sum(flag),
         states= states+1) %>% 
  filter(ac_group<=30) 

statetable.msm(states, icustay_id, data=tut.lmsm) #check results

# 6. Prepare initial state matrix 
# Specify number of states    
nstates<-df %>% ungroup() %>% select(states) %>% distinct(states) %>% nrow()

lqmatrix<-
  matrix((statetable.msm(states, icustay_id, lmsm)/
            rowSums(statetable.msm(states, icustay_id, lmsm))), 
         ncol=nstates[[1]],
         byrow=F)

# Add end states to initial matrix
ltm <- as.matrix(rbind(lqmatrix,
                       c(rep(0,each= (nstates[[1]]))),
                       c(rep(0,each= (nstates[[1]])))))


# 6 Specifying a model----------------------------------------------------------
# Crude estimate consider using as qmatrixs (ie as intial matrix)

lfit.crude<-crudeinits.msm(states ~ day, 
                           icustay_id, 
                           data = lmsm, 
                           qmatrix = ltm)


# Model 1 : Covariates on transition rates--------------------------------------

fit.msm<-msm(states ~ day, 
             icustay_id, 
             data = lmsm, 
             qmatrix = ltm,
             covariates = ~ sofa_score+ac_group,
             exacttimes=TRUE, 
             control=list(fnscale=5000,maxit=500))

# Individual intensity matrices operation---------------------------------------
qmatrix.msm(fit.msm, covariates=list(sofa_score=20, ac_group=1))

#6. Transition matrices --------------------------------------------------------

n_acgroups<-(lmsm %>% ungroup() %>% summarize(max(ac_group)))[[1]]
n_sofascore<-(lmsm %>% ungroup() %>% summarize(max(sofa_score)))[[1]]

tm.list<-lapply(1:n_sofascore, 
                function(y)
                  lapply(1:n_acgroups, 
                         function(x)
                           lapply(1:30, 
                                  function(z)
                                    pmatrix.msm(fit.msm, 
                                                covariates=list(sofa_score=y, 
                                                                ac_group=x),
                                                t=z)
                           )))



#7. Markov objects -------------------------------------------------------------

mo.list<- lapply(1:n_sofascore, 
                 function(y)
                   lapply(1:n_acgroups, 
                          function(x)
                            lapply(1:30, 
                                   function(z)
                                     new("markovchain",
                                         transitionMatrix=
                                           as.matrix.data.frame(tm.list[[y]][[x]][[z]])))))

#8. Non homogenous list of transition matrices----------------------------------

nh_tm_list<-
  lapply(1:n_sofascore, 
         function(y)
           lapply(1:n_acgroups, 
                  function(x)
                    new("markovchainList",
                        markovchains=mo.list[[y]][[x]])))

#9. Non homogenous Marcov simulation -------------------------------------------

npats<-5000

nh_sim_list<-
  lapply(1:length(tm.list), 
         function(y)
           lapply(1:n_acgroups, 
                  function(x)
                    (rmarkovchain(n = npats, #number of patients
                                  object = nh_tm_list[[y]][[x]],
                                  t0 = "2",
                                  include.t0 = TRUE,
                                  parallel = TRUE,
                                  num.cores=detectCores()-1))%>% 
                    group_by(iteration) %>% 
                    mutate(day=as.numeric(streak_run(iteration, k=1000)),
                           sofa=y,
                           ac_care_group=x) %>% 
                    select(patient_id=iteration, 
                           state=values,
                           day,
                           sofa,
                           aggressive_care_group=ac_care_group) %>% 
                    mutate(overall_survival= ifelse(state%in% 5,1,0))
           ))

#10. Bind rows in simulation list for each sofa score --------------------------

u_nhic<-0.66/365 #non aggresive care in icu
u_hic<-0.55/365  #aggressive care in icu
u_cmo<-0.28/365  # cmo
u_discharge<-1/365 # discharge
u_died<-0     #died  
hc_correction<-0.5/365


sim.sofa1<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[1]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa2<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[2]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa3<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[3]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa4<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[4]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa5<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[5]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa6<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[6]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa7<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[7]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa8<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[8]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa9<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[9]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa10<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[10]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa11<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[11]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa12<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[12]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa13<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[13]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa14<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[14]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa15<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[15]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa16<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[16]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa17<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[17]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa18<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[18]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa19<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[19]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa20<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[20]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa21<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[21]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa22<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[22]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa23<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[23]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))
sim.sofa24<-bind_rows(lapply(1:30, function(z) bind_rows(nh_sim_list[[24]][[z]]))) %>% mutate(utility=case_when(state%in%1~u_nhic,state%in%2~u_hic,state%in%3~u_cmo,state%in%4~u_discharge,state%in%5~u_died))

#11 Survival object-------------------------------------------------------------

#sofa 1 
my_survfit.sofa1 = survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa1) 

#sofa 2
my_survfit.sofa2 = survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa2) 

#sofa 3 
my_survfit.sofa3 = survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa3) 

#sofa 4 
my_survfit.sofa4 = survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa4)

#sofa 5 
my_survfit.sofa5 = survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa5)

#sofa 6 
my_survfit.sofa6 = survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa6)

#sofa 7 
my_survfit.sofa7 = survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa7)

#sofa 8 
my_survfit.sofa8 = survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa8)

#sofa 9 
my_survfit.sofa9 = survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa9)

#sofa 10 
my_survfit.sofa10 = survfit(Surv(day, overall_survival)~
                              sofa+aggressive_care_group, data = sim.sofa10)

#sofa 11 
my_survfit.sofa11= survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa11)


#sofa 12 
my_survfit.sofa12= survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa12) 

#sofa 13
my_survfit.sofa13= survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa13) 

#sofa 14 
my_survfit.sofa14= survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa14) 

#sofa 15 
my_survfit.sofa15= survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa15)

#sofa 16 
my_survfit.sofa16= survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa16)

#sofa 17 
my_survfit.sofa17= survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa17)

#sofa 18 
my_survfit.sofa18= survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa18)

#sofa 19 
my_survfit.sofa19= survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa19)

#sofa 20 
my_survfit.sofa20= survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa20)

#sofa 21 
my_survfit.sofa21= survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa21)

#sofa 22 
my_survfit.sofa22= survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa22)

#sofa 23 
my_survfit.sofa23= survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa23)

#sofa 24 
my_survfit.sofa24= survfit(Surv(day, overall_survival)~
                             sofa+aggressive_care_group, data = sim.sofa24)

#12 Analysis--------------------------------------------------------------------

summary(my_survfit.sofa10, times = 30)

survfit(Surv(day, overall_survival)~sofa+aggressive_care_group, data = sim.sofa)

print(tut_survfit.sofa1, rmean= 30)

#12 Survival -----------------------------------------------------------

# Sofa 1
surv.sofa1<-as.data.frame(cbind(x=summary(my_survfit.sofa1,times = 30)[["strata"]],
                                y=summary(my_survfit.sofa1, times = 30)[["surv"]]))
# Sofa 2
surv.sofa2<-as.data.frame(cbind(x=summary(my_survfit.sofa2,times = 30)[["strata"]],
                                y=summary(my_survfit.sofa2, times = 30)[["surv"]]))
# Sofa 3
surv.sofa3<-as.data.frame(cbind(x=summary(my_survfit.sofa3,times = 30)[["strata"]],
                                y=summary(my_survfit.sofa3, times = 30)[["surv"]]))
# Sofa 4
surv.sofa4<-as.data.frame(cbind(x=summary(my_survfit.sofa4,times = 30)[["strata"]],
                                y=summary(my_survfit.sofa4, times = 30)[["surv"]]))
# Sofa 5
surv.sofa5<-as.data.frame(cbind(x=summary(my_survfit.sofa5,times = 30)[["strata"]],
                                y=summary(my_survfit.sofa5, times = 30)[["surv"]]))
# Sofa 6
surv.sofa6<-as.data.frame(cbind(x=summary(my_survfit.sofa6,times = 30)[["strata"]],
                                y=summary(my_survfit.sofa6, times = 30)[["surv"]]))
# Sofa 7
surv.sofa7<-as.data.frame(cbind(x=summary(my_survfit.sofa7,times = 30)[["strata"]],
                                y=summary(my_survfit.sofa7, times = 30)[["surv"]]))
# Sofa 8
surv.sofa8<-as.data.frame(cbind(x=summary(my_survfit.sofa8,times = 30)[["strata"]],
                                y=summary(my_survfit.sofa8, times = 30)[["surv"]]))
# Sofa 9
surv.sofa9<-as.data.frame(cbind(x=summary(my_survfit.sofa9,times = 30)[["strata"]],
                                y=summary(my_survfit.sofa9, times = 30)[["surv"]]))
# Sofa 10
surv.sofa10<-as.data.frame(cbind(x=summary(my_survfit.sofa10,times = 30)[["strata"]],
                                 y=summary(my_survfit.sofa10, times = 30)[["surv"]]))
# Sofa 11
surv.sofa11<-as.data.frame(cbind(x=summary(my_survfit.sofa11,times = 30)[["strata"]],
                                 y=summary(my_survfit.sofa11, times = 30)[["surv"]]))
# Sofa 12
surv.sofa12<-as.data.frame(cbind(x=summary(my_survfit.sofa12,times = 30)[["strata"]],
                                 y=summary(my_survfit.sofa12, times = 30)[["surv"]]))
# Sofa 13
surv.sofa13<-as.data.frame(cbind(x=summary(my_survfit.sofa13,times = 30)[["strata"]],
                                 y=summary(my_survfit.sofa13, times = 30)[["surv"]]))
# Sofa 14
surv.sofa14<-as.data.frame(cbind(x=summary(my_survfit.sofa14,times = 30)[["strata"]],
                                 y=summary(my_survfit.sofa14, times = 30)[["surv"]]))
# Sofa 15
surv.sofa15<-as.data.frame(cbind(x=summary(my_survfit.sofa15,times = 30)[["strata"]],
                                 y=summary(my_survfit.sofa15, times = 30)[["surv"]]))
# Sofa 16
surv.sofa16<-as.data.frame(cbind(x=summary(my_survfit.sofa16,times = 30)[["strata"]],
                                 y=summary(my_survfit.sofa16, times = 30)[["surv"]]))
# Sofa 17
surv.sofa17<-as.data.frame(cbind(x=summary(my_survfit.sofa17,times = 30)[["strata"]],
                                 y=summary(my_survfit.sofa17, times = 30)[["surv"]]))
# Sofa 18
surv.sofa18<-as.data.frame(cbind(x=summary(my_survfit.sofa18,times = 30)[["strata"]],
                                 y=summary(my_survfit.sofa18, times = 30)[["surv"]]))
# Sofa 19
surv.sofa19<-as.data.frame(cbind(x=summary(my_survfit.sofa19,times = 30)[["strata"]],
                                 y=summary(my_survfit.sofa19, times = 30)[["surv"]]))
# Sofa 20
surv.sofa20<-as.data.frame(cbind(x=summary(my_survfit.sofa20,times = 30)[["strata"]],
                                 y=summary(my_survfit.sofa20, times = 30)[["surv"]]))
# Sofa 21
surv.sofa21<-as.data.frame(cbind(x=summary(my_survfit.sofa21,times = 30)[["strata"]],
                                 y=summary(my_survfit.sofa21, times = 30)[["surv"]]))
# Sofa 22
surv.sofa22<-as.data.frame(cbind(x=summary(my_survfit.sofa22,times = 30)[["strata"]],
                                 y=summary(my_survfit.sofa22, times = 30)[["surv"]]))
# Sofa 23
surv.sofa23<-as.data.frame(cbind(x=summary(my_survfit.sofa23,times = 30)[["strata"]],
                                 y=summary(my_survfit.sofa23, times = 30)[["surv"]]))
# Sofa 24
surv.sofa24<-as.data.frame(cbind(x=summary(my_survfit.sofa24,times = 30)[["strata"]],
                                 y=summary(my_survfit.sofa24, times = 30)[["surv"]]))
#13 Sofa comparison-------------------------------------------------------------

# quick visualization: basically shows the expected descrease in survival
# accross sofa scores. But also shows overlap 
ggplot()+
  geom_smooth(data=surv.sofa1, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa2, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa3, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa4, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa5, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa6, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa7, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa8, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa9, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa10, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa11, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa12, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa13, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa14, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa15, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa16, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa17, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa18, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa19, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa20, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa21, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa22, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa23, aes(x=x,y=y), color="red",se = F)+
  geom_smooth(data=surv.sofa24, aes(x=x,y=y), color="red",se = F)




################################################################################


mean(summary(my_survfit.sofa1, times = 3)[[6]])

1−121/228)×100=47%

(1-28/3100)*100

plot(survfit(Surv(day, os) ~ ac_factor, data = mc.sofa4), 
     xlab = "Days", 
     ylab = "Overall survival probability")

ggsurvplot(
  fit = survfit(Surv(day, os) ~ as.numeric(ac_factor), data = survival.sofa1), 
  xlab = "Days", 
  ylab = "Overall survival probability")

test<-summary(my_survfit)

0.1360195, 0.1955093,0.1610899,0.1116790,0.1021441, 0.2264263,0.1045519 0.1038625 0.1025447 0.3464626 0.1955093 0.1092541 0.1610899 0.1116790 0.2255736 0.1021441
[12] 0.1045519

ggplot(data=as.data.frame(cbind(test[["surv"]],as.numeric(test[["strata"]]))), aes(y=V1,x=V2))+
  geom_point()+
  geom_smooth()
as.data.frame(cbind(test[["surv"]],as.numeric(test[["strata"]])))


# singular test
# matrix((replicate(2,{
# rmarkovchain(n = 1, #length of the chain ie number of d. after aggressive care (fixed here)
#              object = mo.sofa1[[1]][[1]],
#              parallel = TRUE,
#              num.cores=detectCores()-1)})), ncol=1)
# 
npats<-1000 # number of patients 

#sofa 1
mc.sofa1<-    
  lapply(1:length(mo.sofa1), function(y) # days of aggressive care received
    lapply(1:length(mo.sofa1[[1]]), function(x) # determines the transition matrix for each day 
      matrix((replicate(npats, {     # number of patients
        rmarkovchain(n = 1, #length of the chain ie number of d. after aggressive care (fixed here)
                     object = mo.sofa1[[1]],
                     parallel = TRUE,
                     num.cores=detectCores()-1)})), ncol=1)))
#sofa 2
mc.sofa2<-    
  lapply(1:length(mo.sofa2), function(y) # days of aggressive care received
    lapply(1:length(mo.sofa2[[1]]), function(x) # determines the transition matrix for each day 
      matrix((replicate(npats, {     # number of patients
        rmarkovchain(n = 1, #length of the chain ie number of d. after aggressive care (fixed here)
                     object = mo.sofa2[[y]][[x]],
                     parallel = TRUE,
                     num.cores=detectCores()-1)})), ncol=1)))
#sofa 3
mc.sofa3<-    
  lapply(1:length(mo.sofa3), function(y) # days of aggressive care received
    lapply(1:length(mo.sofa3[[1]]), function(x) # determines the transition matrix for each day 
      matrix((replicate(npats, {     # number of patients
        rmarkovchain(n = 1, #length of the chain ie number of d. after aggressive care (fixed here)
                     object = mo.sofa3[[y]][[x]],
                     parallel = TRUE,
                     num.cores=detectCores()-1)})), ncol=1)))
#sofa4 
mc.sofa4<-    
  lapply(1:length(mo.sofa4), function(y) # days of aggressive care received
    lapply(1:length(mo.sofa4[[1]]), function(x) # determines the transition matrix for each day 
      matrix((replicate(npats, {     # number of patients
        rmarkovchain(n = 1, #length of the chain ie number of d. after aggressive care (fixed here)
                     object = mo.sofa4[[y]][[x]],
                     parallel = TRUE,
                     num.cores=detectCores()-1)})), ncol=1)))

#-------------------------------------------------------------------------------
#8

#8. Convert marcov chain list of list into a matrix chain-------------------------
# note: needs to be made recursive
#x  select days of aggressive care received 
#y  days in icu after agressive care

# definitions

#fit.msm1,  
# fit.msm2,
# fit.msm3,
# fit.msm5,
# fit.msm6,
# fit.msm7,
# fit.msm9,
# fit.msm11,
# fit.msm16,
# fit.msm17,
# fit.msm18,
# fit.msm20


#sofa1 
matrixchain.sofa1<-
  lapply(1:length(mc.sofa1), function(y)
    cbind(          #x    y    z
      matrix(mc.sofa1[[1]][[1]]),
      matrix(mc.sofa1[[1]][[2]]),
      matrix(mc.sofa1[[1]][[3]]),
      matrix(mc.sofa1[[1]][[4]]),
      matrix(mc.sofa1[[1]][[5]]),
      matrix(mc.sofa1[[1]][[6]]),
      matrix(mc.sofa1[[1]][[7]]),
      matrix(mc.sofa1[[1]][[8]]),
      matrix(mc.sofa1[[1]][[9]]),
      matrix(mc.sofa1[[1]][[10]]),
      matrix(mc.sofa1[[1]][[11]]),
      matrix(mc.sofa1[[1]][[12]]),
      matrix(mc.sofa1[[1]][[13]]),
      matrix(mc.sofa1[[1]][[14]]),
      matrix(mc.sofa1[[1]][[15]]),
      matrix(mc.sofa1[[1]][[16]]),
      matrix(mc.sofa1[[1]][[17]]),
      matrix(mc.sofa1[[1]][[18]]),
      matrix(mc.sofa1[[1]][[19]]),
      matrix(mc.sofa1[[1]][[20]]),
      matrix(mc.sofa1[[1]][[21]]),
      matrix(mc.sofa1[[1]][[22]]),
      matrix(mc.sofa1[[1]][[23]]),
      matrix(mc.sofa1[[1]][[24]]),
      matrix(mc.sofa1[[1]][[25]]),
      matrix(mc.sofa1[[1]][[26]]),
      matrix(mc.sofa1[[1]][[27]]),
      matrix(mc.sofa1[[1]][[28]]),
      matrix(mc.sofa1[[1]][[29]]),
      matrix(mc.sofa1[[1]][[30]])
    )
  )


#sofa2 
matrixchain.sofa2<-
  lapply(1:length(mc.sofa2), function(y)
    cbind(          #x    y    z
      matrix(mc.sofa2[[1]][[1]]),
      matrix(mc.sofa2[[1]][[2]]),
      matrix(mc.sofa2[[1]][[3]]),
      matrix(mc.sofa2[[1]][[4]]),
      matrix(mc.sofa2[[1]][[5]]),
      matrix(mc.sofa2[[1]][[6]]),
      matrix(mc.sofa2[[1]][[7]]),
      matrix(mc.sofa2[[1]][[8]]),
      matrix(mc.sofa2[[1]][[9]]),
      matrix(mc.sofa2[[1]][[10]]),
      matrix(mc.sofa2[[1]][[11]]),
      matrix(mc.sofa2[[1]][[12]]),
      matrix(mc.sofa2[[1]][[13]]),
      matrix(mc.sofa2[[1]][[14]]),
      matrix(mc.sofa2[[1]][[15]]),
      matrix(mc.sofa2[[1]][[16]]),
      matrix(mc.sofa2[[1]][[17]]),
      matrix(mc.sofa2[[1]][[18]]),
      matrix(mc.sofa2[[1]][[19]]),
      matrix(mc.sofa2[[1]][[20]]),
      matrix(mc.sofa2[[1]][[21]]),
      matrix(mc.sofa2[[1]][[22]]),
      matrix(mc.sofa2[[1]][[23]]),
      matrix(mc.sofa2[[1]][[24]]),
      matrix(mc.sofa2[[1]][[25]]),
      matrix(mc.sofa2[[1]][[26]]),
      matrix(mc.sofa2[[1]][[27]]),
      matrix(mc.sofa2[[1]][[28]]),
      matrix(mc.sofa2[[1]][[29]]),
      matrix(mc.sofa2[[1]][[30]])
    )
  )


#sofa3
matrixchain.sofa3<-
  lapply(1:length(mc.sofa3), function(y)
    cbind(          #x    y    z
      matrix(mc.sofa3[[1]][[1]]),
      matrix(mc.sofa3[[1]][[2]]),
      matrix(mc.sofa3[[1]][[3]]),
      matrix(mc.sofa3[[1]][[4]]),
      matrix(mc.sofa3[[1]][[5]]),
      matrix(mc.sofa3[[1]][[6]]),
      matrix(mc.sofa3[[1]][[7]]),
      matrix(mc.sofa3[[1]][[8]]),
      matrix(mc.sofa3[[1]][[9]]),
      matrix(mc.sofa3[[1]][[10]]),
      matrix(mc.sofa3[[1]][[11]]),
      matrix(mc.sofa3[[1]][[12]]),
      matrix(mc.sofa3[[1]][[13]]),
      matrix(mc.sofa3[[1]][[14]]),
      matrix(mc.sofa3[[1]][[15]]),
      matrix(mc.sofa3[[1]][[16]]),
      matrix(mc.sofa3[[1]][[17]]),
      matrix(mc.sofa3[[1]][[18]]),
      matrix(mc.sofa3[[1]][[19]]),
      matrix(mc.sofa3[[1]][[20]]),
      matrix(mc.sofa3[[1]][[21]]),
      matrix(mc.sofa3[[1]][[22]]),
      matrix(mc.sofa3[[1]][[23]]),
      matrix(mc.sofa3[[1]][[24]]),
      matrix(mc.sofa3[[1]][[25]]),
      matrix(mc.sofa3[[1]][[26]]),
      matrix(mc.sofa3[[1]][[27]]),
      matrix(mc.sofa3[[1]][[28]]),
      matrix(mc.sofa3[[1]][[29]]),
      matrix(mc.sofa3[[1]][[30]])
    )
  )


#sofa4 
matrixchain.sofa4<-
  lapply(1:length(mc.sofa4), function(y)
    cbind(          #x    y    z
      matrix(mc.sofa4[[1]][[1]]),
      matrix(mc.sofa4[[1]][[2]]),
      matrix(mc.sofa4[[1]][[3]]),
      matrix(mc.sofa4[[1]][[4]]),
      matrix(mc.sofa4[[1]][[5]]),
      matrix(mc.sofa4[[1]][[6]]),
      matrix(mc.sofa4[[1]][[7]]),
      matrix(mc.sofa4[[1]][[8]]),
      matrix(mc.sofa4[[1]][[9]]),
      matrix(mc.sofa4[[1]][[10]]),
      matrix(mc.sofa4[[1]][[11]]),
      matrix(mc.sofa4[[1]][[12]]),
      matrix(mc.sofa4[[1]][[13]]),
      matrix(mc.sofa4[[1]][[14]]),
      matrix(mc.sofa4[[1]][[15]]),
      matrix(mc.sofa4[[1]][[16]]),
      matrix(mc.sofa4[[1]][[17]]),
      matrix(mc.sofa4[[1]][[18]]),
      matrix(mc.sofa4[[1]][[19]]),
      matrix(mc.sofa4[[1]][[20]]),
      matrix(mc.sofa4[[1]][[21]]),
      matrix(mc.sofa4[[1]][[22]]),
      matrix(mc.sofa4[[1]][[23]]),
      matrix(mc.sofa4[[1]][[24]]),
      matrix(mc.sofa4[[1]][[25]]),
      matrix(mc.sofa4[[1]][[26]]),
      matrix(mc.sofa4[[1]][[27]]),
      matrix(mc.sofa4[[1]][[28]]),
      matrix(mc.sofa4[[1]][[29]]),
      matrix(mc.sofa4[[1]][[30]])
    )
  )
#-------------------------------------------------------------------------------
#30-d survival




#-------------------------------------------------------------------------------

#9. Adjust the length of the days of care received after aggressive care
# for example if someone already received 2 days of ac then they should have a
# total of 28d left to receive of more care (since we are rounding at 30)

#days of aggressive care with enough data to run simulation
days_of_ac<- c(1,2,3,5,6,7,9,11,16,17,18,20)
index_of_ac<-c(1,2,3,4,5,6,7,8,9,10,11,12)

#sofa1

sims.sofa1<-lapply(index_of_ac, function(y)lapply(days_of_ac, function(x) matrixchain.sofa1[[y]][, 1:(30-x)]))

u_nhic<-0.66/365 #non aggresive care in icu
u_hic<-0.55/365  #aggressive care in icu
u_cmo<-0.28/365  # cmo
u_discharge<-1/365 # discharge
u_died<-0     #died  
hc_correction<-0.5/365

days_of_ac<- c(1,2,3,5,6,7,9,11,16,17,18,20)
index_of_ac<-c(1,2,3,4,5,6,7,8,9,10,11,12)

#day1
d1.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[1]][[1]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))
#day2
d2.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[2]][[2]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))
#day3
d3.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[3]][[3]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))

#day5
d5.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[4]][[4]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))
#day6
d6.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[5]][[5]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))
#day7
d7.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[6]][[6]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))

#day9
d9.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[7]][[7]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))
#day11
d11.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[8]][[8]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))
#day16
d16.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[9]][[9]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))
#day17
d17.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[10]][[10]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))
#day18
d18.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[11]][[11]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))
#day20
d20.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[12]][[12]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))




















#-------------------------------------------------------------------------------


#day1
d1.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[1]][[1]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))
#day2
d2.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[2]][[2]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))
#day3
d3.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[3]][[3]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))

#day5
d5.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[4]][[4]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))
#day6
d6.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[5]][[5]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))
#day7
d7.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[6]][[6]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))

#day9
d9.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[7]][[7]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))
#day11
d11.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[8]][[8]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))
#day16
d16.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[9]][[9]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))
#day17
d17.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[10]][[10]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))
#day18
d18.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[11]][[11]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))
#day20
d20.sofa1  <-colMeans(as.matrix(as.data.frame(
  sims.sofa1[[12]][[12]]) %>% 
    mutate_if(~2>0,
              ~case_when(.x %in% 1~u_nhic,
                         .x %in% 2~u_cmo,
                         .x %in% 3~u_discharge,
                         .x %in% 4~u_died))))



(((sum(
  colSums(
    as.matrix(
      as.data.frame(
        sims.sofa1[[12]][[12]]) %>% 
        mutate_if(~2>0,
                  ~case_when(.x %in% 1~u_nhic,
                             .x %in% 2~u_cmo,
                             .x %in% 3~u_discharge,
                             .x %in% 4~u_died))))+
    u_hic*(30-10)))/npats)+
    hc_correction)


#10. Utilities per day----------------------------------------------------------
u_nhic<-0.66/365 #non aggresive care in icu
u_hic<-0.55/365  #aggressive care in icu
u_cmo<-0.28/365  # cmo
u_discharge<-1/365 # discharge
u_died<-0     #died  
hc_correction<-0.5/365



# sofa 1
u.sofa1 <- 
  lapply(1:length(matrixchain.sofa1), 
         function(x)
           as.tibble(matrixchain.sofa1[[x]]) %>% 
           tibble::rowid_to_column("ID") %>% 
           gather(day, value=state, V1:V30) %>% 
           arrange(ID) %>% 
           mutate(los_days=  str_replace_all(day,pattern = "[a-zA-Z ]", replacement="")) %>% 
           select(-(day)) %>% 
           mutate(aggressive_care_tx_group= x) %>%  #update with index
           # mutate(col1=streak_run(state, k=1000)) %>%  #remove redundant days 
           # filter(!(state=="4" & col1!=1)) %>%         #to stop utility from counting after reaching absorption state
           # filter(!(state=="5" & col1!=1)) %>%         3if requiered
           mutate(utility= case_when(state %in% '1'~ u_nhic,
                                     state %in% '2'~ u_hic,
                                     state %in% '3'~ u_cmo,
                                     state %in% '4'~ u_discharge,
                                     state %in% '5'~ u_died)) %>% 
           group_by(los_days=as.numeric(los_days), ID) %>% 
           summarize(utility= sum(utility)+(length(ID)*u_nhic)) %>% 
           group_by(as.numeric(los_days)) %>% 
           summarize(total_utility= sum(utility)) %>% 
           ungroup() %>% 
           mutate(cum_utility=cumsum(total_utility)) %>% 
           ungroup() %>% 
           mutate(qald=cum_utility/N,
                  total_qald=sum(qald)+hc_correction) %>% 
           mutate(aggressive_care_tx_group= x)
  )

u.sofa1<-do.call("rbind", u.sofa1)

# sofa 2
u.sofa2 <- 
  lapply(1:length(matrixchain.sofa2), 
         function(x)
           as.tibble(matrixchain.sofa2[[x]]) %>% 
           tibble::rowid_to_column("ID") %>% 
           gather(day, value=state, V1:V30) %>% 
           arrange(ID) %>% 
           mutate(los_days=  str_replace_all(day,pattern = "[a-zA-Z ]", replacement="")) %>% 
           select(-(day)) %>% 
           mutate(aggressive_care_tx_group= x) %>%  #update with index
           # mutate(col1=streak_run(state, k=1000)) %>%  #remove redundant days 
           # filter(!(state=="4" & col1!=1)) %>%         #to stop utility from counting after reaching absorption state
           # filter(!(state=="5" & col1!=1)) %>%         3if requiered
           mutate(utility= case_when(state %in% '1'~ u_nhic,
                                     state %in% '2'~ u_hic,
                                     state %in% '3'~ u_cmo,
                                     state %in% '4'~ u_discharge,
                                     state %in% '5'~ u_died)) %>% 
           group_by(los_days=as.numeric(los_days), ID) %>% 
           summarize(utility= sum(utility)+(length(ID)*u_nhic)) %>% 
           group_by(as.numeric(los_days)) %>% 
           summarize(total_utility= sum(utility)) %>% 
           ungroup() %>% 
           mutate(cum_utility=cumsum(total_utility)) %>% 
           ungroup() %>% 
           mutate(qald=cum_utility/N,
                  total_qald=sum(qald)+hc_correction) %>% 
           mutate(aggressive_care_tx_group= x)
  )

u.sofa2<-do.call("rbind", u.sofa2)


# sofa 3
u.sofa3 <- 
  lapply(1:length(matrixchain.sofa3), 
         function(x)
           as.tibble(matrixchain.sofa3[[x]]) %>% 
           tibble::rowid_to_column("ID") %>% 
           gather(day, value=state, V1:V30) %>% 
           arrange(ID) %>% 
           mutate(los_days=  str_replace_all(day,pattern = "[a-zA-Z ]", replacement="")) %>% 
           select(-(day)) %>% 
           mutate(aggressive_care_tx_group= x) %>%  #update with index
           # mutate(col1=streak_run(state, k=1000)) %>%  #remove redundant days 
           # filter(!(state=="4" & col1!=1)) %>%         #to stop utility from counting after reaching absorption state
           # filter(!(state=="5" & col1!=1)) %>%         3if requiered
           mutate(utility= case_when(state %in% '1'~ u_nhic,
                                     state %in% '2'~ u_hic,
                                     state %in% '3'~ u_cmo,
                                     state %in% '4'~ u_discharge,
                                     state %in% '5'~ u_died)) %>% 
           group_by(los_days=as.numeric(los_days), ID) %>% 
           summarize(utility= sum(utility)+(length(ID)*u_hic)) %>% 
           group_by(as.numeric(los_days)) %>% 
           summarize(total_utility= sum(utility)) %>% 
           ungroup() %>% 
           mutate(cum_utility=cumsum(total_utility)) %>% 
           ungroup() %>% 
           mutate(qald=cum_utility/N,
                  total_qald=sum(qald)+hc_correction) %>% 
           mutate(aggressive_care_tx_group= x)
  )

u.sofa3<-do.call("rbind", u.sofa3)


# sofa 4
u.sofa4 <- 
  lapply(1:length(matrixchain.sofa4), 
         function(x)
           as.tibble(matrixchain.sofa4[[x]]) %>% 
           tibble::rowid_to_column("ID") %>% 
           gather(day, value=state, V1:V30) %>% 
           arrange(ID) %>% 
           mutate(los_days=  str_replace_all(day,pattern = "[a-zA-Z ]", replacement="")) %>% 
           select(-(day)) %>% 
           mutate(aggressive_care_tx_group= x) %>%  #update with index
           # mutate(col1=streak_run(state, k=1000)) %>%  #remove redundant days 
           # filter(!(state=="4" & col1!=1)) %>%         #to stop utility from counting after reaching absorption state
           # filter(!(state=="5" & col1!=1)) %>%         3if requiered
           mutate(utility= case_when(state %in% '1'~ u_nhic,
                                     state %in% '2'~ u_hic,
                                     state %in% '3'~ u_cmo,
                                     state %in% '4'~ u_discharge,
                                     state %in% '5'~ u_died)) %>% 
           group_by(los_days=as.numeric(los_days), ID) %>% 
           summarize(utility= sum(utility)+(length(ID)*u_hic)) %>% 
           group_by(as.numeric(los_days)) %>% 
           summarize(total_utility= sum(utility)) %>% 
           ungroup() %>% 
           mutate(cum_utility=cumsum(total_utility)) %>% 
           ungroup() %>% 
           mutate(qald=cum_utility/N,
                  total_qald=sum(qald)+hc_correction) %>% 
           mutate(aggressive_care_tx_group= x)
  )

u.sofa4<-do.call("rbind", u.sofa4)

#10.Max utility per aggressive regimen over 30 days by sofa group---------------

max_qald.sofa1<-u.sofa1 %>% 
  select(los_days=`as.numeric(los_days)`,total_qald,aggressive_care_tx_group) %>% 
  group_by(aggressive_care_tx_group) %>% 
  summarize(total_qald=max(total_qald))%>% 
  mutate(xaxis= case_when(aggressive_care_tx_group %in% 1~2,
                          aggressive_care_tx_group %in% 2~6,
                          #aggressive_care_tx_group %in% 3~8,
                          aggressive_care_tx_group %in% 3~10,
                          aggressive_care_tx_group %in% 4~11,
                          aggressive_care_tx_group %in% 5~12,
                          aggressive_care_tx_group %in% 6~13,
                          aggressive_care_tx_group %in% 7~14,
                          #aggressive_care_tx_group %in% 9~17,
                          aggressive_care_tx_group %in% 8~20,
                          aggressive_care_tx_group %in% 9~21,
                          aggressive_care_tx_group %in% 10~27,
                          aggressive_care_tx_group %in% 11~29,
                          aggressive_care_tx_group %in% 12~30))

max_qald.sofa2<-u.sofa2 %>% 
  select(los_days=`as.numeric(los_days)`,total_qald,aggressive_care_tx_group) %>% 
  group_by(aggressive_care_tx_group) %>% 
  summarize(total_qald=max(total_qald))%>% 
  mutate(xaxis= case_when(aggressive_care_tx_group %in% 1~2,
                          aggressive_care_tx_group %in% 2~6,
                          #aggressive_care_tx_group %in% 3~8,
                          aggressive_care_tx_group %in% 3~10,
                          aggressive_care_tx_group %in% 4~11,
                          aggressive_care_tx_group %in% 5~12,
                          aggressive_care_tx_group %in% 6~13,
                          aggressive_care_tx_group %in% 7~14,
                          #aggressive_care_tx_group %in% 9~17,
                          aggressive_care_tx_group %in% 8~20,
                          aggressive_care_tx_group %in% 9~21,
                          aggressive_care_tx_group %in% 10~27,
                          aggressive_care_tx_group %in% 11~29,
                          aggressive_care_tx_group %in% 12~30))

max_qald.sofa3<-u.sofa3 %>% 
  select(los_days=`as.numeric(los_days)`,total_qald,aggressive_care_tx_group) %>% 
  group_by(aggressive_care_tx_group) %>% 
  summarize(total_qald=max(total_qald))%>% 
  mutate(xaxis= case_when(aggressive_care_tx_group %in% 1~2,
                          aggressive_care_tx_group %in% 2~6,
                          #aggressive_care_tx_group %in% 3~8,
                          aggressive_care_tx_group %in% 3~10,
                          aggressive_care_tx_group %in% 4~11,
                          aggressive_care_tx_group %in% 5~12,
                          aggressive_care_tx_group %in% 6~13,
                          aggressive_care_tx_group %in% 7~14,
                          #aggressive_care_tx_group %in% 9~17,
                          aggressive_care_tx_group %in% 8~20,
                          aggressive_care_tx_group %in% 9~21,
                          aggressive_care_tx_group %in% 10~27,
                          aggressive_care_tx_group %in% 11~29,
                          aggressive_care_tx_group %in% 12~30))

max_qald.sofa4<-u.sofa4 %>% 
  select(los_days=`as.numeric(los_days)`,total_qald,aggressive_care_tx_group) %>% 
  group_by(aggressive_care_tx_group) %>% 
  summarize(total_qald=max(total_qald)) %>% 
  mutate(xaxis= case_when(aggressive_care_tx_group %in% 1~2,
                          aggressive_care_tx_group %in% 2~6,
                          #aggressive_care_tx_group %in% 3~8,
                          aggressive_care_tx_group %in% 3~10,
                          aggressive_care_tx_group %in% 4~11,
                          aggressive_care_tx_group %in% 5~12,
                          aggressive_care_tx_group %in% 6~13,
                          aggressive_care_tx_group %in% 7~14,
                          #aggressive_care_tx_group %in% 9~17,
                          aggressive_care_tx_group %in% 8~20,
                          aggressive_care_tx_group %in% 9~21,
                          aggressive_care_tx_group %in% 10~27,
                          aggressive_care_tx_group %in% 11~29,
                          aggressive_care_tx_group %in% 12~30))

# 11. Graphs --------------------------------------------------------------

ggplot(max_qald.sofa1,aes(x=xaxis, y=total_qald))+
  geom_point()+
  geom_smooth(span = 0.25, color='#e7298a')+
  #geom_line()+
  theme_bw()+
  scale_x_continuous(breaks = scales::pretty_breaks(n=30))+
  xlab('Days of aggresive treatment')+
  ylab('Quality adjusted life days')+
  labs(subtitle = "SOFA 1")


ggplot(max_qald.sofa2,aes(x=xaxis, y=total_qald))+
  geom_point()+
  geom_smooth(span = 0.25, color='#fdc086')+
  #geom_line()+
  theme_bw()+
  scale_x_continuous(breaks = scales::pretty_breaks(n=30))+
  xlab('Days of aggresive treatment')+
  ylab('Quality adjusted life days')+
  labs(subtitle = "SOFA 2")

ggplot(max_qald.sofa3,aes(x=xaxis, y=total_qald))+
  geom_point()+
  geom_smooth(span = 0.25,color='#beaed4')+
  #geom_line()+
  theme_bw()+
  scale_x_continuous(breaks = scales::pretty_breaks(n=30))+
  xlab('Days of aggresive treatment')+
  ylab('Quality adjusted life days')+
  labs(subtitle = "SOFA 3")

ggplot(max_qald.sofa4,aes(x=xaxis, y=total_qald))+
  geom_point(color='grey10')+
  geom_smooth(span = 0.25, color='#386cb0')+
  theme_bw()+
  scale_x_continuous(breaks = scales::pretty_breaks(n=30))+
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
  xlab('Days of aggresive treatment')+
  ylab('Quality adjusted life days')+
  labs(subtitle = "SOFA 4")


# To do/notes:------------------------------------------------------------------

# 1) Create a reproducible pipeline for obtaining cohort via sql
# 2) Be more specific with states, ie track time more adequately and extract states more precisely
# 3) Redefine CMO from the sql stage
# 3) A distribution of utilities can be used (eg norm(start,end, etc)) to create ranges for utilities as opposed to fix value
# therefore review utilites and add in ranges (secondary sensitivity analysis)
# 4) multisimulation matrices can also be introduced

