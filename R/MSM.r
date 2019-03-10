# Load library
library("tidyr")
library("dplyr")
library("markovchain")
library("tdisplay", lib.loc="~/R/win-library/3.5")
library("survival", lib.loc="~/R/win-library/3.5")
library("survminer", lib.loc="~/R/win-library/3.5")
library("parallel")
library("stringr")

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
q.sofa<-quantile(sofa$sofa_last)

# Results:
# 0%  25%  50%  75% 100% 
# 0    4    7   10.5   22 
# Q1: 0-4
# Q2: 5-7
# Q3: 8-10.5
# Q4: 11-22

# Assign cohort groups to sofa data frame

sofa<-sofa %>% mutate(sofa_group= case_when(sofa_last %in% c(q.sofa[[1]]:q.sofa[[2]])~"Q1",
                                      sofa_last %in% c(q.sofa[[2]]+1:q.sofa[[3]])~"Q2",
                                      sofa_last %in% c(q.sofa[[3]]+1:)~"Q3",
                                      sofa_last %in% c(q.sofa[[4]]+0.5:q.sofa[[5]])~"Q4")) 
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

#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#

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
table(df.q1$gender)
summary(df.q1$admission_age)
table(df.q1$death_flag)
length(unique(df.q1$icustay_id))
#Q2#########################
table(df.q2$gender)
summary(df.q2$admission_age)
table(df.q2$death_flag)
length(unique(df.q2$icustay_id))
#Q3#########################
table(df.q3$gender)
summary(df.q3$admission_age)
table(df.q3$death_flag)
length(unique(df.q3$icustay_id))
#Q4#########################
table(df.q4$gender)
summary(df.q4$admission_age)
table(df.q4$death_flag)
length(unique(df.q4$icustay_id))

# How many days on average did they spend in each unit

#Q1#########################
table(df.q1$states)[1:3]
#Q2#########################
table(df.q2$states)[1:3]
#Q3#########################
table(df.q3$states)[1:3]
#Q4#########################
table(df.q4$states)[1:3]

# Obtain the last day metrics for each quantile sofa group
df.q1.lastrow<-df.q1 %>% group_by(icustay_id) %>% filter(icudayseq_asc %in% max(icudayseq_asc))
df.q2.lastrow<-df.q2 %>% group_by(icustay_id) %>% filter(icudayseq_asc %in% max(icudayseq_asc))
df.q3.lastrow<-df.q3 %>% group_by(icustay_id) %>% filter(icudayseq_asc %in% max(icudayseq_asc))
df.q4.lastrow<-df.q4 %>% group_by(icustay_id) %>% filter(icudayseq_asc %in% max(icudayseq_asc))

# Last row statistics for each quantile sofa group

#Q1#########################
summary(df.q1.lastrow$icudayseq_asc)
summary(df.q1.lastrow$sofa_last)
#Q2#########################
summary(df.q2.lastrow$icudayseq_asc)
summary(df.q2.lastrow$sofa_last)
#Q3#########################
summary(df.q3.lastrow$icudayseq_asc)
summary(df.q3.lastrow$sofa_last)
#Q4#########################
summary(df.q4.lastrow$icudayseq_asc)
summary(df.q4.lastrow$sofa_last)

rm(df.q1.lastrow)
rm(df.q2.lastrow)
rm(df.q3.lastrow)
rm(df.q4.lastrow)

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
