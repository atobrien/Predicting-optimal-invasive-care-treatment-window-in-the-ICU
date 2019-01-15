# Load library
library("tidyr")
library("dplyr")
library("markovchain")
library("tdisplay", lib.loc="~/R/win-library/3.5")
library("survival", lib.loc="~/R/win-library/3.5")
library("survminer", lib.loc="~/R/win-library/3.5")
library("parallel")

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

# https://www.coursera.org/lecture/foundations-marketing-analytics/how-to-compute-a-transition-matrix-in-r-recital-1-jD6xC

# TRANSITION MATRIX

mysequence<-df$states
createSequenceMatrix(mysequence)

df<- df %>% mutate(seq= ifelse(states %in% c(3,4), NA, lead_states)) 

transition<- df %>% select(states, seq) %>% filter(!is.na(seq))
transition<- df %>% mutate(states= as.factor(states), 
                           seq= as.factor(seq))
tm<-table(transition$states, transition$seq)
tmA<-tm/rowSums(tm)
attributes(tmA)$class <- "matrix"  #problem is that it creates NA 
tmA[tmA == 'NaN'] <- 0 #problem is that it creates NA

# Given the above problem I write the matrix by hand and define the absorbing
# states by using the intersection of the from-to with a probability of 1
tmA <- matrix(c(0.56735751, 0.1658031, 0.002590674, 0.2577720, 0.006476684,
                0.04582022, 0.8674362, 0.001748863, 0.0664568, 0.018537950,
                0.00000000, 0.0000000, 0.000000000, 0.5000000, 0.500000000,
                0.00000000, 0.0000000, 0.000000000, 1.0000000, 0.000000000,
                0.00000000, 0.0000000, 0.000000000, 0.0000000, 1.000000000)
              ,nrow = 5, byrow = TRUE)

dtmcA <- new("markovchain",
             transitionMatrix=tmA, 
             states=c("ICU","Invasive","CMO","Discharged", "Death"), 
             name="MarkovChain A") 

dtmcA

plot(dtmcA, main = "Transition probability matrix for DBM")
library(diagram)


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



initialState <- c(0, 1, 0, 0, 0)
simDays <- initialState * (dtmcA^3)
simDays/rowSums(simDays)
canonicForm(dtmcA)
detectCores()








SIMULATION & prediction
weathersOfDays <- rmarkovchain(n = 3, 
                               object = dtmcA, 
                               t0 = "ICU",
                               include.t0 = TRUE,
                               parallel = TRUE)
(as.data.frame(weathersOfDays))

predict(object = dtmcA, newdata = c("ICU", "ICU", "ICU"), n.ahead = 50)








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









################################################################################




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
################################################################################
## Extra































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
