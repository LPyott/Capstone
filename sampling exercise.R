###Create stratification variable using AGE
library(dplyr)
GSS=read.csv("GSS.csv", header=T)

#GSS is the population
#Calculate summaries for each variable: age, hours, sex, race, income, health
summary(GSS$AGE)
table(GSS$AGE)

GSS <- tibble::rowid_to_column(GSS, "ID")

#####################################################
#Simple random sample
rand_df <- GSS[sample(nrow(GSS), size=500), ]                                   


#####################################################
#systematic sample
#create function first
#Find of pop/sample ratio and use that as the "jump"
obtain_sys = function(N,n){
  k = ceiling(N/n)
  r = sample(1:k, 1)
  seq(r, r + k*(n-1), k)
}
#apply function
sys_sample_df = GSS[obtain_sys(nrow(GSS), 500), ]

####################################################
#Stratified sample
#Create stratification variable on AGE called agecat

GSS=GSS %>%
  mutate(agecat=case_when(AGE<18~"No answer",
                          AGE==18 | AGE==19~"18 and 19",
                          AGE>19 & AGE<30~"20 to 29",
                          AGE>29 & AGE<40~"30 to 39",
                          AGE>39 & AGE<50~"40 to 49",
                          AGE>49 & AGE<60~"50 to 59",
                          AGE>59 & AGE<70~"60 to 69",
                          AGE>69 & AGE<80~"70 to 79",
                          AGE>79 & AGE<90~"80 to 89",
  ))

#obtain stratified samples
strat_sample_1 <- GSS %>%
  group_by(agecat) %>%
  sample_n(size=60)

500/72390
strat_sample_2 <- GSS %>%
  group_by(agecat) %>%
  sample_frac(size=.0069)
