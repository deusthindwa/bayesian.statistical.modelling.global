#Multilevel Bayesian Analysis of the predictors of Serial Monogamy and Concurrent Sexual Partnership 
#written by Deus Thindwa
#01/07/2019

#====================LOAD REQUIRED PACKAGES AND DATASET====================

DDHP.packages <-c("foreign","tidyverse","janitor","readstata13","rethinking","rstan","plyr","DataCombine")
lapply(DDHP.packages, library, character.only=TRUE)

#load male questionnaire csv
male.label <-as_tibble(read.dta13("/Users/dthindwa/Rproject/drivenHIV/data/male.dta"))

#subset the dataset to get all potential covariates
male.label <-select(male.label,mv766b,mv854a,mv001,mv002,mv012,mv025,mv106,mv731,mv130,mv190,mv167,mv213,mv761,mv483,
                 mv501,mv525,mv602,mv605,mv754cp,mv754dp,mv770,mv826a,mv793,mv793a,hiv03,mv822,mv834a)

#rename variables for appropriate use in the models
colnames(male.label) <-c("mcsp","csp","clustno","houseno","age","resid","educ","employ","rel","windex","travel",
                       "partpreg","condom","mmc","mstatus","agesex","fertprof","fertpref",
                       "hiv_condoms","hiv_1part","stdcounsel","hivtest","paidsex","paidsexcondom","hivres","sexinfl","agepart")

#====================RECODE OUTCOME VARIABLES AND POTENTIAL COVARIATES====================

#MCSP (multiple and concurrent sexual partnership) = SM (serial monogamy) + CSP (concurrent sexual partnership)
male.label <-subset(male.label,mcsp>=1)
male.label$mcsp <-if_else(male.label$mcsp==1,0,1)

#Define SM (serial monogamy)
male.label$sm <-if_else(is.na(male.label$csp),0L,if_else(male.label$csp=="no",1L,NULL))
male.label %>% tabyl(sm, show_na=FALSE) %>% adorn_pct_formatting(digits=1)

#Define CSP (concurrent sexual partnership)
male.label$csp <-if_else(is.na(male.label$csp),0L,if_else(male.label$csp=="yes",1L,NULL))
male.label %>% tabyl(csp, show_na=FALSE) %>% adorn_pct_formatting(digits=1)

#Respondent male age
male.label$agegp <-if_else(male.label$age>=15 & male.label$age<20,1,if_else(male.label$age>=20 & male.label$age<30,2,3))
male.label$agegp <-recode_factor(male.label$agegp,`1`="15-19",`2`="20-29",`3`="30+")

#Education
male.label$educ <-recode_factor(male.label$educ,`no education`="1",`primary`="2",`secondary`="3",`higher`="3")
male.label$educ <-recode_factor(male.label$educ,`1`="no education",`2`="primary",`3`="secondary")

#Employment
male.label$employ <-recode_factor(male.label$employ,`no`="1",`in the past year`="1",`currently working`="2")
male.label$employ <-recode_factor(male.label$employ,`1`="none",`2`="working")

#Travel away from home
male.label$travel <-if_else(male.label$travel==0,1,if_else(male.label$travel>0 & male.label$travel<=6,2,3))
male.label$travel <-recode_factor(male.label$travel,`1`="none",`2`="less times",`3`="more times")

#Male circumcision
male.label$mmc <-recode_factor(male.label$mmc,`no`="1",`don't know`="1",`yes`="2")
male.label$mmc <-recode_factor(male.label$mmc,`1`="no",`2`="yes")

#Age at first sex (marriage age in Malawi used to be 16+ at the time of MDHS but now 18+)
male.label$agesexgp <-if_else(male.label$agesex<16,1,if_else(male.label$agesex>=16 & male.label$agesex<20,2,3))
male.label$agesexgp <-recode_factor(male.label$agesexgp,`1`="<16",`2`="16-19",`3`="20+")

#Child desire
male.label$fertpref <- if_else(male.label$fertpref=="wants no more",1,if_else(male.label$fertpref=="wants after 2+ years",1,
                     if_else(male.label$fertpref=="wants within 2 years",2,3)));male.label$fertpref[is.na(male.label$fertpref)]<-3
male.label$fertpref <-recode_factor(male.label$fertpref,`1`="no",`2`="yes",`3`="unsure")

#Paid sex
male.label$paidsex <-recode_factor(male.label$paidsex,`no`="1",`yes`="2")
male.label$paidsex <-recode_factor(male.label$paidsex,`1`="no",`2`="yes")

#Quick descriptive statistics for discussion section
male.label %>% tabyl(condom,sm,show_na=FALSE) #condom use among serial monogamers
male.label %>% tabyl(condom,csp,show_na=FALSE) #condom use among concurrent sexual partnerships
male.label %>% tabyl(windex,sm,show_na=FALSE) #wealth index among serial monogamers
male.label %>% tabyl(windex,csp,show_na=FALSE) #wealth index among concurrent sexual partnership
male.label %>% tabyl(hiv_1part,educ,show_na=FALSE) #HIV knowledge vs. education level

#====================TABULATE COVARIATES BY OUTCOME VARIABLE (TABLE 1.0)====================

#Calculate the Mean+SD of the age of participant and age at first sex
male.label <-subset(male.label, select=c(clustno,houseno,mcsp,sm,csp,age,agegp,educ,employ,travel,mmc,agesexgp,agesex,fertpref,paidsex))
ddply(male.label,.(!is.na(mcsp)),summarize, mean=mean(age),sd=sd(age)) #overall
ddply(male.label,.(mcsp==1),summarize, mean=mean(age),sd=sd(age)) #single partnership
ddply(male.label, .(!is.na(sm)), summarize, mean=mean(age), sd=sd(age)) #serial monogamy
ddply(male.label, .(!is.na(csp)), summarize, mean=mean(age), sd=sd(age)) #concurrent sexual partnership
male.label$age<-NULL #remove age

#Calculate the Mean+SD of the age at sexual debut
ddply(male.label, .(!is.na(mcsp)), summarize, mean=mean(agesex),sd=sd(agesex)) #overall
ddply(male.label, .(mcsp==1), summarize, mean=mean(agesex),sd=sd(agesex)) #single partnership
ddply(male.label, .(!is.na(sm)), summarize, mean=mean(agesex), sd=sd(agesex)) #serial monogamy
ddply(male.label, .(!is.na(csp)), summarize, mean=mean(agesex), sd=sd(agesex)) #concurrent sexual partnership
male.label$agesex<-NULL #remove age

#Iterate while tabulating the proportion of outcome for each predictor variable
for(i in colnames(male.label[c(6:13)])){
  print(tabyl(male.label[[i]],show_na=FALSE) %>% adorn_pct_formatting(digits=1))
};remove(i)

for(i in colnames(male.label[c(6:13)])){
  print(tabyl(male.label[[i]][male.label$mcsp==0],show_na=FALSE) %>% adorn_pct_formatting(digits=1))
};remove(i)

for(i in colnames(male.label[c(6:13)])){
  print(tabyl(male.label[[i]][male.label$sm==1],show_na=FALSE) %>% adorn_pct_formatting(digits=1))
};remove(i)

for(i in colnames(male.label[c(6:13)])){
  print(tabyl(male.label[[i]][male.label$csp==1],show_na=FALSE) %>% adorn_pct_formatting(digits=1))
};remove(i)

#Chi-squared tests for associations between each predictor variable and each outcome variable
for(i in colnames(male.label[c(6:13)])){
  print(chisq.test(male.label[[i]],male.label$sm))
};remove(i)

for(i in colnames(male.label[c(6:13)])){
  print(chisq.test(male.label[[i]],male.label$sm))
};remove(i)

#create a coded dataset for model fitting while keeping labels in original dataset
male <-subset(male.label, select=c(clustno,houseno,sm,csp,agegp,educ,employ,travel,mmc,agesexgp,fertpref,paidsex))

#====================PREPARE DATASETS FOR MODEL FITTING====================

#recode all predictor variables to represent integer class for binomial model fitting
male$agegp <-coerce_index(recode_factor(male$agegp,`15-19`=1L,`20-29`=2L,`30+`=3L))
male$educ <-coerce_index(recode_factor(male$educ,`no education`=1L,`primary`=2L,`secondary`=3L))
male$employ <-coerce_index(recode_factor(male$employ,`none`=1L,`working`=2L))
male$travel <-coerce_index(recode_factor(male$travel,`none`=1L,`less times`=2L,`more times`=3L))
male$mmc <-coerce_index(recode_factor(male$mmc,`no`=1L,`yes`=2L))
male$agesexgp <-coerce_index(recode_factor(male$agesexgp,`<16`=1L,`16-19`=2L,`20+`=3L))
male$fertpref <-coerce_index(recode_factor(male$fertpref,`no`=1L,`yes`=2L,`unsure`=3L))
male$paidsex <-coerce_index(recode_factor(male$paidsex,`no`=1L,`yes`=2L))

#Subset the dataset to get serial monogamy separate from concurrent sexual partnership
male.sm <- subset(male,!is.na(sm)); male.sm <- male.sm[,-4]
male.csp <- subset(male,!is.na(csp)); male.csp <- male.csp[-3]

#Integerize the enumeration area and household ids so they are contiguous
male.sm$houseno <- coerce_index(male.sm$houseno)
male.sm$clustno <- coerce_index(male.sm$clustno)
male.csp$houseno <- coerce_index(male.csp$houseno)
male.csp$clustno <- coerce_index(male.csp$clustno)

#==============FIT MODELS FOR SERIAL MONOGAMY AND SAMPLE FROM POSTERIOR DISTRIBUTION USING HALMITONIAN MCMC==================

#model1: a model without random-effects variables
set.seed(1988)
sm.model1 <- map2stan(
  alist(sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+age*agegp+education*educ+employment*employ+travel_times*travel+circumcision*mmc+sexual_debut*agesexgp+child_desire*fertpref+paid_sex*paidsex,
    c(age,education,employment,travel_times,circumcision,sexual_debut,child_desire,paid_sex) ~ dnorm(0,1),
    a ~ dnorm(0,1)), 
    data=as.data.frame(na.omit(male.sm)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=1988)

set.seed(1988)
sm.model1p <- map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+age[agegp]+education[educ]+employment[employ]+travel_times[travel]+circumcision[mmc]+sexual_debut[agesexgp]+child_desire[fertpref]+paid_sex[paidsex],
    age[agegp] ~ dnorm(0,1),
    education[educ] ~ dnorm(0,1),
    employment[employ] ~ dnorm(0,1),
    travel_times[travel] ~ dnorm(0,1),
    circumcision[mmc] ~ dnorm(0,1),
    sexual_debut[agesexgp] ~ dnorm(0,1),
    child_desire[fertpref] ~ dnorm(0,1),
    paid_sex[paidsex] ~ dnorm(0,1),
    a ~ dnorm(0,1)), 
  data=as.data.frame(na.omit(male.sm)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=1988)

#model2: a model with household as random-effects variable
set.seed(2040)
sm.model2 <- map2stan(
  alist(sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_houseno[houseno]+age*agegp+education*educ+employment*employ+travel_times*travel+circumcision*mmc+sexual_debut*agesexgp+child_desire*fertpref+paid_sex*paidsex,
    c(age,education,employment,travel_times,circumcision,sexual_debut,child_desire,paid_sex) ~ dnorm(0,1),
    a ~ dnorm(0,1),
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    s_houseno ~ dcauchy(0,1)),
  data=as.data.frame(na.omit(male.sm)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=2040)

set.seed(2040)
sm.model2p <- map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_houseno[houseno]+age[agegp]+education[educ]+employment[employ]+travel_times[travel]+circumcision[mmc]+sexual_debut[agesexgp]+child_desire[fertpref]+paid_sex[paidsex],
    age[agegp] ~ dnorm(0,1),
    education[educ] ~ dnorm(0,1),
    employment[employ] ~ dnorm(0,1),
    travel_times[travel] ~ dnorm(0,1),
    circumcision[mmc] ~ dnorm(0,1),
    sexual_debut[agesexgp] ~ dnorm(0,1),
    child_desire[fertpref] ~ dnorm(0,1),
    paid_sex[paidsex] ~ dnorm(0,1),
    a ~ dnorm(0,1),
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    s_houseno ~ dcauchy(0,1)),
  data=as.data.frame(na.omit(male.sm)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=2040)

#model3: a model with enumeration area as random-effects variable
set.seed(1717)
sm.model3 <- map2stan(
  alist(sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_clustno[clustno]+age*agegp+education*educ+employment*employ+travel_times*travel+circumcision*mmc+sexual_debut*agesexgp+child_desire*fertpref+paid_sex*paidsex,
    c(age,education,employment,travel_times,circumcision,sexual_debut,child_desire,paid_sex) ~ dnorm(0,1),
    a ~ dnorm(0,1),
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_clustno ~ dcauchy(0,1)),
  data=as.data.frame(na.omit(male.sm)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=1717)

set.seed(1717)
sm.model3p <- map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_clustno[clustno]+age[agegp]+education[educ]+employment[employ]+travel_times[travel]+circumcision[mmc]+sexual_debut[agesexgp]+child_desire[fertpref]+paid_sex[paidsex],
    age[agegp] ~ dnorm(0,1),
    education[educ] ~ dnorm(0,1),
    employment[employ] ~ dnorm(0,1),
    travel_times[travel] ~ dnorm(0,1),
    circumcision[mmc] ~ dnorm(0,1),
    sexual_debut[agesexgp] ~ dnorm(0,1),
    child_desire[fertpref] ~ dnorm(0,1),
    paid_sex[paidsex] ~ dnorm(0,1),
    a ~ dnorm(0,1),
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_clustno ~ dcauchy(0,1)),
  data=as.data.frame(na.omit(male.sm)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=1717)
save.image()
#model4: a model with household and enumeration area random-effects variables
set.seed(3939)
sm.model4 <- map2stan(
  alist(sm ~ dbinom(1,sm_p),
        logit(sm_p) <- a+a_clustno[clustno]+a_houseno[houseno]+age*agegp+education*educ+employment*employ+travel_times*travel+circumcision*mmc+sexual_debut*agesexgp+child_desire*fertpref+paid_sex*paidsex,
        c(age,education,employment,travel_times,circumcision,sexual_debut,child_desire,paid_sex) ~ dnorm(0,1),
        a ~ dnorm(0,1),
        a_clustno[clustno] ~ dnorm(0,s_clustno),
        s_clustno ~ dcauchy(0,1),
        a_houseno[houseno] ~ dnorm(0,s_houseno),
        s_houseno ~ dcauchy(0,1)),
  data=as.data.frame(na.omit(male.sm)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=3939)

set.seed(3939)
sm.model4p <- map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_clustno[clustno]+a_houseno[houseno]+age[agegp]+education[educ]+employment[employ]+travel_times[travel]+circumcision[mmc]+sexual_debut[agesexgp]+child_desire[fertpref]+paid_sex[paidsex],
    age[agegp] ~ dnorm(0,1),
    education[educ] ~ dnorm(0,1),
    employment[employ] ~ dnorm(0,1),
    travel_times[travel] ~ dnorm(0,1),
    circumcision[mmc] ~ dnorm(0,1),
    sexual_debut[agesexgp] ~ dnorm(0,1),
    child_desire[fertpref] ~ dnorm(0,1),
    paid_sex[paidsex] ~ dnorm(0,1),
    a ~ dnorm(0,1),
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_clustno ~ dcauchy(0,1),
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    s_houseno ~ dcauchy(0,1)),
  data=as.data.frame(na.omit(male.sm)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=3939)
save.image()
#==============FIT MODELS FOR CONCURRENT SEXUAL PARTNERSHIP AND SAMPLE FROM POSTERIOR DISTRIBUTION USING HALMITONIAN MCMC==================

#model1: a model without random-effects variables
set.seed(1988)
csp.model1 <- map2stan(
  alist(csp ~ dbinom(1,csp_p),
        logit(csp_p) <- a+age*agegp+education*educ+employment*employ+travel_times*travel+circumcision*mmc+sexual_debut*agesexgp+child_desire*fertpref+paid_sex*paidsex,
        c(age,education,employment,travel_times,circumcision,sexual_debut,child_desire,paid_sex) ~ dnorm(0,1),
        a ~ dnorm(0,1)), 
  data=as.data.frame(na.omit(male.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=1988)

set.seed(1988)
csp.model1p <- map2stan(
  alist(
    csp ~ dbinom(1,csp_p),
    logit(csp_p) <- a+age[agegp]+education[educ]+employment[employ]+travel_times[travel]+circumcision[mmc]+sexual_debut[agesexgp]+child_desire[fertpref]+paid_sex[paidsex],
    age[agegp] ~ dnorm(0,1),
    education[educ] ~ dnorm(0,1),
    employment[employ] ~ dnorm(0,1),
    travel_times[travel] ~ dnorm(0,1),
    circumcision[mmc] ~ dnorm(0,1),
    sexual_debut[agesexgp] ~ dnorm(0,1),
    child_desire[fertpref] ~ dnorm(0,1),
    paid_sex[paidsex] ~ dnorm(0,1),
    a ~ dnorm(0,1)), 
  data=as.data.frame(na.omit(male.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=1988)
save.image()
#model2: a model with household as random-effects variable
set.seed(2040)
csp.model2 <- map2stan(
  alist(csp ~ dbinom(1,csp_p),
        logit(csp_p) <- a+a_houseno[houseno]+age*agegp+education*educ+employment*employ+travel_times*travel+circumcision*mmc+sexual_debut*agesexgp+child_desire*fertpref+paid_sex*paidsex,
        c(age,education,employment,travel_times,circumcision,sexual_debut,child_desire,paid_sex) ~ dnorm(0,1),
        a ~ dnorm(0,1),
        a_houseno[houseno] ~ dnorm(0,s_houseno),
        s_houseno ~ dcauchy(0,1)),
  data=as.data.frame(na.omit(male.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=2040)

set.seed(2040)
csp.model2p <- map2stan(
  alist(
    csp ~ dbinom(1,csp_p),
    logit(csp_p) <- a+a_houseno[houseno]+age[agegp]+education[educ]+employment[employ]+travel_times[travel]+circumcision[mmc]+sexual_debut[agesexgp]+child_desire[fertpref]+paid_sex[paidsex],
    age[agegp] ~ dnorm(0,1),
    education[educ] ~ dnorm(0,1),
    employment[employ] ~ dnorm(0,1),
    travel_times[travel] ~ dnorm(0,1),
    circumcision[mmc] ~ dnorm(0,1),
    sexual_debut[agesexgp] ~ dnorm(0,1),
    child_desire[fertpref] ~ dnorm(0,1),
    paid_sex[paidsex] ~ dnorm(0,1),
    a ~ dnorm(0,1),
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    s_houseno ~ dcauchy(0,1)),
  data=as.data.frame(na.omit(male.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=2040)
save.image()
#model3: a model with enumeration area as random-effects variable
set.seed(1717)
csp.model3 <- map2stan(
  alist(csp ~ dbinom(1,csp_p),
        logit(csp_p) <- a+a_clustno[clustno]+age*agegp+education*educ+employment*employ+travel_times*travel+circumcision*mmc+sexual_debut*agesexgp+child_desire*fertpref+paid_sex*paidsex,
        c(age,education,employment,travel_times,circumcision,sexual_debut,child_desire,paid_sex) ~ dnorm(0,1),
        a ~ dnorm(0,1),
        a_clustno[clustno] ~ dnorm(0,s_clustno),
        s_clustno ~ dcauchy(0,1)),
  data=as.data.frame(na.omit(male.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=1717)

set.seed(1717)
csp.model3p <- map2stan(
  alist(
    csp ~ dbinom(1,csp_p),
    logit(csp_p) <- a+a_clustno[clustno]+age[agegp]+education[educ]+employment[employ]+travel_times[travel]+circumcision[mmc]+sexual_debut[agesexgp]+child_desire[fertpref]+paid_sex[paidsex],
    age[agegp] ~ dnorm(0,1),
    education[educ] ~ dnorm(0,1),
    employment[employ] ~ dnorm(0,1),
    travel_times[travel] ~ dnorm(0,1),
    circumcision[mmc] ~ dnorm(0,1),
    sexual_debut[agesexgp] ~ dnorm(0,1),
    child_desire[fertpref] ~ dnorm(0,1),
    paid_sex[paidsex] ~ dnorm(0,1),
    a ~ dnorm(0,1),
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_clustno ~ dcauchy(0,1)),
  data=as.data.frame(na.omit(male.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=1717)
save.image()
#model4: a model with household and enumeration area random-effects variables
set.seed(3939)
csp.model4 <- map2stan(
  alist(csp ~ dbinom(1,csp_p),
        logit(csp_p) <- a+a_clustno[clustno]+a_houseno[houseno]+age*agegp+education*educ+employment*employ+travel_times*travel+circumcision*mmc+sexual_debut*agesexgp+child_desire*fertpref+paid_sex*paidsex,
        c(age,education,employment,travel_times,circumcision,sexual_debut,child_desire,paid_sex) ~ dnorm(0,1),
        a ~ dnorm(0,1),
        a_clustno[clustno] ~ dnorm(0,s_clustno),
        s_clustno ~ dcauchy(0,1),
        a_houseno[houseno] ~ dnorm(0,s_houseno),
        s_houseno ~ dcauchy(0,1)),
  data=as.data.frame(na.omit(male.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=3939)

set.seed(3939)
csp.model4p <- map2stan(
  alist(
    csp ~ dbinom(1,csp_p),
    logit(csp_p) <- a+a_clustno[clustno]+a_houseno[houseno]+age[agegp]+education[educ]+employment[employ]+travel_times[travel]+circumcision[mmc]+sexual_debut[agesexgp]+child_desire[fertpref]+paid_sex[paidsex],
    age[agegp] ~ dnorm(0,1),
    education[educ] ~ dnorm(0,1),
    employment[employ] ~ dnorm(0,1),
    travel_times[travel] ~ dnorm(0,1),
    circumcision[mmc] ~ dnorm(0,1),
    sexual_debut[agesexgp] ~ dnorm(0,1),
    child_desire[fertpref] ~ dnorm(0,1),
    paid_sex[paidsex] ~ dnorm(0,1),
    a ~ dnorm(0,1),
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_clustno ~ dcauchy(0,1),
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    s_houseno ~ dcauchy(0,1)),
  data=as.data.frame(na.omit(male.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=3939)
save.image()
#==============POSTERIOR DISTRIBUTION ASSESSMENT THROUGH TRACEPLOTS, PAIRSPLOTS==================

##iterate for all models fitted above from sm.model1 through csp.model4p
posterior.cov = c("age","education","employment","travel_times","circumcision","sexual_debut","child_desire","paid_sex")
dev.off()
plot(sm.model1); pairs(sm.model1); summary(sm.model1); divergent(sm.model1); dashboard(sm.model1)
#*everything looks perfectly fitted*

#==============ANALYSIS OF THE POSTERIOR DISTRIBUTION==================

#Absolute Predicted Risk of serial monogamy and concurrent sexual partnership by covariate levels (Table 2)
sm.extract<-data.frame(extract.samples(sm.model4p))
csp.extract<-data.frame(extract.samples(csp.model4p))

for(i in colnames(sm.extract[,1:21])){
      print(precis(data.frame(logistic(sm.extract[[i]])),prob=0.95),justify="right",digits=3)
};remove(i)

for(i in colnames(csp.extract[,1:21])){
  print(precis(data.frame(logistic(csp.extract[[i]])),prob=0.95),justify="right",digits=3)
};remove(i)

#Differences in predicted risk of serial monogamy between the baseline and other covariate levels (Table 2)
precis(logistic(sm.extract$age.2-sm.extract$age.1),prob=0.95,digits=3) #age group 20-29
precis(logistic(sm.extract$age.3-sm.extract$age.1),prob=0.95,digits=3) #age group 30+
precis(logistic(sm.extract$education.2-sm.extract$education.1),prob=0.95,digits=3) #primary
precis(logistic(sm.extract$education.3-sm.extract$education.1),prob=0.95,digits=3) #secondary
precis(logistic(sm.extract$employment.2-sm.extract$employment.1),prob=0.95,digits=3) #working
precis(logistic(sm.extract$travel_times.2-sm.extract$travel_times.1),prob=0.95,digits=3) #less travel
precis(logistic(sm.extract$travel_times.3-sm.extract$travel_times.1),prob=0.95,digits=3) #more travel
precis(logistic(sm.extract$circumcision.2-sm.extract$circumcision.1),prob=0.95,digits=3) #circumcised
precis(logistic(sm.extract$sexual_debut.2-sm.extract$sexual_debut.1),prob=0.95,digits=3) #age sex debut 16-19
precis(logistic(sm.extract$sexual_debut.3-sm.extract$sexual_debut.1),prob=0.95,digits=3) #age sex debut 16-19
precis(logistic(sm.extract$child_desire.2-sm.extract$child_desire.1),prob=0.95,digits=3) #desire child
precis(logistic(sm.extract$child_desire.3-sm.extract$child_desire.1),prob=0.95,digits=3) #notsure
precis(logistic(sm.extract$paid_sex.2-sm.extract$paid_sex.1),prob=0.95,digits=3) #paid for sex

#Differences in predicted risk of concurrent sexual partnership between the baseline and other covariate levels (Table 2)
precis(logistic(csp.extract$age.2-csp.extract$age.1),prob=0.95,digits=3) #age group 20-29
precis(logistic(csp.extract$age.3-csp.extract$age.1),prob=0.95,digits=3) #age group 30+
precis(logistic(csp.extract$education.2-csp.extract$education.1),prob=0.95,digits=3) #primary
precis(logistic(csp.extract$education.3-csp.extract$education.1),prob=0.95,digits=3) #secondary
precis(logistic(csp.extract$employment.2-csp.extract$employment.1),prob=0.95,digits=3) #working
precis(logistic(csp.extract$travel_times.2-csp.extract$travel_times.1),prob=0.95,digits=3) #less travel
precis(logistic(csp.extract$travel_times.3-csp.extract$travel_times.1),prob=0.95,digits=3) #more travel
precis(logistic(csp.extract$circumcision.2-csp.extract$circumcision.1),prob=0.95,digits=3) #circumcised
precis(logistic(csp.extract$sexual_debut.2-csp.extract$sexual_debut.1),prob=0.95,digits=3) #age sex debut 16-19
precis(logistic(csp.extract$sexual_debut.3-csp.extract$sexual_debut.1),prob=0.95,digits=3) #age sex debut 16-19
precis(logistic(csp.extract$child_desire.2-csp.extract$child_desire.1),prob=0.95,digits=3) #desire child
precis(logistic(csp.extract$child_desire.3-csp.extract$child_desire.1),prob=0.95,digits=3) #notsure
precis(logistic(csp.extract$paid_sex.2-csp.extract$paid_sex.1),prob=0.95,digits=3) #paid for sex

#model comparisons using WAIC for serial monogamy (supplementary figure 1)
dev.off()
cov.par1=c("age[1]","age[2]","age[3]","education[1]","education[2]","education[3]","employment[1]")
cov.par2=c("employment[2]","travel_times[1]","travel_times[2]","travel_times[3]","circumcision[1]","circumcision[2]","sexual_debut[1]")
cov.par3=c("sexual_debut[2]","sexual_debut[3]","child_desire[1]","child_desire[2]","child_desire[3]","paid_sex[1]","paid_sex[2]")
par(mfrow=c(1,4),mai=c(0.9,0,0.5,0.1))
plot(compare(sm.model1p,sm.model2p,sm.model3p,sm.model4p))
title(main="A",line=-1,adj=0.35)
plot(coeftab(sm.model2p,sm.model1p,sm.model3p,sm.model4p),pars=cov.par1)
title(main="B_1",line=-1,adj=0.5)
plot(coeftab(sm.model2p,sm.model1p,sm.model3p,sm.model4p),pars=cov.par2)
title(main="B_2",line=-1,adj=0.5)
plot(coeftab(sm.model2p,sm.model1p,sm.model3p,sm.model4p),pars=cov.par3)
title(main="B_3",line=-1,adj=0.5)

#model comparisons using WAIC for concurrent sexual partnership (supplementary figure 2)
dev.off()
cov.par1=c("age[1]","age[2]","age[3]","education[1]","education[2]","education[3]","employment[1]")
cov.par2=c("employment[2]","travel_times[1]","travel_times[2]","travel_times[3]","circumcision[1]","circumcision[2]","sexual_debut[1]")
cov.par3=c("sexual_debut[2]","sexual_debut[3]","child_desire[1]","child_desire[2]","child_desire[3]","paid_sex[1]","paid_sex[2]")
par(mfrow=c(1,4),mai=c(0.9,0,0.5,0.1))
plot(compare(csp.model1p,csp.model2p,csp.model3p,csp.model4p))
title(main="A",line=-1,adj=0.35)
plot(coeftab(csp.model2p,csp.model1p,csp.model3p,csp.model4p),pars=cov.par1)
title(main="B_1",line=-1,adj=0.5)
plot(coeftab(csp.model2p,csp.model1p,csp.model3p,csp.model4p),pars=cov.par2)
title(main="B_2",line=-1,adj=0.5)
plot(coeftab(csp.model2p,csp.model1p,csp.model3p,csp.model4p),pars=cov.par3)
title(main="B_3",line=-1,adj=0.5)

#posterior samples and traceplots of parameters (supplementary figure 3, figure 4)
dev.off()
par(mfrow=c(3,7),mai = c(0.6, 0.4, 0.2, 0.1))

par.values<-data.frame(extract.samples(csp.model4p))
par.values<-par.values[,1:21]
par.values<-InsertRow(par.values, c("age[15-19]","age[20-29]","age[30+]","education[none]","education[prim]",
"education[second]","employment[none]","employment[working]","travel_times[none]","travel_times[less]","travel_times[more]",
"circumcision[no]","circumcision[yes]","sexual_debut[<16]","sexual_debut[16-19]","sexual_debut[20+]",
"child_desire[no]","child_desire[yes]","child_desire[unsure]","paid_sex[no]","paid_sex[yes]"), RowNum=1)
par.values <- par.values[!(par.values$age.1 !="age[15-19]"),]

for(i in colnames(sm.extract[,1:21])){
  plot(logistic(sm.extract[[i]]), col="lightblue", xlab="",ylab="",main="",type="l",ylim=c(0,1))
  mtext(par.values[[i]],side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7)
  };remove(i)

for(i in colnames(csp.extract[,1:21])){
  plot(logistic(csp.extract[[i]]), col="red3", xlab="",ylab="",main="",type="l",ylim=c(0,1))
  mtext(par.values[[i]],side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7)
};remove(i);remove(par.values)

#Posterior predictive checks and varying effects plots  (supplementary figure 4)
dev.off()
par(mfrow=c(2,2),mai = c(0.6, 0.8, 0.3, 0.1))

sm.link <- link(sm.model4p, data=na.omit(male.sm))
sm.mean <- apply(sm.link,2,mean)
sm.PI <- apply(sm.link,2,PI)
sm.sim <- sim(sm.model4p, n=10)
sm.CI <- apply(sm.sim,2,PI)
plot(sm.mean ~ sm , col=rangi2, ylim=range(sm.PI),xlab="",ylab="predicted serial monogamy", data=na.omit(male.sm))
abline(a=0,b=1,lty=2)
for(i in 1:nrow(na.omit(male.sm))) lines(rep(na.omit(male.sm$sm[i]),2), c(sm.PI[1,i],sm.PI[2,i]), col="red3")
mtext("observed serial monogamy",side=1,line=2,cex=0.9)
mtext("A",side=3,line=0,cex=1.5)

csp.ppp.mu <- link(csp.model4p)
csp.ppp.mu.mean <- apply(csp.ppp.mu,2,mean)
csp.ppp.mu.PI <- apply(csp.ppp.mu,2,PI)
csp.ppp.sim <- sim(csp.model4p, n=10)
csp.ppp.CI <- apply(csp.ppp.sim,2,PI)
plot(csp.ppp.mu.mean ~ csp , col=rangi2, ylim=range(csp.ppp.mu.PI),xlab="",ylab="predicted concurrent sexual partners", data=na.omit(male.csp))
abline(a=0,b=1,lty=2)
for(i in 1:nrow(na.omit(male.csp))) lines(rep(na.omit(male.csp$csp[i]),2), c(csp.ppp.mu.PI[1,i],csp.ppp.mu.PI[2,i]), col="red3")
mtext("observed concurrent sexual partners",side=1,line=2,cex=0.9)
mtext("B",side=3,line=0,cex=1.5)

dens(sm.extract$s_clustno, xlab="", main="",ylim=c(0,6),xlim=c(0,1))
dens(sm.extract$s_houseno, col=rangi2, lwd=2, add=TRUE)
text(0.3,6,"Enumeration area", col=rangi2)
text(0.70, 4.5,"Household")
mtext("variance",side=1,line=2,cex=1)
mtext("C",side=3,line=0,cex=1.5)

dens(csp.extract$s_clustno, xlab="", main="",ylim=c(0,6),xlim=c(0,1))
dens(csp.extract$s_houseno, col=rangi2, lwd=2, add=TRUE)
text(0.3,4.3,"Enumeration area", col=rangi2)
text(0.6, 2.8,"Household")
mtext("variance",side=1,line=2,cex=1)
mtext("D",side=3,line=0,cex=1.5)

#area under the ROC curve (AUC)


#-------------------------------------------------------------------------------------
#counterfactual plot of each predictor on serial monogamy and concurrent sexual partnership (main text figure)
#-------------------------------------------------------------------------------------
cov.par=c("Age","Education","Employment","Travel","Circumcision","Marriage","Sexual_debut","Child_desire","Paid_sex")
cov.seq3=seq.int(from=1, to=3, length.out=30)
cov.seq2=seq.int(from=1, to=2, length.out=30)

for(i in colnames(csp.extract[,1:21])){
  plot(logistic(csp.extract[[i]]), col="red3", xlab="",ylab="",main="",type="l",ylim=c(0,1))
  mtext(par.values[[i]],side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7)
};remove(i);remove(par.values)

agegp.pred.sm <- data_frame(clustno=mean(male.sm$clustno),houseno=mean(male.sm$houseno),agegp=cov.seq3,
educ=mean(na.omit(male.sm$educ)),employ=mean(na.omit(male.sm$employ)),travel=mean(na.omit(male.sm$travel)),
mmc=mean(na.omit(male.sm$mmc)),mstatus=mean(na.omit(male.sm$mstatus)),agesexgp=mean(na.omit(male.sm$agesexgp)),
fertpref=mean(na.omit(male.sm$fertpref)),paidsex=mean(na.omit(male.sm$paidsex)))

agegp.pred.csp <- data_frame(clustno=mean(male.csp$clustno),houseno=mean(male.csp$houseno),agegp=cov.seq3,
educ=mean(na.omit(male.csp$educ)),employ=mean(na.omit(male.csp$employ)),travel=mean(na.omit(male.csp$travel)),
mmc=mean(na.omit(male.csp$mmc)),mstatus=mean(na.omit(male.csp$mstatus)),agesexgp=mean(na.omit(male.csp$agesexgp)),
fertpref=mean(na.omit(male.csp$fertpref)),paidsex=mean(na.omit(male.csp$paidsex)))

sm.agegp.mu <- link(sm.model4, data=agegp.pred.sm)
sm.agegp.mean <- apply(sm.agegp.mu,2,mean)
sm.agegp.PI <- apply(sm.agegp.mu,2,PI,prob=0.95)
csp.agegp.mu <- link(csp.model4, data=agegp.pred.csp)
csp.agegp.mean <- apply(csp.agegp.mu,2,mean)
csp.agegp.PI <- apply(csp.agegp.mu,2,PI,prob=0.95)

sm.agegp.sim <- sim(sm.model4, data=agegp.pred.sm)
sm.agegp.CI <- apply(sm.agegp.sim,2,PI,prob=0.95)
csp.agegp.sim <- sim(csp.model4, data=agegp.pred.csp)
csp.agegp.CI <- apply(csp.agegp.sim,2,PI,prob=0.95)
#-------------------------------------------------------------------------------------
educ.pred.sm <- data_frame(clustno=mean(male.sm$clustno),houseno=mean(male.sm$houseno),educ=cov.seq3,
agegp=mean(na.omit(male.sm$agegp)),employ=mean(na.omit(male.sm$employ)),travel=mean(na.omit(male.sm$travel)),
mmc=mean(na.omit(male.sm$mmc)),mstatus=mean(na.omit(male.sm$mstatus)),agesexgp=mean(na.omit(male.sm$agesexgp)),
fertpref=mean(na.omit(male.sm$fertpref)),paidsex=mean(na.omit(male.sm$paidsex)))

educ.pred.csp <- data_frame(clustno=mean(male.csp$clustno),houseno=mean(male.csp$houseno),educ=cov.seq3,
agegp=mean(na.omit(male.csp$agegp)),employ=mean(na.omit(male.csp$employ)),travel=mean(na.omit(male.csp$travel)),
mmc=mean(na.omit(male.csp$mmc)),mstatus=mean(na.omit(male.csp$mstatus)),agesexgp=mean(na.omit(male.csp$agesexgp)),
fertpref=mean(na.omit(male.csp$fertpref)),paidsex=mean(na.omit(male.csp$paidsex)))

sm.educ.mu <- link(sm.model4, data=educ.pred.sm)
sm.educ.mean <- apply(sm.educ.mu,2,mean)
sm.educ.PI <- apply(sm.educ.mu,2,PI,prob=0.95)
csp.educ.mu <- link(csp.model4, data=educ.pred.csp)
csp.educ.mean <- apply(csp.educ.mu,2,mean)
csp.educ.PI <- apply(csp.educ.mu,2,PI,prob=0.95)

sm.educ.sim <- sim(sm.model4, data=educ.pred.sm)
sm.educ.CI <- apply(sm.educ.sim,2,PI,prob=0.95)
csp.educ.sim <- sim(csp.model4, data=educ.pred.csp)
csp.educ.CI <- apply(csp.educ.sim,2,PI,prob=0.95)
#-------------------------------------------------------------------------------------
employ.pred.sm <- data_frame(clustno=mean(male.sm$clustno),houseno=mean(male.sm$houseno),employ=cov.seq2,
agegp=mean(na.omit(male.sm$agegp)),educ=mean(na.omit(male.sm$educ)),travel=mean(na.omit(male.sm$travel)),
mmc=mean(na.omit(male.sm$mmc)),mstatus=mean(na.omit(male.sm$mstatus)),agesexgp=mean(na.omit(male.sm$agesexgp)),
fertpref=mean(na.omit(male.sm$fertpref)),paidsex=mean(na.omit(male.sm$paidsex)))

employ.pred.csp <- data_frame(clustno=mean(male.csp$clustno),houseno=mean(male.csp$houseno),employ=cov.seq2,
agegp=mean(na.omit(male.csp$agegp)),educ=mean(na.omit(male.csp$educ)),travel=mean(na.omit(male.csp$travel)),
mmc=mean(na.omit(male.csp$mmc)),mstatus=mean(na.omit(male.csp$mstatus)),agesexgp=mean(na.omit(male.csp$agesexgp)),
fertpref=mean(na.omit(male.csp$fertpref)),paidsex=mean(na.omit(male.csp$paidsex)))

sm.employ.mu <- link(sm.model4, data=employ.pred.sm)
sm.employ.mean <- apply(sm.employ.mu,2,mean)
sm.employ.PI <- apply(sm.employ.mu,2,PI,prob=0.95)
csp.employ.mu <- link(csp.model4, data=employ.pred.csp)
csp.employ.mean <- apply(csp.employ.mu,2,mean)
csp.employ.PI <- apply(csp.employ.mu,2,PI,prob=0.95)

sm.employ.sim <- sim(sm.model4, data=employ.pred.sm)
sm.employ.CI <- apply(sm.employ.sim,2,PI,prob=0.95)
csp.employ.sim <- sim(csp.model4, data=employ.pred.csp)
csp.employ.CI <- apply(csp.employ.sim,2,PI,prob=0.95)
#-------------------------------------------------------------------------------------
travel.pred.sm <- data_frame(clustno=mean(male.sm$clustno),houseno=mean(male.sm$houseno),travel=cov.seq3,
agegp=mean(na.omit(male.sm$agegp)),educ=mean(na.omit(male.sm$educ)),employ=mean(na.omit(male.sm$employ)),
mmc=mean(na.omit(male.sm$mmc)),mstatus=mean(na.omit(male.sm$mstatus)),agesexgp=mean(na.omit(male.sm$agesexgp)),
fertpref=mean(na.omit(male.sm$fertpref)),paidsex=mean(na.omit(male.sm$paidsex)))

travel.pred.csp <- data_frame(clustno=mean(male.csp$clustno),houseno=mean(male.csp$houseno),travel=cov.seq3,
agegp=mean(na.omit(male.csp$agegp)),educ=mean(na.omit(male.csp$educ)),employ=mean(na.omit(male.csp$employ)),
mmc=mean(na.omit(male.csp$mmc)),mstatus=mean(na.omit(male.csp$mstatus)),agesexgp=mean(na.omit(male.csp$agesexgp)),
fertpref=mean(na.omit(male.csp$fertpref)),paidsex=mean(na.omit(male.csp$paidsex)))

sm.travel.mu <- link(sm.model4, data=travel.pred.sm)
sm.travel.mean <- apply(sm.travel.mu,2,mean)
sm.travel.PI <- apply(sm.travel.mu,2,PI,prob=0.95)
csp.travel.mu <- link(csp.model4, data=travel.pred.csp)
csp.travel.mean <- apply(csp.travel.mu,2,mean)
csp.travel.PI <- apply(csp.travel.mu,2,PI,prob=0.95)

sm.travel.sim <- sim(sm.model4, data=travel.pred.sm)
sm.travel.CI <- apply(sm.travel.sim,2,PI,prob=0.95)
csp.travel.sim <- sim(csp.model4, data=travel.pred.csp)
csp.travel.CI <- apply(csp.travel.sim,2,PI,prob=0.95)
#-------------------------------------------------------------------------------------
mmc.pred.sm <- data_frame(clustno=mean(male.sm$clustno),houseno=mean(male.sm$houseno),mmc=cov.seq2,
agegp=mean(na.omit(male.sm$agegp)),educ=mean(na.omit(male.sm$educ)),employ=mean(na.omit(male.sm$employ)),
travel=mean(na.omit(male.sm$travel)),mstatus=mean(na.omit(male.sm$mstatus)),agesexgp=mean(na.omit(male.sm$agesexgp)),
fertpref=mean(na.omit(male.sm$fertpref)),paidsex=mean(na.omit(male.sm$paidsex)))

mmc.pred.csp <- data_frame(clustno=mean(male.csp$clustno),houseno=mean(male.csp$houseno),mmc=cov.seq2,
agegp=mean(na.omit(male.csp$agegp)),educ=mean(na.omit(male.csp$educ)),employ=mean(na.omit(male.csp$employ)),
travel=mean(na.omit(male.csp$travel)),mstatus=mean(na.omit(male.csp$mstatus)),agesexgp=mean(na.omit(male.csp$agesexgp)),
fertpref=mean(na.omit(male.csp$fertpref)),paidsex=mean(na.omit(male.csp$paidsex)))

sm.mmc.mu <- link(sm.model4, data=mmc.pred.sm)
sm.mmc.mean <- apply(sm.mmc.mu,2,mean)
sm.mmc.PI <- apply(sm.mmc.mu,2,PI,prob=0.95)
csp.mmc.mu <- link(csp.model4, data=mmc.pred.csp)
csp.mmc.mean <- apply(csp.mmc.mu,2,mean)
csp.mmc.PI <- apply(csp.mmc.mu,2,PI,prob=0.95)

sm.mmc.sim <- sim(sm.model4, data=mmc.pred.sm)
sm.mmc.CI <- apply(sm.mmc.sim,2,PI,prob=0.95)
csp.mmc.sim <- sim(csp.model4, data=mmc.pred.csp)
csp.mmc.CI <- apply(csp.mmc.sim,2,PI,prob=0.95)
#-------------------------------------------------------------------------------------
mstatus.pred.sm <- data_frame(clustno=mean(male.sm$clustno),houseno=mean(male.sm$houseno),mstatus=cov.seq2,
agegp=mean(na.omit(male.sm$agegp)),educ=mean(na.omit(male.sm$educ)),employ=mean(na.omit(male.sm$employ)),
travel=mean(na.omit(male.sm$travel)),mmc=mean(na.omit(male.sm$mmc)),agesexgp=mean(na.omit(male.sm$agesexgp)),
fertpref=mean(na.omit(male.sm$fertpref)),paidsex=mean(na.omit(male.sm$paidsex)))

mstatus.pred.csp <- data_frame(clustno=mean(male.csp$clustno),houseno=mean(male.csp$houseno),mstatus=cov.seq2,
agegp=mean(na.omit(male.csp$agegp)),educ=mean(na.omit(male.csp$educ)),employ=mean(na.omit(male.csp$employ)),
travel=mean(na.omit(male.csp$travel)),mmc=mean(na.omit(male.csp$mmc)),agesexgp=mean(na.omit(male.csp$agesexgp)),
fertpref=mean(na.omit(male.csp$fertpref)),paidsex=mean(na.omit(male.csp$paidsex)))

sm.mstatus.mu <- link(sm.model4, data=mstatus.pred.sm)
sm.mstatus.mean <- apply(sm.mstatus.mu,2,mean)
sm.mstatus.PI <- apply(sm.mstatus.mu,2,PI,prob=0.95)
csp.mstatus.mu <- link(csp.model4, data=mstatus.pred.csp)
csp.mstatus.mean <- apply(csp.mstatus.mu,2,mean)
csp.mstatus.PI <- apply(csp.mstatus.mu,2,PI,prob=0.95)

sm.mstatus.sim <- sim(sm.model4, data=mstatus.pred.sm)
sm.mstatus.CI <- apply(sm.mstatus.sim,2,PI,prob=0.95)
csp.mstatus.sim <- sim(csp.model4, data=mstatus.pred.csp)
csp.mstatus.CI <- apply(csp.mstatus.sim,2,PI,prob=0.95)
#-------------------------------------------------------------------------------------
agesexgp.pred.sm <- data_frame(clustno=mean(male.sm$clustno),houseno=mean(male.sm$houseno),agesexgp=cov.seq3,
agegp=mean(na.omit(male.sm$agegp)),educ=mean(na.omit(male.sm$educ)),employ=mean(na.omit(male.sm$employ)),
travel=mean(na.omit(male.sm$travel)),mmc=mean(na.omit(male.sm$mmc)),mstatus=mean(na.omit(male.sm$mstatus)),
fertpref=mean(na.omit(male.sm$fertpref)),paidsex=mean(na.omit(male.sm$paidsex)))

agesexgp.pred.csp <- data_frame(clustno=mean(male.csp$clustno),houseno=mean(male.csp$houseno),agesexgp=cov.seq3,
agegp=mean(na.omit(male.csp$agegp)),educ=mean(na.omit(male.csp$educ)),employ=mean(na.omit(male.csp$employ)),
travel=mean(na.omit(male.csp$travel)),mmc=mean(na.omit(male.csp$mmc)),mstatus=mean(na.omit(male.csp$mstatus)),
fertpref=mean(na.omit(male.csp$fertpref)),paidsex=mean(na.omit(male.csp$paidsex)))

sm.agesexgp.mu <- link(sm.model4, data=agesexgp.pred.sm)
sm.agesexgp.mean <- apply(sm.agesexgp.mu,2,mean)
sm.agesexgp.PI <- apply(sm.agesexgp.mu,2,PI,prob=0.95)
csp.agesexgp.mu <- link(csp.model4, data=agesexgp.pred.csp)
csp.agesexgp.mean <- apply(csp.agesexgp.mu,2,mean)
csp.agesexgp.PI <- apply(csp.agesexgp.mu,2,PI,prob=0.95)

sm.agesexgp.sim <- sim(sm.model4, data=agesexgp.pred.sm)
sm.agesexgp.CI <- apply(sm.agesexgp.sim,2,PI,prob=0.95)
csp.agesexgp.sim <- sim(csp.model4, data=agesexgp.pred.csp)
csp.agesexgp.CI <- apply(csp.agesexgp.sim,2,PI,prob=0.95)
#-------------------------------------------------------------------------------------
fertpref.pred.sm <- data_frame(clustno=mean(male.sm$clustno),houseno=mean(male.sm$houseno),fertpref=cov.seq2,
agegp=mean(na.omit(male.sm$agegp)),educ=mean(na.omit(male.sm$educ)),employ=mean(na.omit(male.sm$employ)),
travel=mean(na.omit(male.sm$travel)),mmc=mean(na.omit(male.sm$mmc)),mstatus=mean(na.omit(male.sm$mstatus)),
agesexgp=mean(na.omit(male.sm$agesexgp)),paidsex=mean(na.omit(male.sm$paidsex)))

fertpref.pred.csp <- data_frame(clustno=mean(male.csp$clustno),houseno=mean(male.csp$houseno),fertpref=cov.seq2,
agegp=mean(na.omit(male.csp$agegp)),educ=mean(na.omit(male.csp$educ)),employ=mean(na.omit(male.csp$employ)),
travel=mean(na.omit(male.csp$travel)),mmc=mean(na.omit(male.csp$mmc)),mstatus=mean(na.omit(male.csp$mstatus)),
agesexgp=mean(na.omit(male.csp$agesexgp)),paidsex=mean(na.omit(male.csp$paidsex)))

sm.fertpref.mu <- link(sm.model4, data=fertpref.pred.sm)
sm.fertpref.mean <- apply(sm.fertpref.mu,2,mean)
sm.fertpref.PI <- apply(sm.fertpref.mu,2,PI,prob=0.95)
csp.fertpref.mu <- link(csp.model4, data=fertpref.pred.csp)
csp.fertpref.mean <- apply(csp.fertpref.mu,2,mean)
csp.fertpref.PI <- apply(csp.fertpref.mu,2,PI,prob=0.95)

sm.fertpref.sim <- sim(sm.model4, data=fertpref.pred.sm)
sm.fertpref.CI <- apply(sm.fertpref.sim,2,PI,prob=0.95)
csp.fertpref.sim <- sim(csp.model4, data=fertpref.pred.csp)
csp.fertpref.CI <- apply(csp.fertpref.sim,2,PI,prob=0.95)
#-------------------------------------------------------------------------------------
paidsex.pred.sm <- data_frame(clustno=mean(male.sm$clustno),houseno=mean(male.sm$houseno),paidsex=cov.seq2,
agegp=mean(na.omit(male.sm$agegp)),educ=mean(na.omit(male.sm$educ)),employ=mean(na.omit(male.sm$employ)),
travel=mean(na.omit(male.sm$travel)),mmc=mean(na.omit(male.sm$mmc)),mstatus=mean(na.omit(male.sm$mstatus)),
agesexgp=mean(na.omit(male.sm$agesexgp)),fertpref=mean(na.omit(male.sm$fertpref)))

paidsex.pred.csp <- data_frame(clustno=mean(male.csp$clustno),houseno=mean(male.csp$houseno),paidsex=cov.seq2,
agegp=mean(na.omit(male.csp$agegp)),educ=mean(na.omit(male.csp$educ)),employ=mean(na.omit(male.csp$employ)),
travel=mean(na.omit(male.csp$travel)),mmc=mean(na.omit(male.csp$mmc)),mstatus=mean(na.omit(male.csp$mstatus)),
agesexgp=mean(na.omit(male.csp$agesexgp)),fertpref=mean(na.omit(male.csp$fertpref)))

sm_paidsex.mu <- link(sm.model4, data=paidsex.pred.sm)
sm_paidsex.mean <- apply(sm_paidsex.mu,2,mean)
sm_paidsex.PI <- apply(sm_paidsex.mu,2,PI,prob=0.95)
csp_paidsex.mu <- link(csp.model4, data=paidsex.pred.csp)
csp_paidsex.mean <- apply(csp_paidsex.mu,2,mean)
csp_paidsex.PI <- apply(csp_paidsex.mu,2,PI,prob=0.95)

sm_paidsex.sim <- sim(sm.model4, data=paidsex.pred.sm)
sm_paidsex.CI <- apply(sm_paidsex.sim,2,PI,prob=0.95)
csp_paidsex.sim <- sim(csp.model4, data=paidsex.pred.csp)
csp_paidsex.CI <- apply(csp_paidsex.sim,2,PI,prob=0.95)
#-------------------------------------------------------------------------------------

dev.off()
par(mfrow=c(3,3),mai = c(0.5, 0.5, 0.2, 0.1))
plot(sm ~ agegp, data=male.label,pch=16,col=rangi2, type="n",xlab="",ylab="")
mtext("Age",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("A",side=3,line=0); 
shade(sm.agegp.PI,cov.seq3,col="lightblue")
lines(cov.seq3,sm.agegp.mean, col="darkblue",lwd=3)
lines(cov.seq3,csp.agegp.mean, col="red3",lwd=3,lty="dotted")
shade(csp.agegp.PI,cov.seq3,col=rainbow(70, alpha = 0.3))
legend("topleft", legend=c("SM", "CSP"), col=c("darkblue", "red3"), lty=1:1, cex=0.7, lwd=2)

plot(sm ~ educ, data=male.label, type="n",xlab="",ylab="")
mtext("Education",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("B",side=3,line=0);
shade(sm.educ.PI,cov.seq3,col="lightblue")
lines(cov.seq3,sm.educ.mean, col="darkblue",lwd=3)
lines(cov.seq3,csp.educ.mean, col="red3",lwd=3,lty="dotted")
shade(csp.educ.PI,cov.seq3,col=rainbow(70, alpha = 0.4))
legend("topleft", legend=c("SM", "CSP"), col=c("darkblue", "red3"), lty=1:1, cex=0.7, lwd=2)

plot(sm ~ employ, data=male.label, type="n",xlab="",ylab="")
mtext("Employment",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("C",side=3,line=0);
shade(sm.employ.PI,cov.seq2,col="lightblue")
lines(cov.seq2,sm.employ.mean, col="darkblue",lwd=3)
lines(cov.seq2,csp.employ.mean, col="red3",lwd=3,lty="dotted")
shade(csp.employ.PI,cov.seq2,col=rainbow(70, alpha = 0.4))
legend("topleft", legend=c("SM", "CSP"), col=c("darkblue", "red3"), lty=1:1, cex=0.7, lwd=2)

plot(sm ~ travel, data=male.label, type="n",xlab="",ylab="")
mtext("Travel away from home",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("D",side=3,line=0);
shade(sm.travel.PI,cov.seq3,col="lightblue")
lines(cov.seq3,sm.travel.mean, col="darkblue",lwd=3)
lines(cov.seq3,csp.travel.mean, col="red3",lwd=3,lty="dotted")
shade(csp.travel.PI,cov.seq3,col=rainbow(70, alpha = 0.4))
legend("topleft", legend=c("SM", "CSP"), col=c("darkblue", "red3"), lty=1:1, cex=0.7, lwd=2)

plot(sm ~ mmc, data=male.label, type="n",xlab="",ylab="")
mtext("Circumcision",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("E",side=3,line=0);
shade(sm.mmc.PI,cov.seq2,col="lightblue")
lines(cov.seq2,sm.mmc.mean, col="darkblue",lwd=3)
lines(cov.seq2,csp.mmc.mean, col="red3",lwd=3,lty="dotted")
shade(csp.mmc.PI,cov.seq2,col=rainbow(70, alpha = 0.4))
legend("topleft", legend=c("SM", "CSP"), col=c("darkblue", "red3"), lty=1:1, cex=0.7, lwd=2)

plot(sm ~ mstatus, data=male.label, type="n",xlab="",ylab="")
mtext("Marital status",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("F",side=3,line=0);
shade(sm.mstatus.PI,cov.seq2,col="lightblue")
lines(cov.seq2,sm.mstatus.mean, col="darkblue",lwd=3)
lines(cov.seq2,csp.mstatus.mean, col="red3",lwd=3,lty="dotted")
shade(csp.mstatus.PI,cov.seq2,col=rainbow(70, alpha = 0.4))
legend("topleft", legend=c("SM", "CSP"), col=c("darkblue", "red3"), lty=1:1, cex=0.7, lwd=2)

plot(sm ~ agesexgp, data=male.label, type="n",xlab="",ylab="")
mtext("Age at sexual debut",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("G",side=3,line=0)
shade(sm.agesexgp.PI,cov.seq3,col="lightblue")
lines(cov.seq3,sm.agesexgp.mean, col="darkblue",lwd=3)
lines(cov.seq3,csp.agesexgp.mean, col="red3",lwd=3,lty="dotted")
shade(csp.agesexgp.PI,cov.seq3,col=rainbow(70, alpha = 0.4))
legend("topleft", legend=c("SM", "CSP"), col=c("darkblue", "red3"), lty=1:1, cex=0.7, lwd=2)

plot(sm ~ fertpref, data=male.label, type="n",xlab="",ylab="")
mtext("Child desire",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("H",side=3,line=0)
shade(sm.fertpref.PI,cov.seq2,col="lightblue")
lines(cov.seq2,sm.fertpref.mean, col="darkblue",lwd=3)
lines(cov.seq2,csp.fertpref.mean, col="red3",lwd=3,lty="dotted")
shade(csp.fertpref.PI,cov.seq2,col=rainbow(70, alpha = 0.4))
legend("topleft", legend=c("SM", "CSP"), col=c("darkblue", "red3"), lty=1:1, cex=0.7, lwd=2)

plot(sm ~ paidsex, data=male.label, type="n",xlab="",ylab="")
mtext("Paid sex",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("I",side=3,line=0)
shade(sm_paidsex.PI,cov.seq2,col="lightblue")
lines(cov.seq2,sm_paidsex.mean, col="darkblue",lwd=3)
lines(cov.seq2,csp_paidsex.mean, col="red3",lwd=3,lty="dotted")
shade(csp_paidsex.PI,cov.seq2,col=rainbow(70, alpha = 0.4))
legend("topleft", legend=c("SM", "CSP"), col=c("darkblue", "red3"), lty=1:1, cex=0.7, lwd=2)

#-------------------------------------------------------------------------------------






    