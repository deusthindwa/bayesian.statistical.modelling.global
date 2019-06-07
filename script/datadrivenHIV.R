#written by Deus Thindwa
#11/04/2019

#====================LOAD REQUIRED PACKAGES AND DATASET====================

DDHP.packages <-c("foreign","tidyverse","janitor","readstata13","rethinking","brms","rstan","coda","plyr")
lapply(DDHP.packages, library, character.only=TRUE)

#load male questionnaire csv
male.dhs <-as_tibble(read.dta13("/Users/lsh1703394/Rproject/drivenHIV/data/male.dta"))

#subset the dataset to get appropriate variables
male.DS <-select(male.dhs,mv766b,mv854a,mv001,mv002,mv012,mv025,mv106,mv731,mv130,mv190,mv167,mv213,mv761,mv483,
                 mv501,mv525,mv602,mv605,mv754cp,mv754dp,mv770,mv826a,mv793,hiv03,mv822,mv834a)

#rename variables to appropriately use them
colnames(male.DS) <-c("mcsp","csp","clustno","houseno","age","resid","educ","employ","rel","windex","travel",
                       "partpreg","condom","mmc","mstatus","agesex","fertprof","fertpref",
                       "hiv_condoms","hiv_1part","stdcounsel","hivtest","paidsex","hivres","sexinfl","agepart")

#====================RECODE OUTCOME VARIABLES AND POTENTIAL COVARIATES====================

#MCSP (multiple and concurrent sexual partnership) = SM (serial monogamy) + CSP (concurrent sexual partnership)
male.DSF <-subset(male.DS,mcsp>=1)
male.DSF$mcsp <-if_else(male.DSF$mcsp==1,0,1)

#Define SM (serial monogamy)
male.DSF$sm <-if_else(is.na(male.DSF$csp),0,if_else(male.DSF$csp=="no",1,NULL))
male.DSF %>% tabyl(sm, show_na=FALSE) %>% adorn_pct_formatting(digits=1)

#Define CSP (concurrent sexual partnership)
male.DSF$csp <-if_else(is.na(male.DSF$csp),0,if_else(male.DSF$csp=="yes",1,NULL))
male.DSF %>% tabyl(csp, show_na=FALSE) %>% adorn_pct_formatting(digits=1)

#Respondent male age
male.DSF$agegp <-if_else(male.DSF$age>=15 & male.DSF$age<20,0,if_else(male.DSF$age>=20 & male.DSF$age<30,1,2))
male.DSF$agegp <-recode_factor(male.DSF$agegp,`0`="15-19",`1`="20-29",`2`="30+")

#Education
male.DSF$educ <-recode_factor(male.DSF$educ,`no education`="0",`primary`="1",`secondary`="2",`higher`="2")
male.DSF$educ <-recode_factor(male.DSF$educ,`0`="no education",`1`="primary",`2`="secondary")

#Employment
male.DSF$employ <-recode_factor(male.DSF$employ,`no`="0",`in the past year`="0",`currently working`="1")
male.DSF$employ <-recode_factor(male.DSF$employ,`0`="none",`1`="working")

#Religion
male.DSF$rel <-recode_factor(male.DSF$rel,`no religion`="0",`other`="0",`anglican`="1",`catholic`="1",`ccap`="1",`other christian`="1",`seventh day adventist / baptist`="1",`muslim`="2")
male.DSF$rel <-recode_factor(male.DSF$rel,`0`="none",`1`="christianity",`2`="islam")

#Travel away from home
male.DSF$travel <-if_else(male.DSF$travel==0,0,if_else(male.DSF$travel>0 & male.DSF$travel<=6,1,2))
male.DSF$travel <-recode_factor(male.DSF$travel,`0`="none",`1`="less times",`2`="more times")

#Male circumcision
male.DSF$mmc <-recode_factor(male.DSF$mmc,`no`="0",`don't know`="0",`yes`="1")
male.DSF$mmc <-recode_factor(male.DSF$mmc,`0`="no",`1`="yes")

#Marital status
male.DSF$mstatus <-recode_factor(male.DSF$mstatus,`never in union`="0",`living with partner`="1",`married`="1",`divorced`="2",`no longer living together/separated`="2",`widowed`="2")
male.DSF$mstatus <-recode_factor(male.DSF$mstatus,`0`="never married",`1`="married",`2`="seperated")

#Age at first sex (marriage age in Malawi used to be 16+ at time of MDHS but now 18+)
male.DSF$agesexgp <-if_else(male.DSF$agesex<16,0,if_else(male.DSF$agesex>=16 & male.DSF$agesex<20,1,2))
male.DSF$agesexgp <-recode_factor(male.DSF$agesexgp,`0`="<16",`1`="16-19",`2`="20+")

#Fertility preference
male.DSF$fertpref <-recode_factor(male.DSF$fertpref,`wants no more`="0",`wants within 2 years`="1",`wants, unsure timing`="1",`wants after 2+ years`="1",`undecided`="2",
                                  `declared infecund (respondent or partner(s))`="2",`sterilized (respondent or partner(s))`="2",
                                  `never had sex`="2")
male.DSF$fertpref <-recode_factor(male.DSF$fertpref,`0`="doesnt want",`1`="wants",`2`= NULL)

#Paid sex
male.DSF$paidsex <-recode_factor(male.DSF$paidsex,`no`="0",`yes`="1")
male.DSF$paidsex <-recode_factor(male.DSF$paidsex,`0`="no",`1`="yes")

#====================TABULATE COVARIATES BY OUTCOME VARIABLE (TABLE 1.0)====================

mean(male.DSF$age); sd(male.DSF$age)
ddply(male.DSF, .(mcsp==1), summarize, mean=mean(age),sd=sd(age))
ddply(male.DSF, .(is.na(sm)), summarize, mean=mean(age), sd=sd(age))
ddply(male.DSF, .(is.na(csp)), summarize, mean=mean(age), sd=sd(age))
male.DSF %>% tabyl(agegp,show_na=FALSE) %>% adorn_pct_formatting(digits=1)
male.DSF %>% tabyl(agegp,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(agegp,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$agegp,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$agegp,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(educ,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(educ,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(educ,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$educ,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$educ,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(rel,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(rel,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(rel,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$rel,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$rel,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(employ,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(employ,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(employ,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$employ,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$employ,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(travel,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(travel,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(travel,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$travel,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$travel,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(mmc,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(mmc,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(mmc,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$mmc,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$mmc,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(mstatus,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(mstatus,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(mstatus,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$mstatus,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$mstatus,male.DSF$csp, correct=TRUE)

mean(male.DSF$agesex); sd(male.DSF$agesex)
ddply(male.DSF, .(mcsp==1), summarize, mean=mean(agesex),sd=sd(agesex))
ddply(male.DSF, .(is.na(sm)), summarize, mean=mean(agesex), sd=sd(agesex))
ddply(male.DSF, .(is.na(csp)), summarize, mean=mean(agesex), sd=sd(agesex))
male.DSF %>% tabyl(agesexgp,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(agesexgp,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(agesexgp,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$agesexgp,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$agesexgp,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(fertpref,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(fertpref,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(fertpref,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$fertpref,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$fertpref,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(paidsex,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(paidsex,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(paidsex,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$paidsex,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$paidsex,male.DSF$csp, correct=TRUE)

rm(male.dhs, male.DS)
male.DSF <-subset(male.DSF, select=c(clustno,houseno,sm,csp,agegp,educ,rel,employ,travel,mmc, mstatus,agesexgp,fertpref,paidsex))

#====================PREPARE DATASETS FOR MODEL FIITING====================

#recode all predictors to represent numeric class for binomial model fitting
male.DSF$agegp <-as.numeric(recode_factor(male.DSF$agegp,`15-19`=0,`20-29`=1,`30+`=2))
male.DSF$educ <-as.numeric(recode_factor(male.DSF$educ,`no education`=0,`primary`=1,`secondary`=2))
male.DSF$rel <-as.numeric(recode_factor(male.DSF$rel,`none`=0,`christianity`=1,`islam`=2))
male.DSF$employ <-as.numeric(recode_factor(male.DSF$employ,`none`=0,`working`=1))
male.DSF$travel <-as.numeric(recode_factor(male.DSF$travel,`none`=0,`less times`=1,`more times`=2))
male.DSF$mmc <-as.numeric(recode_factor(male.DSF$mmc,`no`=0,`yes`=1))
male.DSF$mstatus <-as.numeric(recode_factor(male.DSF$mstatus,`never married`=0,`married`=1,`seperated`=NULL))
male.DSF$agesexgp <-as.numeric(recode_factor(male.DSF$agesexgp,`<16`=0,`16-19`=1,`20+`=2))
male.DSF$fertpref <-as.numeric(recode_factor(male.DSF$fertpref,`doesnt want`=0,`wants`=1))
male.DSF$paidsex <-as.numeric(recode_factor(male.DSF$paidsex,`no`=0,`yes`=1))

#create separate datasets for each outcome and remove NAs
male.DSF.sm <- subset(male.DSF,!is.na(sm)); male.DSF.sm <- male.DSF.sm[,-4]
male.DSF.csp <- subset(male.DSF,!is.na(csp)); male.DSF.csp <- male.DSF.csp[-3]

#==============FIT MODEL FOR "SM" AND SAMPLE FROM POST.DISTR USING HMC==================

#integerize the cluster and household ids so they are contiguous.
male.DSF.sm$clustno <- as.integer(as.factor(male.DSF.sm$clustno))
male.DSF.sm$houseno <- as.integer(as.factor(male.DSF.sm$houseno))
sort(unique(male.DSF.sm$clustno))
sort(unique(male.DSF.sm$houseno))

#model1: without random-effects
set.seed(9)
m1.pd_sm <- map2stan(
  alist(sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+b_agegp*agegp+b_educ*educ+b_rel*rel+b_employ*employ+b_travel*travel+b_mmc*mmc+b_mstatus*mstatus+b_agesexgp*agesexgp+b_fertpref*fertpref+b_paidsex*paidsex,
    c(b_agegp,b_educ,b_rel,b_employ,b_travel,b_mmc,b_mstatus,b_agesexgp,b_fertpref,b_paidsex) ~ dnorm(0,1),
    a ~ dnorm(0,1)), 
    data=as.data.frame(na.omit(male.DSF.sm)),chains=2,iter=4000,warmup=1000,cores=2,rng_seed=9)

#model1: convergence correlation checks
plot(m1.pd_sm, pars=c("b_agegp","b_educ","b_rel","b_employ","b_travel","b_mmc","b_mstatus","b_agesexgp","b_fertpref","b_paidsex"))
pairs(m1.pd_sm, pars=c("b_agegp","b_educ","b_rel","b_employ","b_travel","b_mmc","b_mstatus","b_agesexgp","b_fertpref","b_paidsex"))
summary(m1.pd_sm)

#model2: with household random-effects variable
set.seed(8)
m2.pd_sm <- map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_houseno[houseno]+b_agegp*agegp+b_educ*educ+b_rel*rel+b_employ*employ+b_travel*travel+b_mmc*mmc+b_mstatus*mstatus+b_agesexgp*agesexgp+b_fertpref*fertpref+b_paidsex*paidsex,
    c(b_agegp,b_educ,b_rel,b_employ,b_travel,b_mmc,b_mstatus,b_agesexgp,b_fertpref,b_paidsex) ~ dnorm(0,1),
    a ~ dnorm(0,1), 
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    s_houseno ~ dcauchy(0,1)), 
    data=as.data.frame(na.omit(male.DSF.sm)),chains=2,iter=4000,warmup=1000,cores=2,rng_seed=8)

#model2: convergence correlation checks
plot(m2.pd_sm, pars=c("b_agegp","b_educ","b_rel","b_employ","b_travel","b_mmc","b_mstatus","b_agesexgp","b_fertpref","b_paidsex"))
pairs(m2.pd_sm, pars=c("b_agegp","b_educ","b_rel","b_employ","b_travel","b_mmc","b_mstatus","b_agesexgp","b_fertpref","b_paidsex"))
summary(m2.pd_sm)

#model3: with enumeration area random-effects variable
set.seed(7)
m3.pd_sm <- map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_clustno[clustno]+b_agegp*agegp+b_educ*educ+b_rel*rel+b_employ*employ+b_travel*travel+b_mmc*mmc+b_mstatus*mstatus+b_agesexgp*agesexgp+b_fertpref*fertpref+b_paidsex*paidsex,
    c(b_agegp,b_educ,b_rel,b_employ,b_travel,b_mmc,b_mstatus,b_agesexgp,b_fertpref,b_paidsex) ~ dnorm(0,1),
    a ~ dnorm(0,1), 
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_clustno ~ dcauchy(0,1)), 
  data=as.data.frame(na.omit(male.DSF.sm)),chains=2,iter=4000,warmup=1000,cores=2,rng_seed=7)

#model3: convergence correlation checks
plot(m3.pd_sm, pars=c("b_agegp","b_educ","b_rel","b_employ","b_travel","b_mmc","b_mstatus","b_agesexgp","b_fertpref","b_paidsex"))
pairs(m3.pd_sm, pars=c("b_agegp","b_educ","b_rel","b_employ","b_travel","b_mmc","b_mstatus","b_agesexgp","b_fertpref","b_paidsex"))
summary(m3.pd_sm)

#model4: with household and enumeration area random-effects variables
set.seed(6)
m4.pd_sm <- map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_houseno[houseno]+a_clustno[clustno]+b_agegp*agegp+b_resid*resid+b_educ*educ+b_employ*employ+b_rel*rel+b_windex*windex+b_travel*travel+b_partpreg*partpreg
    +b_condom*condom+b_mmc*mmc+b_mstatus*mstatus+b_agesexgp*agesexgp+b_fertpref*fertpref+b_paidsex*paidsex+b_agepartgp*agepartgp,
    c(b_agegp,b_resid,b_educ,b_employ,b_rel,b_windex,b_travel,b_partpreg) ~ dnorm(0,5),
    c(b_condom,b_mmc,b_mstatus,b_agesexgp,b_fertpref,b_paidsex,b_agepartgp) ~ dnorm(0,5),
    a ~ dnorm(0,5),
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_houseno ~ dcauchy(0,1),
    s_clustno ~ dcauchy(0,1)),
    data=as.data.frame(na.omit(male.DSF)),chains=4,iter=4000,warmup=1000,cores=2,rng_seed=6)

#model4: convergence correlation checks
plot(m4.pd_sm)
pairs(m4.pd_sm, pars=c("b_agegp","b_resid","b_educ","b_employ","b_rel","b_windex","b_travel","b_partpreg","b_condom","b_mmc","b_mstatus","b_agesexgp","b_fertpref","b_paidsex","b_agepartgp"))
summary(m4.pd_sm)

#compare model out of sample predictions through WAIC
compare(m1.pd_sm,m2.pd_sm,m3.pd_sm,m4.pd_sm)


#==============fit models for "csp" and sample from posterior distribution using HMC==================

#integerize the cluster and household ids so they are contiguous.
male.DSF.csp$clustno <- as.integer(as.factor(male.DSF.csp$clustno))
male.DSF.csp$houseno <- as.integer(as.factor(male.DSF.csp$houseno))
sort(unique(male.DSF.csp$clustno))
sort(unique(male.DSF.csp$houseno))

#model1: without random-effects
set.seed(9)
m1.pd_csp <- map2stan(
  alist(
    csp ~ dbinom(1,csp_p),
    logit(csp_p) <- a+b_agegp*agegp+b_resid*resid+b_educ*educ+b_employ*employ+b_rel*rel+b_windex*windex+b_travel*travel+b_partpreg*partpreg
    +b_condom*condom+b_mmc*mmc+b_mstatus*mstatus+b_agesexgp*agesexgp+b_fertpref*fertpref+b_paidsex*paidsex+b_agepartgp*agepartgp,
    c(b_agegp,b_resid,b_educ,b_employ,b_rel,b_windex,b_travel,b_partpreg) ~ dnorm(0,5),
    c(b_condom,b_mmc,b_mstatus,b_agesexgp,b_fertpref,b_paidsex,b_agepartgp) ~ dnorm(0,5),
    a ~ dnorm(0,5)), 
  data=as.data.frame(na.omit(male.DSF.csp)),chains=4,iter=4000,warmup=1000,cores=2,rng_seed=9)

#model1: convergence correlation checks
plot(m1.pd_csp)
pairs(m1.pd_csp, pars=c("b_agegp","b_resid","b_educ","b_employ","b_rel","b_windex","b_travel","b_partpreg","b_condom","b_mmc","b_mstatus","b_agesexgp","b_fertpref","b_paidsex","b_agepartgp"))
summary(m1.pd_csp)

#model2: with household random-effects variable
set.seed(8)
m2.pd_csp <- map2stan(
  alist(
    csp ~ dbinom(1,csp_p),
    logit(csp_p) <- a+a_houseno[houseno]+b_agegp*agegp+b_resid*resid+b_educ*educ+b_employ*employ+b_rel*rel+b_windex*windex+b_travel*travel+b_partpreg*partpreg
    +b_condom*condom+b_mmc*mmc+b_mstatus*mstatus+b_agesexgp*agesexgp+b_fertpref*fertpref+b_paidsex*paidsex+b_agepartgp*agepartgp,
    c(b_agegp,b_resid,b_educ,b_employ,b_rel,b_windex,b_travel,b_partpreg) ~ dnorm(0,5),
    c(b_condom,b_mmc,b_mstatus,b_agesexgp,b_fertpref,b_paidsex,b_agepartgp) ~ dnorm(0,5),
    a ~ dnorm(0,5),
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    s_houseno ~ dcauchy(0,1)), 
  data=as.data.frame(na.omit(male.DSF.csp)),chains=4,iter=4000,warmup=1000,cores=2,rng_seed=8)

#model2: convergence correlation checks
plot(m2.pd_csp)
pairs(m2.pd_csp, pars=c("b_agegp","b_resid","b_educ","b_employ","b_rel","b_windex","b_travel","b_partpreg","b_condom","b_mmc","b_mstatus","b_agesexgp","b_fertpref","b_paidsex","b_agepartgp"))
summary(m2.pd_csp)

#model3: with enumeration area random-effects variable
set.seed(7)
m3.pd_csp <- map2stan(
  alist(
    csp ~ dbinom(1,csp_p),
    logit(csp_p) <- a+a_clustno[clustno]+b_agegp*agegp+b_resid*resid+b_educ*educ+b_employ*employ+b_rel*rel+b_windex*windex+b_travel*travel+b_partpreg*partpreg
    +b_condom*condom+b_mmc*mmc+b_mstatus*mstatus+b_agesexgp*agesexgp+b_fertpref*fertpref+b_paidsex*paidsex+b_agepartgp*agepartgp,
    c(b_agegp,b_resid,b_educ,b_employ,b_rel,b_windex,b_travel,b_partpreg) ~ dnorm(0,2.5),
    c(b_condom,b_mmc,b_mstatus,b_agesexgp,b_fertpref,b_paidsex,b_agepartgp) ~ dnorm(0,2.5),
    a ~ dnorm(170,100),
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_clustno ~ dcauchy(0,2.5)),
  data=as.data.frame(na.omit(male.DSF.csp)),chains=4,iter=4000,warmup=1000,cores=2,rng_seed=7)

#model3: convergence correlation checks
plot(m3.pd_csp)
pairs(m3.pd_csp, pars=c("b_agegp","b_resid","b_educ","b_employ","b_rel","b_windex","b_travel","b_partpreg","b_condom","b_mmc","b_mstatus","b_agesexgp","b_fertpref","b_paidsex","b_agepartgp"))
summary(m3.pd_csp)

#model4: with household and enumeration area random-effects variables
set.seed(6)
m4.pd_csp <- map2stan(
  alist(
    csp ~ dbinom(1,csp_p),
    logit(csp_p) <- a+a_houseno[houseno]+a_clustno[clustno]+b_agegp*agegp+b_resid*resid+b_educ*educ+b_employ*employ+b_rel*rel+b_windex*windex+b_travel*travel+b_partpreg*partpreg
    +b_condom*condom+b_mmc*mmc+b_mstatus*mstatus+b_agesexgp*agesexgp+b_fertpref*fertpref+b_paidsex*paidsex+b_agepartgp*agepartgp,
    c(b_agegp,b_resid,b_educ,b_employ,b_rel,b_windex,b_travel,b_partpreg) ~ dnorm(0,5),
    c(b_condom,b_mmc,b_mstatus,b_agesexgp,b_fertpref,b_paidsex,b_agepartgp) ~ dnorm(0,5),
    a ~ dnorm(0,5),
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_houseno ~ dcauchy(0,1),
    s_clustno ~ dcauchy(0,1)),
  data=as.data.frame(na.omit(male.DSF.csp)),chains=4,iter=4000,warmup=1000,cores=2,rng_seed=6)

#model4: convergence correlation checks
plot(m4.pd_csp)
pairs(m4.pd_csp, pars=c("b_agegp","b_resid","b_educ","b_employ","b_rel","b_windex","b_travel","b_partpreg","b_condom","b_mmc","b_mstatus","b_agesexgp","b_fertpref","b_paidsex","b_agepartgp"))
summary(m4.pd_csp)

#compare model out of sample predictions through WAIC
compare(m1.pd_csp,m2.pd_csp,m3.pd_csp,m4.pd_csp)














