#written by Deus Thindwa
#11/04/2019

#====================LOAD REQUIRED PACKAGES AND DATASET====================

DDHP.packages <-c("foreign","tidyverse","janitor","readstata13","rethinking","rstan","coda","plyr")
lapply(DDHP.packages, library, character.only=TRUE)

#load male questionnaire csv
male.dhs <-as_tibble(read.dta13("/Users/dthindwa/Rproject/drivenHIV/data/male.dta"))

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
male.DSF <-subset(male.DSF, select=c(clustno,houseno,sm,csp,agegp,educ,employ,travel,mmc, mstatus,agesexgp,fertpref,paidsex))

#====================PREPARE DATASETS FOR MODEL FIITING====================

#recode all predictors to represent numeric class for binomial model fitting
male.DSF$agegp <-as.numeric(recode_factor(male.DSF$agegp,`15-19`=0,`20-29`=1,`30+`=2))
male.DSF$educ <-as.numeric(recode_factor(male.DSF$educ,`no education`=0,`primary`=1,`secondary`=2))
male.DSF$employ <-as.numeric(recode_factor(male.DSF$employ,`none`=0,`working`=1))
male.DSF$travel <-as.numeric(recode_factor(male.DSF$travel,`none`=0,`less times`=1,`more times`=2))
male.DSF$mmc <-as.numeric(recode_factor(male.DSF$mmc,`no`=0,`yes`=1))
male.DSF$mstatus <-as.numeric(recode_factor(male.DSF$mstatus,`never married`=0,`married`=1,`seperated`=NULL))
male.DSF$agesexgp <-as.numeric(recode_factor(male.DSF$agesexgp,`<16`=0,`16-19`=1,`20+`=2))
male.DSF$fertpref <-as.numeric(recode_factor(male.DSF$fertpref,`doesnt want`=0,`wants`=1))
male.DSF$paidsex <-as.numeric(recode_factor(male.DSF$paidsex,`no`=0,`yes`=1))

#integerize the enumeration area and household ids so they are contiguous.
male.DSF$clustno <- as.integer(as.factor(male.DSF$clustno))
male.DSF$houseno <- as.integer(as.factor(male.DSF$houseno))
sort(unique(male.DSF$clustno))
sort(unique(male.DSF$houseno))

#create separate datasets for each outcome and remove NAs
male.DSF.sm <- subset(male.DSF,!is.na(sm)); male.DSF.sm <- male.DSF.sm[,-4]
male.DSF.csp <- subset(male.DSF,!is.na(csp)); male.DSF.csp <- male.DSF.csp[-3]
male.DSF.sm$clustno[male.DSF.sm$clustno >843] <- 843
male.DSF.csp$clustno[male.DSF.csp$clustno >843] <- 843

#==============FIT MODELS FOR "SM" AND SAMPLE FROM POST.DISTR USING HMC==================

#model1: without random-effects
set.seed(9)
m1.pd_sm <- map2stan(
  alist(sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+b_agegp*agegp+b_educ*educ+b_employ*employ+b_travel*travel+b_mmc*mmc+b_mstatus*mstatus+b_agesexgp*agesexgp+b_fertpref*fertpref+b_paidsex*paidsex,
    c(b_agegp,b_educ,b_employ,b_travel,b_mmc,b_mstatus,b_agesexgp,b_fertpref,b_paidsex) ~ dnorm(0,1),
    a ~ dnorm(0,1)), 
    data=as.data.frame(na.omit(male.DSF.sm)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=9)

#model2: with household random-effects variable
set.seed(8)
m2.pd_sm <- map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_houseno[houseno]+b_agegp*agegp+b_educ*educ+b_employ*employ+b_travel*travel+b_mmc*mmc+b_mstatus*mstatus+b_agesexgp*agesexgp+b_fertpref*fertpref+b_paidsex*paidsex,
    c(b_agegp,b_educ,b_employ,b_travel,b_mmc,b_mstatus,b_agesexgp,b_fertpref,b_paidsex) ~ dnorm(0,1),
    a ~ dnorm(0,1), 
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    s_houseno ~ dcauchy(0,1)), 
    data=as.data.frame(na.omit(male.DSF.sm)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=8)

#model3: with enumeration area random-effects variable
set.seed(7)
m3.pd_sm <- map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_clustno[clustno]+b_agegp*agegp+b_educ*educ+b_employ*employ+b_travel*travel+b_mmc*mmc+b_mstatus*mstatus+b_agesexgp*agesexgp+b_fertpref*fertpref+b_paidsex*paidsex,
    c(b_agegp,b_educ,b_employ,b_travel,b_mmc,b_mstatus,b_agesexgp,b_fertpref,b_paidsex) ~ dnorm(0,1),
    a ~ dnorm(0,1), 
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_clustno ~ dcauchy(0,1)), 
  data=as.data.frame(na.omit(male.DSF.sm)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=7)

#model4: with household and enumeration area random-effects variables
set.seed(6)
m4.pd_sm <- map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_houseno[houseno]+a_clustno[clustno]+b_agegp*agegp+b_educ*educ+b_employ*employ+b_travel*travel+b_mmc*mmc+b_mstatus*mstatus+b_agesexgp*agesexgp+b_fertpref*fertpref+b_paidsex*paidsex,
    c(b_agegp,b_educ,b_employ,b_travel,b_mmc,b_mstatus,b_agesexgp,b_fertpref,b_paidsex) ~ dnorm(0,1),
    a ~ dnorm(0,1),
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_houseno ~ dcauchy(0,1),
    s_clustno ~ dcauchy(0,1)),
  data=as.data.frame(na.omit(male.DSF.sm)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=6)

#==============SERIAL MONOGAMY MODEL AND MCMC DIAGNOSTICS==================

#put all covariates in a vector
cov.par=c("b_agegp","b_educ","b_employ","b_travel","b_mmc","b_mstatus","b_agesexgp","b_fertpref","b_paidsex")

#checking models' convergence and covariate correlation
plot(m1.pd_sm, pars=cov.par)
pairs(m1.pd_sm, pars=cov.par)
summary(m1.pd_sm)

plot(m2.pd_sm, pars=cov.par)
pairs(m2.pd_sm, pars=cov.par)
summary(m2.pd_sm)

plot(m3.pd_sm, pars=cov.par)
pairs(m3.pd_sm, pars=cov.par)
summary(m3.pd_sm)

plot(m4.pd_sm, pars=cov.par)
pairs(m4.pd_sm, pars=cov.par)
summary(m4.pd_sm)

#checking divergence of mcmc
divergent(m1.pd_sm)
divergent(m2.pd_sm)
divergent(m3.pd_sm)
divergent(m4.pd_sm)

#full report of sampler parameters
dev.off()
dashboard(m1.pd_sm)
dashboard(m2.pd_sm)
dashboard(m3.pd_sm)
dashboard(m4.pd_sm)

#==============FIT MODELS FOR "CSP" AND SAMPLE FROM POST.DISTR USING HMC==================

#model1: without random-effects
set.seed(9)
m1.pd_csp <- map2stan(
  alist(csp ~ dbinom(1,csp_p),
        logit(csp_p) <- a+b_agegp*agegp+b_educ*educ+b_employ*employ+b_travel*travel+b_mmc*mmc+b_mstatus*mstatus+b_agesexgp*agesexgp+b_fertpref*fertpref+b_paidsex*paidsex,
        c(b_agegp,b_educ,b_employ,b_travel,b_mmc,b_mstatus,b_agesexgp,b_fertpref,b_paidsex) ~ dnorm(0,1),
        a ~ dnorm(0,1)), 
  data=as.data.frame(na.omit(male.DSF.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=9)

#model2: with household random-effects variable
set.seed(8)
m2.pd_csp <- map2stan(
  alist(
    csp ~ dbinom(1,csp_p),
    logit(csp_p) <- a+a_houseno[houseno]+b_agegp*agegp+b_educ*educ+b_employ*employ+b_travel*travel+b_mmc*mmc+b_mstatus*mstatus+b_agesexgp*agesexgp+b_fertpref*fertpref+b_paidsex*paidsex,
    c(b_agegp,b_educ,b_employ,b_travel,b_mmc,b_mstatus,b_agesexgp,b_fertpref,b_paidsex) ~ dnorm(0,1),
    a ~ dnorm(0,1), 
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    s_houseno ~ dcauchy(0,1)), 
  data=as.data.frame(na.omit(male.DSF.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=8)

#model3: with enumeration area random-effects variable
set.seed(7)
m3.pd_csp <- map2stan(
  alist(
    csp ~ dbinom(1,csp_p),
    logit(csp_p) <- a+a_clustno[clustno]+b_agegp*agegp+b_educ*educ+b_employ*employ+b_travel*travel+b_mmc*mmc+b_mstatus*mstatus+b_agesexgp*agesexgp+b_fertpref*fertpref+b_paidsex*paidsex,
    c(b_agegp,b_educ,b_employ,b_travel,b_mmc,b_mstatus,b_agesexgp,b_fertpref,b_paidsex) ~ dnorm(0,1),
    a ~ dnorm(0,1), 
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_clustno ~ dcauchy(0,1)), 
  data=as.data.frame(na.omit(male.DSF.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=7)

#model4: with household and enumeration area random-effects variables
set.seed(6)
m4.pd_csp <- map2stan(
  alist(
    csp ~ dbinom(1,csp_p),
    logit(csp_p) <- a+a_houseno[houseno]+a_clustno[clustno]+b_agegp*agegp+b_educ*educ+b_employ*employ+b_travel*travel+b_mmc*mmc+b_mstatus*mstatus+b_agesexgp*agesexgp+b_fertpref*fertpref+b_paidsex*paidsex,
    c(b_agegp,b_educ,b_employ,b_travel,b_mmc,b_mstatus,b_agesexgp,b_fertpref,b_paidsex) ~ dnorm(0,1),
    a ~ dnorm(0,1),
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_houseno ~ dcauchy(0,1),
    s_clustno ~ dcauchy(0,1)),
  data=as.data.frame(na.omit(male.DSF.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=6)

#==============CONCURRENT SEXUAL PARTNERSHIP MODEL AND MCMC DIAGNOSTICS==================

#checking models' convergence and covariate correlation
plot(m1.pd_csp, pars=cov.par)
pairs(m1.pd_csp, pars=cov.par)
summary(m1.pd_csp)

plot(m2.pd_csp, pars=cov.par)
pairs(m2.pd_csp, pars=cov.par)
summary(m2.pd_csp)

plot(m3.pd_csp, pars=cov.par)
pairs(m3.pd_csp, pars=cov.par)
summary(m3.pd_csp)

plot(m4.pd_csp, pars=cov.par)
pairs(m4.pd_csp, pars=cov.par)
summary(m4.pd_csp)

#checking divergence of mcmc
divergent(m1.pd_csp)
divergent(m2.pd_csp)
divergent(m3.pd_csp)
divergent(m4.pd_csp)

#full report of sampler parameters
dev.off()
dashboard(m1.pd_csp)
dashboard(m2.pd_csp)
dashboard(m3.pd_csp)
dashboard(m4.pd_csp)

#==============POSTERIOR ANALYSIS==================

#plot model comparisons for each outcome (S1 Figure)
dev.off()
par(mfrow=c(1,2))
plot(compare(m1.pd_sm,m2.pd_sm,m3.pd_sm,m4.pd_sm),main="A")
plot(compare(m1.pd_csp,m2.pd_csp,m3.pd_csp,m4.pd_csp),main="B")

#posterior density and traceplots of parameters (S2 Figure)


#posterior predictive plots of outcomes (S3 Figure)


#posterior distribution estimates for sm and csp from final models


set.seed(6)
m4.pd_cspF <- map2stan(
  alist(
    csp ~ dbinom(1,csp_p),
    logit(csp_p) <- a+a_houseno[houseno]+a_clustno[clustno]+b_agegp[agegp]+b_educ[educ]+b_employ[employ]+b_travel[travel]+b_mmc[mmc]+b_mstatus[mstatus]+b_agesexgp[agesexgp]+b_fertpref[fertpref]+b_paidsex[paidsex],
    b_agegp[agegp] ~ dnorm(0,1),
    b_educ[educ] ~ dnorm(0,1),
    b_employ[employ] ~ dnorm(0,1),
    b_travel[travel] ~ dnorm(0,1),
    b_mmc[mmc] ~ dnorm(0,1),
    b_mstatus[mstatus] ~ dnorm(0,1),
    b_agesexgp[agesexgp] ~ dnorm(0,1),
    b_fertpref[fertpref] ~ dnorm(0,1),
    b_paidsex[paidsex] ~ dnorm(0,1),
    a ~ dnorm(0,1),
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_houseno ~ dcauchy(0,1),
    s_clustno ~ dcauchy(0,1)),
  data=as.data.frame(na.omit(male.DSF.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=6)



dev.off()
par(mfrow=c(1,2))
plot(precis(m3.pd_sm),main="A",xlab="log-odds")
plot(precis(m4.pd_csp),main="B",xlab="log-odds")

#traceplots and posterior density

#counterfactual plot of each predictor of sm

#-------------------------------------------------------------------------------------
cov.seq=seq.int(from=1, to=3, length.out=3)
agegp.pred <- data_frame(clustno=mean(male.DSF.sm$clustno),
                         houseno=mean(male.DSF.sm$houseno),
                         agegp=cov.seq,
                         educ=mean(na.omit(male.DSF.sm$educ)),
                         employ=mean(na.omit(male.DSF.sm$employ)),
                         travel=mean(na.omit(male.DSF.sm$travel)),
                         mmc=mean(na.omit(male.DSF.sm$mmc)),
                         mstatus=mean(na.omit(male.DSF.sm$mstatus)),
                         agesexgp=mean(na.omit(male.DSF.sm$agesexgp)),
                         fertpref=mean(na.omit(male.DSF.sm$fertpref)),
                         paidsex=mean(na.omit(male.DSF.sm$paidsex)))

sm.agegp.mu <- link(m4.pd_sm, data=agegp.pred)
sm.agegp.mean <- apply(sm.agegp.mu,2,mean)
sm.agegp.PI <- apply(sm.agegp.mu,2,PI)
csp.agegp.mu <- link(m4.pd_csp, data=agegp.pred)
csp.agegp.mean <- apply(csp.agegp.mu,2,mean)
csp.agegp.PI <- apply(csp.agegp.mu,2,PI)

sm.agegp.sim <- sim(m4.pd_sm, data=agegp.pred)
sm.agegp.CI <- apply(sm.agegp.sim,2,PI)
csp.agegp.sim <- sim(m4.pd_csp, data=agegp.pred)
csp.agegp.CI <- apply(csp.agegp.sim,2,PI)
#-------------------------------------------------------------------------------------
educ.pred <- data_frame(clustno=mean(male.DSF.sm$clustno),
                       houseno=mean(male.DSF.sm$houseno),
                       agegp=mean(na.omit(male.DSF.sm$agegp)),
                       educ=cov.seq,
                       employ=mean(na.omit(male.DSF.sm$employ)),
                       travel=mean(na.omit(male.DSF.sm$travel)),
                       mmc=mean(na.omit(male.DSF.sm$mmc)),
                       mstatus=mean(na.omit(male.DSF.sm$mstatus)),
                       agesexgp=mean(na.omit(male.DSF.sm$agesexgp)),
                       fertpref=mean(na.omit(male.DSF.sm$fertpref)),
                       paidsex=mean(na.omit(male.DSF.sm$paidsex)))

sm.educ.mu <- link(m4.pd_sm, data=educ.pred)
sm.educ.mean <- apply(sm.educ.mu,2,mean)
sm.educ.PI <- apply(sm.educ.mu,2,PI)
csp.educ.mu <- link(m4.pd_csp, data=educ.pred)
csp.educ.mean <- apply(csp.educ.mu,2,mean)
csp.educ.PI <- apply(csp.educ.mu,2,PI)

sm.educ.sim <- sim(m4.pd_sm, data=educ.pred)
sm.educ.CI <- apply(sm.educ.sim,2,PI)
csp.educ.sim <- sim(m4.pd_csp, data=educ.pred)
csp.educ.CI <- apply(csp.educ.sim,2,PI)
#-------------------------------------------------------------------------------------
employ.pred <- data_frame(clustno=mean(male.DSF.sm$clustno),
                        houseno=mean(male.DSF.sm$houseno),
                        agegp=mean(na.omit(male.DSF.sm$agegp)),
                        educ=mean(na.omit(male.DSF.sm$educ)),
                        employ=cov.seq,
                        travel=mean(na.omit(male.DSF.sm$travel)),
                        mmc=mean(na.omit(male.DSF.sm$mmc)),
                        mstatus=mean(na.omit(male.DSF.sm$mstatus)),
                        agesexgp=mean(na.omit(male.DSF.sm$agesexgp)),
                        fertpref=mean(na.omit(male.DSF.sm$fertpref)),
                        paidsex=mean(na.omit(male.DSF.sm$paidsex)))

sm.employ.mu <- link(m4.pd_sm, data=employ.pred)
sm.employ.mean <- apply(sm.employ.mu,2,mean)
sm.employ.PI <- apply(sm.employ.mu,2,PI)
csp.employ.mu <- link(m4.pd_csp, data=employ.pred)
csp.employ.mean <- apply(csp.employ.mu,2,mean)
csp.employ.PI <- apply(csp.employ.mu,2,PI)

sm.employ.sim <- sim(m4.pd_sm, data=employ.pred)
sm.employ.CI <- apply(sm.employ.sim,2,PI)
csp.employ.sim <- sim(m4.pd_csp, data=employ.pred)
csp.employ.CI <- apply(csp.employ.sim,2,PI)
#-------------------------------------------------------------------------------------
travel.pred <- data_frame(clustno=mean(male.DSF.sm$clustno),
                          houseno=mean(male.DSF.sm$houseno),
                          agegp=mean(na.omit(male.DSF.sm$agegp)),
                          educ=mean(na.omit(male.DSF.sm$educ)),
                          employ=mean(na.omit(male.DSF.sm$employ)),
                          travel=cov.seq,
                          mmc=mean(na.omit(male.DSF.sm$mmc)),
                          mstatus=mean(na.omit(male.DSF.sm$mstatus)),
                          agesexgp=mean(na.omit(male.DSF.sm$agesexgp)),
                          fertpref=mean(na.omit(male.DSF.sm$fertpref)),
                          paidsex=mean(na.omit(male.DSF.sm$paidsex)))

sm.travel.mu <- link(m4.pd_sm, data=travel.pred)
sm.travel.mean <- apply(sm.travel.mu,2,mean)
sm.travel.PI <- apply(sm.travel.mu,2,PI)
csp.travel.mu <- link(m4.pd_csp, data=travel.pred)
csp.travel.mean <- apply(csp.travel.mu,2,mean)
csp.travel.PI <- apply(csp.travel.mu,2,PI)

sm.travel.sim <- sim(m4.pd_sm, data=travel.pred)
sm.travel.CI <- apply(sm.travel.sim,2,PI)
csp.travel.sim <- sim(m4.pd_csp, data=travel.pred)
csp.travel.CI <- apply(csp.travel.sim,2,PI)
#-------------------------------------------------------------------------------------
mmc.pred <- data_frame(clustno=mean(male.DSF.sm$clustno),
                          houseno=mean(male.DSF.sm$houseno),
                          agegp=mean(na.omit(male.DSF.sm$agegp)),
                          educ=mean(na.omit(male.DSF.sm$educ)),
                          employ=mean(na.omit(male.DSF.sm$employ)),
                          travel=mean(na.omit(male.DSF.sm$travel)),
                          mmc=cov.seq,
                          mstatus=mean(na.omit(male.DSF.sm$mstatus)),
                          agesexgp=mean(na.omit(male.DSF.sm$agesexgp)),
                          fertpref=mean(na.omit(male.DSF.sm$fertpref)),
                          paidsex=mean(na.omit(male.DSF.sm$paidsex)))

sm.mmc.mu <- link(m4.pd_sm, data=mmc.pred)
sm.mmc.mean <- apply(sm.mmc.mu,2,mean)
sm.mmc.PI <- apply(sm.mmc.mu,2,PI)
csp.mmc.mu <- link(m4.pd_csp, data=mmc.pred)
csp.mmc.mean <- apply(csp.mmc.mu,2,mean)
csp.mmc.PI <- apply(csp.mmc.mu,2,PI)

sm.mmc.sim <- sim(m4.pd_sm, data=mmc.pred)
sm.mmc.CI <- apply(sm.mmc.sim,2,PI)
csp.mmc.sim <- sim(m4.pd_csp, data=mmc.pred)
csp.mmc.CI <- apply(csp.mmc.sim,2,PI)
#-------------------------------------------------------------------------------------
mstatus.pred <- data_frame(clustno=mean(male.DSF.sm$clustno),
                       houseno=mean(male.DSF.sm$houseno),
                       agegp=mean(na.omit(male.DSF.sm$agegp)),
                       educ=mean(na.omit(male.DSF.sm$educ)),
                       employ=mean(na.omit(male.DSF.sm$employ)),
                       travel=mean(na.omit(male.DSF.sm$travel)),
                       mmc=mean(na.omit(male.DSF.sm$mmc)),
                       mstatus=cov.seq,
                       agesexgp=mean(na.omit(male.DSF.sm$agesexgp)),
                       fertpref=mean(na.omit(male.DSF.sm$fertpref)),
                       paidsex=mean(na.omit(male.DSF.sm$paidsex)))

sm.mstatus.mu <- link(m4.pd_sm, data=mstatus.pred)
sm.mstatus.mean <- apply(sm.mstatus.mu,2,mean)
sm.mstatus.PI <- apply(sm.mstatus.mu,2,PI)
csp.mstatus.mu <- link(m4.pd_csp, data=mstatus.pred)
csp.mstatus.mean <- apply(csp.mstatus.mu,2,mean)
csp.mstatus.PI <- apply(csp.mstatus.mu,2,PI)

sm.mstatus.sim <- sim(m4.pd_sm, data=mstatus.pred)
sm.mstatus.CI <- apply(sm.mstatus.sim,2,PI)
csp.mstatus.sim <- sim(m4.pd_csp, data=mstatus.pred)
csp.mstatus.CI <- apply(csp.mstatus.sim,2,PI)
#-------------------------------------------------------------------------------------
agesexgp.pred <- data_frame(clustno=mean(male.DSF.sm$clustno),
                           houseno=mean(male.DSF.sm$houseno),
                           agegp=mean(na.omit(male.DSF.sm$agegp)),
                           educ=mean(na.omit(male.DSF.sm$educ)),
                           employ=mean(na.omit(male.DSF.sm$employ)),
                           travel=mean(na.omit(male.DSF.sm$travel)),
                           mmc=mean(na.omit(male.DSF.sm$mmc)),
                           mstatus=mean(na.omit(male.DSF.sm$mstatus)),
                           agesexgp=cov.seq,
                           fertpref=mean(na.omit(male.DSF.sm$fertpref)),
                           paidsex=mean(na.omit(male.DSF.sm$paidsex)))

sm.agesexgp.mu <- link(m4.pd_sm, data=agesexgp.pred)
sm.agesexgp.mean <- apply(sm.agesexgp.mu,2,mean)
sm.agesexgp.PI <- apply(sm.agesexgp.mu,2,PI)
csp.agesexgp.mu <- link(m4.pd_csp, data=agesexgp.pred)
csp.agesexgp.mean <- apply(csp.agesexgp.mu,2,mean)
csp.agesexgp.PI <- apply(csp.agesexgp.mu,2,PI)

sm.agesexgp.sim <- sim(m4.pd_sm, data=agesexgp.pred)
sm.agesexgp.CI <- apply(sm.agesexgp.sim,2,PI)
csp.agesexgp.sim <- sim(m4.pd_csp, data=agesexgp.pred)
csp.agesexgp.CI <- apply(csp.agesexgp.sim,2,PI)
#-------------------------------------------------------------------------------------
fertpref.pred <- data_frame(clustno=mean(male.DSF.sm$clustno),
                            houseno=mean(male.DSF.sm$houseno),
                            agegp=mean(na.omit(male.DSF.sm$agegp)),
                            educ=mean(na.omit(male.DSF.sm$educ)),
                            employ=mean(na.omit(male.DSF.sm$employ)),
                            travel=mean(na.omit(male.DSF.sm$travel)),
                            mmc=mean(na.omit(male.DSF.sm$mmc)),
                            mstatus=mean(na.omit(male.DSF.sm$mstatus)),
                            agesexgp=mean(na.omit(male.DSF.sm$agesexgp)),
                            fertpref=cov.seq,
                            paidsex=mean(na.omit(male.DSF.sm$paidsex)))

sm.fertpref.mu <- link(m4.pd_sm, data=fertpref.pred)
sm.fertpref.mean <- apply(sm.fertpref.mu,2,mean)
sm.fertpref.PI <- apply(sm.fertpref.mu,2,PI)
csp.fertpref.mu <- link(m4.pd_csp, data=fertpref.pred)
csp.fertpref.mean <- apply(csp.fertpref.mu,2,mean)
csp.fertpref.PI <- apply(csp.fertpref.mu,2,PI)

sm.fertpref.sim <- sim(m4.pd_sm, data=fertpref.pred)
sm.fertpref.CI <- apply(sm.fertpref.sim,2,PI)
csp.fertpref.sim <- sim(m4.pd_csp, data=fertpref.pred)
csp.fertpref.CI <- apply(csp.fertpref.sim,2,PI)
#-------------------------------------------------------------------------------------
paidsex.pred <- data_frame(clustno=mean(male.DSF.sm$clustno),
                            houseno=mean(male.DSF.sm$houseno),
                            agegp=mean(na.omit(male.DSF.sm$agegp)),
                            educ=mean(na.omit(male.DSF.sm$educ)),
                            employ=mean(na.omit(male.DSF.sm$employ)),
                            travel=mean(na.omit(male.DSF.sm$travel)),
                            mmc=mean(na.omit(male.DSF.sm$mmc)),
                            mstatus=mean(na.omit(male.DSF.sm$mstatus)),
                            agesexgp=mean(na.omit(male.DSF.sm$agesexgp)),
                            fertpref=mean(na.omit(male.DSF.sm$paidsex)),
                            paidsex=cov.seq)

sm.paidsex.mu <- link(m4.pd_sm, data=paidsex.pred)
sm.paidsex.mean <- apply(sm.paidsex.mu,2,mean)
sm.paidsex.PI <- apply(sm.paidsex.mu,2,PI)
csp.paidsex.mu <- link(m4.pd_csp, data=paidsex.pred)
csp.paidsex.mean <- apply(csp.paidsex.mu,2,mean)
csp.paidsex.PI <- apply(csp.paidsex.mu,2,PI)

sm.paidsex.sim <- sim(m4.pd_sm, data=paidsex.pred)
sm.paidsex.CI <- apply(sm.paidsex.sim,2,PI)
csp.paidsex.sim <- sim(m4.pd_csp, data=paidsex.pred)
csp.paidsex.CI <- apply(csp.paidsex.sim,2,PI)
#-------------------------------------------------------------------------------------
dev.off()
par(mfrow=c(3,3),mai = c(0.5, 0.5, 0.2, 0.1))
plot(sm ~ agegp, data=male.DSF.sm, type="n",xlab="",ylab="")
mtext("Age group",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("A",side=3,line=0); 
lines(cov.seq,sm.agegp.mean, col="green4",lwd=4.5)
shade(sm.agegp.PI,cov.seq,col=terrain.colors(70, alpha = 0.4))
lines(cov.seq,csp.agegp.mean, col="red4",lwd=4.5,lty="dotted")
shade(csp.agegp.PI,cov.seq,col=rainbow(70, alpha = 0.4))
legend("topright", legend=c("SM", "CSP"), col=c("green4", "red4"), lty=1:1, cex=0.8, lwd=3)

plot(sm ~ educ, data=male.DSF.sm, type="n",xlab="",ylab="")
mtext("Education level",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("B",side=3,line=0);
lines(cov.seq,sm.educ.mean, col="green4",lwd=4.5)
shade(sm.educ.PI,cov.seq,col=terrain.colors(70, alpha = 0.4))
lines(cov.seq,csp.educ.mean, col="red4",lwd=4.5,lty="dotted")
shade(csp.educ.PI,cov.seq,col=rainbow(70, alpha = 0.4))
#legend("topright", legend=c("SM", "CSP"), col=c("green4", "red4"), lty=1:1, cex=1, lwd=3)

plot(sm ~ employ, data=male.DSF.sm, type="n",xlab="",ylab="")
mtext("Employment",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("C",side=3,line=0);
lines(cov.seq,sm.employ.mean, col="green4",lwd=4.5)
shade(sm.employ.PI,cov.seq,col=terrain.colors(70, alpha = 0.4))
lines(cov.seq,csp.employ.mean, col="red4",lwd=4.5,lty="dotted")
shade(csp.employ.PI,cov.seq,col=rainbow(70, alpha = 0.4))
#legend("topright", legend=c("SM", "CSP"), col=c("green4", "red4"), lty=1:1, cex=1, lwd=3)

plot(sm ~ travel, data=male.DSF.sm, type="n",xlab="",ylab="")
mtext("Times away from home",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("D",side=3,line=0);
lines(cov.seq,sm.travel.mean, col="green4",lwd=4.5)
shade(sm.travel.PI,cov.seq,col=terrain.colors(70, alpha = 0.4))
lines(cov.seq,csp.travel.mean, col="red4",lwd=4.5,lty="dotted")
shade(csp.travel.PI,cov.seq,col=rainbow(70, alpha = 0.4))
#legend("topright", legend=c("SM", "CSP"), col=c("green4", "red4"), lty=1:1, cex=1, lwd=3)

plot(sm ~ mmc, data=male.DSF.sm, type="n",xlab="",ylab="")
mtext("Circumcision",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("E",side=3,line=0);
lines(cov.seq,sm.mmc.mean, col="green4",lwd=4.5)
shade(sm.mmc.PI,cov.seq,col=terrain.colors(70, alpha = 0.4))
lines(cov.seq,csp.mmc.mean, col="red4",lwd=4.5,lty="dotted")
shade(csp.mmc.PI,cov.seq,col=rainbow(70, alpha = 0.4))
#legend("topright", legend=c("SM", "CSP"), col=c("green4", "red4"), lty=1:1, cex=1, lwd=3)

plot(sm ~ mstatus, data=male.DSF.sm, type="n",xlab="",ylab="")
mtext("Marital status",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("F",side=3,line=0);
lines(cov.seq,sm.mstatus.mean, col="green4",lwd=4.5)
shade(sm.mstatus.PI,cov.seq,col=terrain.colors(70, alpha = 0.4))
lines(cov.seq,csp.mstatus.mean, col="red4",lwd=4.5)
shade(csp.mstatus.PI,cov.seq,col=rainbow(70, alpha = 0.4))
#legend("topright", legend=c("SM", "CSP"), col=c("green4", "red4"), lty=1:1, cex=1, lwd=3)

plot(sm ~ agesexgp, data=male.DSF.sm, type="n",xlab="",ylab="")
mtext("Age at first sex",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("G",side=3,line=0)
lines(cov.seq,sm.agesexgp.mean, col="green4",lwd=4.5)
shade(sm.agesexgp.PI,cov.seq,col=terrain.colors(70, alpha = 0.4))
lines(cov.seq,csp.agesexgp.mean, col="red4",lwd=4.5,lty="dotted")
shade(csp.agesexgp.PI,cov.seq,col=rainbow(70, alpha = 0.4))
#legend("topright", legend=c("SM", "CSP"), col=c("green4", "red4"), lty=1:1, cex=1, lwd=3)

plot(sm ~ fertpref, data=male.DSF.sm, type="n",xlab="",ylab="")
mtext("Fertility preference",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("H",side=3,line=0)
lines(cov.seq,sm.fertpref.mean, col="green4",lwd=4.5)
shade(sm.fertpref.PI,cov.seq,col=terrain.colors(70, alpha = 0.4))
lines(cov.seq,csp.fertpref.mean, col="red4",lwd=4.5,lty="dotted")
shade(csp.fertpref.PI,cov.seq,col=rainbow(70, alpha = 0.4))
#legend("topright", legend=c("SM", "CSP"), col=c("green4", "red4"), lty=1:1, cex=1, lwd=3)

plot(sm ~ paidsex, data=male.DSF.sm, type="n",xlab="",ylab="")
mtext("Paid sexual intercourse",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("I",side=3,line=0)
lines(cov.seq,sm.paidsex.mean, col="green4",lwd=4.5)
shade(sm.paidsex.PI,cov.seq,col=terrain.colors(70, alpha = 0.4))
lines(cov.seq,csp.paidsex.mean, col="red4",lwd=4.5,lty="dotted")
shade(csp.paidsex.PI,cov.seq,col=rainbow(70, alpha = 0.4))
#legend("topright", legend=c("SM", "CSP"), col=c("green4", "red4"), lty=1:1, cex=1, lwd=3)

#-------------------------------------------------------------------------------------

#posterior prediction plot
pp.sm.mu <- link(m4.pd_sm)
pp.sm.mean <- apply(pp.sm.mu,2,mean)
pp.sm.PI <- apply(pp.sm.mu,2,PI)
pp.sm.sim <- sim(m4.pd_sm)
pp.sm.CI <- apply(pp.sm.sim,2,PI)
plot(pp.sm.mean ~ male.DSF.sm$sm, col=rangi2, ylim=range(pp.sm.PI),
     xlab="Observed serial monogamy", ylab="Predicted serial monogamy")
     abline(a=0, b=1, lty=2)
     for(i in 1:nrow(d))
     lines(rep(male.DSF.sm$sm[i],2), c(pp.sm.PI[1,i], pp.sm.PI[2,i]), col=rangi2)

