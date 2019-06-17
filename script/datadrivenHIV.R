#written by Deus Thindwa
#11/04/2019

#====================LOAD REQUIRED PACKAGES AND DATASET====================

DDHP.packages <-c("foreign","tidyverse","janitor","readstata13","rethinking","rstan","plyr")
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
male.DSF$agegp <-if_else(male.DSF$age>=15 & male.DSF$age<20,1,if_else(male.DSF$age>=20 & male.DSF$age<30,2,3))
male.DSF$agegp <-recode_factor(male.DSF$agegp,`1`="15-19",`2`="20-29",`3`="30+")

#Education
male.DSF$educ <-recode_factor(male.DSF$educ,`no education`="1",`primary`="2",`secondary`="3",`higher`="3")
male.DSF$educ <-recode_factor(male.DSF$educ,`1`="no education",`2`="primary",`3`="secondary")

#Employment
male.DSF$employ <-recode_factor(male.DSF$employ,`no`="1",`in the past year`="1",`currently working`="2")
male.DSF$employ <-recode_factor(male.DSF$employ,`1`="none",`2`="working")

#Travel away from home
male.DSF$travel <-if_else(male.DSF$travel==0,1,if_else(male.DSF$travel>0 & male.DSF$travel<=6,2,3))
male.DSF$travel <-recode_factor(male.DSF$travel,`1`="none",`2`="less times",`3`="more times")

#Male circumcision
male.DSF$mmc <-recode_factor(male.DSF$mmc,`no`="1",`don't know`="1",`yes`="2")
male.DSF$mmc <-recode_factor(male.DSF$mmc,`1`="no",`2`="yes")

#Marital status
male.DSF$mstatus <-recode_factor(male.DSF$mstatus,`never in union`="1",`living with partner`="2",`married`="2",`divorced`="1",`no longer living together/separated`="1",`widowed`="1")
male.DSF$mstatus <-recode_factor(male.DSF$mstatus,`1`="single",`2`="married")

#Age at first sex (marriage age in Malawi used to be 16+ at the time of MDHS but now 18+)
male.DSF$agesexgp <-if_else(male.DSF$agesex<16,1,if_else(male.DSF$agesex>=16 & male.DSF$agesex<20,2,3))
male.DSF$agesexgp <-recode_factor(male.DSF$agesexgp,`1`="<16",`2`="16-19",`3`="20+")

#Condom use

male.DSF$condom <-recode_factor(male.DSF$fertpref,`wants no more`="1",`wants within 2 years`="2",`wants, unsure timing`="2",`wants after 2+ years`="2",`undecided`="3",
                                  `declared infecund (respondent or partner(s))`="3",`sterilized (respondent or partner(s))`="3",
                                  `never had sex`="3")
male.DSF$fertpref <-recode_factor(male.DSF$fertpref,`1`="no",`2`="yes",`3`= NULL)

#Paid sex
male.DSF$paidsex <-recode_factor(male.DSF$paidsex,`no`="1",`yes`="2")
male.DSF$paidsex <-recode_factor(male.DSF$paidsex,`1`="no",`2`="yes")

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

male.DSF %>% tabyl(condom) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(condom,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(condom,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$condom,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$condom,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(paidsex,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(paidsex,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(paidsex,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$paidsex,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$paidsex,male.DSF$csp, correct=TRUE)

rm(male.dhs, male.DS)
male.DSF <-subset(male.DSF, select=c(clustno,houseno,sm,csp,agegp,educ,employ,travel,mmc, mstatus,agesexgp,condom,paidsex))

#====================PREPARE DATASETS FOR MODEL FITTING====================

#recode all predictors to represent numeric class for binomial model fitting
male.DSF.labelled <-male.DSF
male.DSF$agegp <-as.numeric(recode_factor(male.DSF$agegp,`15-19`=1,`20-29`=2,`30+`=3))
male.DSF$educ <-as.numeric(recode_factor(male.DSF$educ,`no education`=1,`primary`=2,`secondary`=3))
male.DSF$employ <-as.numeric(recode_factor(male.DSF$employ,`none`=1,`working`=2))
male.DSF$travel <-as.numeric(recode_factor(male.DSF$travel,`none`=1,`less times`=2,`more times`=3))
male.DSF$mmc <-as.numeric(recode_factor(male.DSF$mmc,`no`=1,`yes`=2))
male.DSF$mstatus <-as.numeric(recode_factor(male.DSF$mstatus,`single`=1,`married`=2))
male.DSF$agesexgp <-as.numeric(recode_factor(male.DSF$agesexgp,`<16`=1,`16-19`=2,`20+`=3))
male.DSF$fertpref <-as.numeric(recode_factor(male.DSF$fertpref,`no`=1,`yes`=2))
male.DSF$paidsex <-as.numeric(recode_factor(male.DSF$paidsex,`no`=1,`yes`=2))

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

#stratum-specific posterior distribution estimates for sm and csp from final models (Table 2)
set.seed(7)
m3.pd_sm.J <- map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_clustno[clustno]+b_agegp[agegp]+b_educ[educ]+b_employ[employ]+b_travel[travel]+b_mmc[mmc]+b_mstatus*mstatus+b_agesexgp[agesexgp]+b_fertpref[fertpref]+b_paidsex[paidsex],
    b_agegp[agegp] ~ dnorm(0,1),
    b_educ[educ] ~ dnorm(0,1),
    b_employ[employ] ~ dnorm(0,1),
    b_travel[travel] ~ dnorm(0,1),
    b_mmc[mmc] ~ dnorm(0,1),
    b_mstatus ~ dnorm(0,1),
    b_agesexgp[agesexgp] ~ dnorm(0,1),
    b_fertpref[fertpref] ~ dnorm(0,1),
    b_paidsex[paidsex] ~ dnorm(0,1),
    a ~ dnorm(0,1), 
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_clustno ~ dcauchy(0,1)), 
  data=as.data.frame(na.omit(male.DSF.sm)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=7)

set.seed(6)
m4.pd_csp.J <- map2stan(
  alist(
    csp ~ dbinom(1,csp_p),
    logit(csp_p) <- a+a_clustno[clustno]+a_houseno[houseno]+b_agegp[agegp]+b_educ[educ]+b_employ[employ]+b_travel[travel]+b_mmc[mmc]+b_mstatus*mstatus+b_agesexgp[agesexgp]+b_fertpref[fertpref]+b_paidsex[paidsex],
    b_agegp[agegp] ~ dnorm(0,1),
    b_educ[educ] ~ dnorm(0,1),
    b_employ[employ] ~ dnorm(0,1),
    b_travel[travel] ~ dnorm(0,1),
    b_mmc[mmc] ~ dnorm(0,1),
    b_mstatus ~ dnorm(0,1),
    b_agesexgp[agesexgp] ~ dnorm(0,1),
    b_fertpref[fertpref] ~ dnorm(0,1),
    b_paidsex[paidsex] ~ dnorm(0,1),
    a ~ dnorm(0,1), 
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_houseno ~ dcauchy(0,1),
    s_clustno ~ dcauchy(0,1)), 
  data=as.data.frame(na.omit(male.DSF.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=7)

summary(m3.pd_sm.J)

J3<-data.frame(extract.samples(m3.pd_sm.J))
mean(logistic(J3$b_agegp.1)); quantile(logistic(J3$b_agegp.1),prob=c(0.025,0.975))
mean(logistic(J3$b_agegp.2)); quantile(logistic(J3$b_agegp.2),prob=c(0.025,0.975))
mean(logistic(J3$b_agegp.3)); quantile(logistic(J3$b_agegp.3),prob=c(0.025,0.975))

mean(logistic(J3$b_educ.1)); quantile(logistic(J3$b_educ.1),prob=c(0.025,0.975))
mean(logistic(J3$b_educ.2)); quantile(logistic(J3$b_educ.2),prob=c(0.025,0.975))
mean(logistic(J3$b_educ.3)); quantile(logistic(J3$b_educ.3),prob=c(0.025,0.975))

mean(logistic(J3$b_employ.1)); quantile(logistic(J3$b_employ.1),prob=c(0.025,0.975))
mean(logistic(J3$b_employ.2)); quantile(logistic(J3$b_employ.2),prob=c(0.025,0.975))

mean(logistic(J3$b_travel.1)); quantile(logistic(J3$b_travel.1),prob=c(0.025,0.975))
mean(logistic(J3$b_travel.2)); quantile(logistic(J3$b_travel.2),prob=c(0.025,0.975))
mean(logistic(J3$b_travel.3)); quantile(logistic(J3$b_travel.3),prob=c(0.025,0.975))

mean(logistic(J3$b_travel.1)); quantile(logistic(J3$b_travel.1),prob=c(0.025,0.975))
mean(logistic(J3$b_travel.2)); quantile(logistic(J3$b_travel.2),prob=c(0.025,0.975))
mean(logistic(J3$b_travel.3)); quantile(logistic(J3$b_travel.3),prob=c(0.025,0.975))

mean(logistic(J3$b_paidsex.1)); quantile(logistic(J3$b_paidsex.1),prob=c(0.025,0.975))
mean(logistic(J3$b_paidsex.2)); quantile(logistic(J3$b_paidsex.2),prob=c(0.025,0.975))

#posterior density and traceplots of parameters (S1 Figure)
m3.pd_smX <- data.frame(p=extract.samples(m3.pd_sm))
m3.pd_smX <- m3.pd_smX[,1:9]
m4.pd_cspX <- data.frame(p=extract.samples(m3.pd_csp))
m4.pd_cspX <- m4.pd_cspX[,1:9]

dev.off()
par(mfrow=c(3,6),mai = c(0.6, 0.4, 0.2, 0.1))
plot(logistic(m3.pd_smX$p.b_agegp), col="lightblue", xlab="",ylab="",main="A",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(m3.pd_smX$p.b_agegp),xlab="",ylab="",main="A",col="darkblue",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(m3.pd_smX$p.b_educ), col="lightblue", xlab="",ylab="",main="B",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(m3.pd_smX$p.b_educ),xlab="",ylab="",main="B",col="darkblue",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(m3.pd_smX$p.b_employ), col="lightblue", xlab="",ylab="",main="C",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(m3.pd_smX$p.b_employ),xlab="",ylab="",main="C",col="darkblue",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(m3.pd_smX$p.b_travel), col="lightblue", xlab="",ylab="",main="D",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(m3.pd_smX$p.b_travel),xlab="",ylab="",main="D",col="darkblue",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(m3.pd_smX$p.b_mmc), col="lightblue", xlab="",ylab="",main="E",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(m3.pd_smX$p.b_mmc),xlab="",ylab="",main="E",col="darkblue",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(m3.pd_smX$p.b_mstatus), col="lightblue", xlab="",ylab="",main="F",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(m3.pd_smX$p.b_mstatus),xlab="",ylab="",main="F",col="darkblue",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(m3.pd_smX$p.b_agesexgp), col="lightblue", xlab="",ylab="",main="G",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(m3.pd_smX$p.b_agesexgp),xlab="",ylab="",main="G",col="darkblue",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(m3.pd_smX$p.b_fertpref), col="lightblue", xlab="",ylab="",main="H",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(m3.pd_smX$p.b_fertpref),xlab="",ylab="",main="H",col="darkblue",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(m3.pd_smX$p.b_paidsex), col="lightblue", xlab="",ylab="",main="I",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(m3.pd_smX$p.b_paidsex),xlab="",ylab="",main="I",col="darkblue",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)
#-------------------------------------------------------------------------------------
dev.off()
par(mfrow=c(3,6),mai = c(0.6, 0.4, 0.2, 0.1))
plot(logistic(m4.pd_cspX$p.b_agegp), col="red1", xlab="",ylab="",main="A",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(m4.pd_cspX$p.b_agegp),xlab="",ylab="",main="A",col="red3",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(m4.pd_cspX$p.b_educ), col="red1", xlab="",ylab="",main="B",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(m4.pd_cspX$p.b_educ),xlab="",ylab="",main="B",col="red3",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(m4.pd_cspX$p.b_employ), col="red1", xlab="",ylab="",main="C",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(m4.pd_cspX$p.b_employ),xlab="",ylab="",main="C",col="red3",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(m4.pd_cspX$p.b_travel), col="red1", xlab="",ylab="",main="D",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(m4.pd_cspX$p.b_travel),xlab="",ylab="",main="D",col="red3",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(m4.pd_cspX$p.b_mmc), col="red1", xlab="",ylab="",main="E",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(m4.pd_cspX$p.b_mmc),xlab="",ylab="",main="E",col="red3",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(m4.pd_cspX$p.b_mstatus), col="red1", xlab="",ylab="",main="F",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(m4.pd_cspX$p.b_mstatus),xlab="",ylab="",main="F",col="red3",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(m4.pd_cspX$p.b_agesexgp), col="red1", xlab="",ylab="",main="G",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(m4.pd_cspX$p.b_agesexgp),xlab="",ylab="",main="G",col="red3",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(m4.pd_cspX$p.b_fertpref), col="red1", xlab="",ylab="",main="H",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(m4.pd_cspX$p.b_fertpref),xlab="",ylab="",main="H",col="red3",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(m4.pd_cspX$p.b_paidsex), col="red1", xlab="",ylab="",main="I",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(m4.pd_cspX$p.b_paidsex),xlab="",ylab="",main="I",col="red3",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

#model comparison and posterior means plots for each outcome (S2 Figure)
dev.off()
cov.par=c("b_agegp","b_educ","b_employ","b_travel","b_mmc","b_mstatus","b_agesexgp","b_fertpref","b_paidsex")
par(mfrow=c(1,2),mai=c(0.9,0,0.5,0.1))
plot(compare(m1.pd_sm,m2.pd_sm,m3.pd_sm,m4.pd_sm))
title(main="A",adj=0.3)
plot(coeftab(m2.pd_sm,m1.pd_sm,m4.pd_sm,m3.pd_sm),pars=cov.par)
title(main="B",adj=0.3)

dev.off()
par(mfrow=c(1,2),mai=c(0.9,0,0.5,0.1))
plot(compare(m2.pd_csp,m1.pd_csp,m3.pd_csp,m4.pd_csp))
title(main="A",adj=0)
plot(coeftab(m2.pd_csp,m1.pd_csp,m3.pd_csp,m4.pd_csp),pars=cov.par)
title(main="B",adj=0.3)

#counterfactual plot (marginal posterior density) of each predictor of sm (Figure 1)
#-------------------------------------------------------------------------------------
cov.par=c("b_agegp","b_educ","b_employ","b_travel","b_mmc","b_mstatus","b_agesexgp","b_fertpref","b_paidsex")
cov.seq3=seq.int(from=1, to=3, length.out=30)
cov.seq2=seq.int(from=1, to=2, length.out=30)

agegp.pred.sm <- data_frame(clustno=mean(male.DSF.sm$clustno),houseno=mean(male.DSF.sm$houseno),agegp=cov.seq3,
educ=mean(na.omit(male.DSF.sm$educ)),employ=mean(na.omit(male.DSF.sm$employ)),travel=mean(na.omit(male.DSF.sm$travel)),
mmc=mean(na.omit(male.DSF.sm$mmc)),mstatus=mean(na.omit(male.DSF.sm$mstatus)),agesexgp=mean(na.omit(male.DSF.sm$agesexgp)),
fertpref=mean(na.omit(male.DSF.sm$fertpref)),paidsex=mean(na.omit(male.DSF.sm$paidsex)))

agegp.pred.csp <- data_frame(clustno=mean(male.DSF.csp$clustno),houseno=mean(male.DSF.csp$houseno),agegp=cov.seq3,
educ=mean(na.omit(male.DSF.csp$educ)),employ=mean(na.omit(male.DSF.csp$employ)),travel=mean(na.omit(male.DSF.csp$travel)),
mmc=mean(na.omit(male.DSF.csp$mmc)),mstatus=mean(na.omit(male.DSF.csp$mstatus)),agesexgp=mean(na.omit(male.DSF.csp$agesexgp)),
fertpref=mean(na.omit(male.DSF.csp$fertpref)),paidsex=mean(na.omit(male.DSF.csp$paidsex)))

sm.agegp.mu <- link(m3.pd_sm, data=agegp.pred.sm)
sm.agegp.mean <- apply(sm.agegp.mu,2,mean)
sm.agegp.PI <- apply(sm.agegp.mu,2,PI,prob=0.95)
csp.agegp.mu <- link(m4.pd_csp, data=agegp.pred.csp)
csp.agegp.mean <- apply(csp.agegp.mu,2,mean)
csp.agegp.PI <- apply(csp.agegp.mu,2,PI,prob=0.95)

sm.agegp.sim <- sim(m3.pd_sm, data=agegp.pred.sm)
sm.agegp.CI <- apply(sm.agegp.sim,2,PI,prob=0.95)
csp.agegp.sim <- sim(m4.pd_csp, data=agegp.pred.csp)
csp.agegp.CI <- apply(csp.agegp.sim,2,PI,prob=0.95)
#-------------------------------------------------------------------------------------
educ.pred.sm <- data_frame(clustno=mean(male.DSF.sm$clustno),houseno=mean(male.DSF.sm$houseno),educ=cov.seq3,
                            agegp=mean(na.omit(male.DSF.sm$agegp)),employ=mean(na.omit(male.DSF.sm$employ)),travel=mean(na.omit(male.DSF.sm$travel)),
                            mmc=mean(na.omit(male.DSF.sm$mmc)),mstatus=mean(na.omit(male.DSF.sm$mstatus)),agesexgp=mean(na.omit(male.DSF.sm$agesexgp)),
                            fertpref=mean(na.omit(male.DSF.sm$fertpref)),paidsex=mean(na.omit(male.DSF.sm$paidsex)))

educ.pred.csp <- data_frame(clustno=mean(male.DSF.csp$clustno),houseno=mean(male.DSF.csp$houseno),educ=cov.seq3,
                             agegp=mean(na.omit(male.DSF.csp$agegp)),employ=mean(na.omit(male.DSF.csp$employ)),travel=mean(na.omit(male.DSF.csp$travel)),
                             mmc=mean(na.omit(male.DSF.csp$mmc)),mstatus=mean(na.omit(male.DSF.csp$mstatus)),agesexgp=mean(na.omit(male.DSF.csp$agesexgp)),
                             fertpref=mean(na.omit(male.DSF.csp$fertpref)),paidsex=mean(na.omit(male.DSF.csp$paidsex)))

sm.educ.mu <- link(m3.pd_sm, data=educ.pred.sm)
sm.educ.mean <- apply(sm.educ.mu,2,mean)
sm.educ.PI <- apply(sm.educ.mu,2,PI,prob=0.95)
csp.educ.mu <- link(m4.pd_csp, data=educ.pred.csp)
csp.educ.mean <- apply(csp.educ.mu,2,mean)
csp.educ.PI <- apply(csp.educ.mu,2,PI,prob=0.95)

sm.educ.sim <- sim(m3.pd_sm, data=educ.pred.sm)
sm.educ.CI <- apply(sm.educ.sim,2,PI,prob=0.95)
csp.educ.sim <- sim(m4.pd_csp, data=educ.pred.csp)
csp.educ.CI <- apply(csp.educ.sim,2,PI,prob=0.95)
#-------------------------------------------------------------------------------------
employ.pred.sm <- data_frame(clustno=mean(male.DSF.sm$clustno),houseno=mean(male.DSF.sm$houseno),employ=cov.seq2,
                           agegp=mean(na.omit(male.DSF.sm$agegp)),educ=mean(na.omit(male.DSF.sm$educ)),travel=mean(na.omit(male.DSF.sm$travel)),
                           mmc=mean(na.omit(male.DSF.sm$mmc)),mstatus=mean(na.omit(male.DSF.sm$mstatus)),agesexgp=mean(na.omit(male.DSF.sm$agesexgp)),
                           fertpref=mean(na.omit(male.DSF.sm$fertpref)),paidsex=mean(na.omit(male.DSF.sm$paidsex)))

employ.pred.csp <- data_frame(clustno=mean(male.DSF.csp$clustno),houseno=mean(male.DSF.csp$houseno),employ=cov.seq2,
                            agegp=mean(na.omit(male.DSF.csp$agegp)),educ=mean(na.omit(male.DSF.csp$educ)),travel=mean(na.omit(male.DSF.csp$travel)),
                            mmc=mean(na.omit(male.DSF.csp$mmc)),mstatus=mean(na.omit(male.DSF.csp$mstatus)),agesexgp=mean(na.omit(male.DSF.csp$agesexgp)),
                            fertpref=mean(na.omit(male.DSF.csp$fertpref)),paidsex=mean(na.omit(male.DSF.csp$paidsex)))

sm.employ.mu <- link(m3.pd_sm, data=employ.pred.sm)
sm.employ.mean <- apply(sm.employ.mu,2,mean)
sm.employ.PI <- apply(sm.employ.mu,2,PI,prob=0.95)
csp.employ.mu <- link(m4.pd_csp, data=employ.pred.csp)
csp.employ.mean <- apply(csp.employ.mu,2,mean)
csp.employ.PI <- apply(csp.employ.mu,2,PI,prob=0.95)

sm.employ.sim <- sim(m3.pd_sm, data=employ.pred.sm)
sm.employ.CI <- apply(sm.employ.sim,2,PI,prob=0.95)
csp.employ.sim <- sim(m4.pd_csp, data=employ.pred.csp)
csp.employ.CI <- apply(csp.employ.sim,2,PI,prob=0.95)
#-------------------------------------------------------------------------------------
travel.pred.sm <- data_frame(clustno=mean(male.DSF.sm$clustno),houseno=mean(male.DSF.sm$houseno),travel=cov.seq3,
                             agegp=mean(na.omit(male.DSF.sm$agegp)),educ=mean(na.omit(male.DSF.sm$educ)),employ=mean(na.omit(male.DSF.sm$employ)),
                             mmc=mean(na.omit(male.DSF.sm$mmc)),mstatus=mean(na.omit(male.DSF.sm$mstatus)),agesexgp=mean(na.omit(male.DSF.sm$agesexgp)),
                             fertpref=mean(na.omit(male.DSF.sm$fertpref)),paidsex=mean(na.omit(male.DSF.sm$paidsex)))

travel.pred.csp <- data_frame(clustno=mean(male.DSF.csp$clustno),houseno=mean(male.DSF.csp$houseno),travel=cov.seq3,
                              agegp=mean(na.omit(male.DSF.csp$agegp)),educ=mean(na.omit(male.DSF.csp$educ)),employ=mean(na.omit(male.DSF.csp$employ)),
                              mmc=mean(na.omit(male.DSF.csp$mmc)),mstatus=mean(na.omit(male.DSF.csp$mstatus)),agesexgp=mean(na.omit(male.DSF.csp$agesexgp)),
                              fertpref=mean(na.omit(male.DSF.csp$fertpref)),paidsex=mean(na.omit(male.DSF.csp$paidsex)))

sm.travel.mu <- link(m3.pd_sm, data=travel.pred.sm)
sm.travel.mean <- apply(sm.travel.mu,2,mean)
sm.travel.PI <- apply(sm.travel.mu,2,PI,prob=0.95)
csp.travel.mu <- link(m4.pd_csp, data=travel.pred.csp)
csp.travel.mean <- apply(csp.travel.mu,2,mean)
csp.travel.PI <- apply(csp.travel.mu,2,PI,prob=0.95)

sm.travel.sim <- sim(m3.pd_sm, data=travel.pred.sm)
sm.travel.CI <- apply(sm.travel.sim,2,PI,prob=0.95)
csp.travel.sim <- sim(m4.pd_csp, data=travel.pred.csp)
csp.travel.CI <- apply(csp.travel.sim,2,PI,prob=0.95)
#-------------------------------------------------------------------------------------
mmc.pred.sm <- data_frame(clustno=mean(male.DSF.sm$clustno),houseno=mean(male.DSF.sm$houseno),mmc=cov.seq2,
                             agegp=mean(na.omit(male.DSF.sm$agegp)),educ=mean(na.omit(male.DSF.sm$educ)),employ=mean(na.omit(male.DSF.sm$employ)),
                             travel=mean(na.omit(male.DSF.sm$travel)),mstatus=mean(na.omit(male.DSF.sm$mstatus)),agesexgp=mean(na.omit(male.DSF.sm$agesexgp)),
                             fertpref=mean(na.omit(male.DSF.sm$fertpref)),paidsex=mean(na.omit(male.DSF.sm$paidsex)))

mmc.pred.csp <- data_frame(clustno=mean(male.DSF.csp$clustno),houseno=mean(male.DSF.csp$houseno),mmc=cov.seq2,
                              agegp=mean(na.omit(male.DSF.csp$agegp)),educ=mean(na.omit(male.DSF.csp$educ)),employ=mean(na.omit(male.DSF.csp$employ)),
                              travel=mean(na.omit(male.DSF.csp$travel)),mstatus=mean(na.omit(male.DSF.csp$mstatus)),agesexgp=mean(na.omit(male.DSF.csp$agesexgp)),
                              fertpref=mean(na.omit(male.DSF.csp$fertpref)),paidsex=mean(na.omit(male.DSF.csp$paidsex)))

sm.mmc.mu <- link(m3.pd_sm, data=mmc.pred.sm)
sm.mmc.mean <- apply(sm.mmc.mu,2,mean)
sm.mmc.PI <- apply(sm.mmc.mu,2,PI,prob=0.95)
csp.mmc.mu <- link(m4.pd_csp, data=mmc.pred.csp)
csp.mmc.mean <- apply(csp.mmc.mu,2,mean)
csp.mmc.PI <- apply(csp.mmc.mu,2,PI,prob=0.95)

sm.mmc.sim <- sim(m3.pd_sm, data=mmc.pred.sm)
sm.mmc.CI <- apply(sm.mmc.sim,2,PI,prob=0.95)
csp.mmc.sim <- sim(m4.pd_csp, data=mmc.pred.csp)
csp.mmc.CI <- apply(csp.mmc.sim,2,PI,prob=0.95)
#-------------------------------------------------------------------------------------
mstatus.pred.sm <- data_frame(clustno=mean(male.DSF.sm$clustno),houseno=mean(male.DSF.sm$houseno),mstatus=cov.seq2,
                          agegp=mean(na.omit(male.DSF.sm$agegp)),educ=mean(na.omit(male.DSF.sm$educ)),employ=mean(na.omit(male.DSF.sm$employ)),
                          travel=mean(na.omit(male.DSF.sm$travel)),mmc=mean(na.omit(male.DSF.sm$mmc)),agesexgp=mean(na.omit(male.DSF.sm$agesexgp)),
                          fertpref=mean(na.omit(male.DSF.sm$fertpref)),paidsex=mean(na.omit(male.DSF.sm$paidsex)))

mstatus.pred.csp <- data_frame(clustno=mean(male.DSF.csp$clustno),houseno=mean(male.DSF.csp$houseno),mstatus=cov.seq2,
                           agegp=mean(na.omit(male.DSF.csp$agegp)),educ=mean(na.omit(male.DSF.csp$educ)),employ=mean(na.omit(male.DSF.csp$employ)),
                           travel=mean(na.omit(male.DSF.csp$travel)),mmc=mean(na.omit(male.DSF.csp$mmc)),agesexgp=mean(na.omit(male.DSF.csp$agesexgp)),
                           fertpref=mean(na.omit(male.DSF.csp$fertpref)),paidsex=mean(na.omit(male.DSF.csp$paidsex)))

sm.mstatus.mu <- link(m3.pd_sm, data=mstatus.pred.sm)
sm.mstatus.mean <- apply(sm.mstatus.mu,2,mean)
sm.mstatus.PI <- apply(sm.mstatus.mu,2,PI,prob=0.95)
csp.mstatus.mu <- link(m4.pd_csp, data=mstatus.pred.csp)
csp.mstatus.mean <- apply(csp.mstatus.mu,2,mean)
csp.mstatus.PI <- apply(csp.mstatus.mu,2,PI,prob=0.95)

sm.mstatus.sim <- sim(m3.pd_sm, data=mstatus.pred.sm)
sm.mstatus.CI <- apply(sm.mstatus.sim,2,PI,prob=0.95)
csp.mstatus.sim <- sim(m4.pd_csp, data=mstatus.pred.csp)
csp.mstatus.CI <- apply(csp.mstatus.sim,2,PI,prob=0.95)
#-------------------------------------------------------------------------------------
agesexgp.pred.sm <- data_frame(clustno=mean(male.DSF.sm$clustno),houseno=mean(male.DSF.sm$houseno),agesexgp=cov.seq3,
                              agegp=mean(na.omit(male.DSF.sm$agegp)),educ=mean(na.omit(male.DSF.sm$educ)),employ=mean(na.omit(male.DSF.sm$employ)),
                              travel=mean(na.omit(male.DSF.sm$travel)),mmc=mean(na.omit(male.DSF.sm$mmc)),mstatus=mean(na.omit(male.DSF.sm$mstatus)),
                              fertpref=mean(na.omit(male.DSF.sm$fertpref)),paidsex=mean(na.omit(male.DSF.sm$paidsex)))

agesexgp.pred.csp <- data_frame(clustno=mean(male.DSF.csp$clustno),houseno=mean(male.DSF.csp$houseno),agesexgp=cov.seq3,
                               agegp=mean(na.omit(male.DSF.csp$agegp)),educ=mean(na.omit(male.DSF.csp$educ)),employ=mean(na.omit(male.DSF.csp$employ)),
                               travel=mean(na.omit(male.DSF.csp$travel)),mmc=mean(na.omit(male.DSF.csp$mmc)),mstatus=mean(na.omit(male.DSF.csp$mstatus)),
                               fertpref=mean(na.omit(male.DSF.csp$fertpref)),paidsex=mean(na.omit(male.DSF.csp$paidsex)))

sm.agesexgp.mu <- link(m3.pd_sm, data=agesexgp.pred.sm)
sm.agesexgp.mean <- apply(sm.agesexgp.mu,2,mean)
sm.agesexgp.PI <- apply(sm.agesexgp.mu,2,PI,prob=0.95)
csp.agesexgp.mu <- link(m4.pd_csp, data=agesexgp.pred.csp)
csp.agesexgp.mean <- apply(csp.agesexgp.mu,2,mean)
csp.agesexgp.PI <- apply(csp.agesexgp.mu,2,PI,prob=0.95)

sm.agesexgp.sim <- sim(m3.pd_sm, data=agesexgp.pred.sm)
sm.agesexgp.CI <- apply(sm.agesexgp.sim,2,PI,prob=0.95)
csp.agesexgp.sim <- sim(m4.pd_csp, data=agesexgp.pred.csp)
csp.agesexgp.CI <- apply(csp.agesexgp.sim,2,PI,prob=0.95)
#-------------------------------------------------------------------------------------
fertpref.pred.sm <- data_frame(clustno=mean(male.DSF.sm$clustno),houseno=mean(male.DSF.sm$houseno),fertpref=cov.seq2,
                               agegp=mean(na.omit(male.DSF.sm$agegp)),educ=mean(na.omit(male.DSF.sm$educ)),employ=mean(na.omit(male.DSF.sm$employ)),
                               travel=mean(na.omit(male.DSF.sm$travel)),mmc=mean(na.omit(male.DSF.sm$mmc)),mstatus=mean(na.omit(male.DSF.sm$mstatus)),
                               agesexgp=mean(na.omit(male.DSF.sm$agesexgp)),paidsex=mean(na.omit(male.DSF.sm$paidsex)))

fertpref.pred.csp <- data_frame(clustno=mean(male.DSF.csp$clustno),houseno=mean(male.DSF.csp$houseno),fertpref=cov.seq2,
                                agegp=mean(na.omit(male.DSF.csp$agegp)),educ=mean(na.omit(male.DSF.csp$educ)),employ=mean(na.omit(male.DSF.csp$employ)),
                                travel=mean(na.omit(male.DSF.csp$travel)),mmc=mean(na.omit(male.DSF.csp$mmc)),mstatus=mean(na.omit(male.DSF.csp$mstatus)),
                                agesexgp=mean(na.omit(male.DSF.csp$agesexgp)),paidsex=mean(na.omit(male.DSF.csp$paidsex)))

sm.fertpref.mu <- link(m3.pd_sm, data=fertpref.pred.sm)
sm.fertpref.mean <- apply(sm.fertpref.mu,2,mean)
sm.fertpref.PI <- apply(sm.fertpref.mu,2,PI,prob=0.95)
csp.fertpref.mu <- link(m4.pd_csp, data=fertpref.pred.csp)
csp.fertpref.mean <- apply(csp.fertpref.mu,2,mean)
csp.fertpref.PI <- apply(csp.fertpref.mu,2,PI,prob=0.95)

sm.fertpref.sim <- sim(m3.pd_sm, data=fertpref.pred.sm)
sm.fertpref.CI <- apply(sm.fertpref.sim,2,PI,prob=0.95)
csp.fertpref.sim <- sim(m4.pd_csp, data=fertpref.pred.csp)
csp.fertpref.CI <- apply(csp.fertpref.sim,2,PI,prob=0.95)
#-------------------------------------------------------------------------------------
paidsex.pred.sm <- data_frame(clustno=mean(male.DSF.sm$clustno),houseno=mean(male.DSF.sm$houseno),paidsex=cov.seq2,
                               agegp=mean(na.omit(male.DSF.sm$agegp)),educ=mean(na.omit(male.DSF.sm$educ)),employ=mean(na.omit(male.DSF.sm$employ)),
                               travel=mean(na.omit(male.DSF.sm$travel)),mmc=mean(na.omit(male.DSF.sm$mmc)),mstatus=mean(na.omit(male.DSF.sm$mstatus)),
                               agesexgp=mean(na.omit(male.DSF.sm$agesexgp)),fertpref=mean(na.omit(male.DSF.sm$fertpref)))

paidsex.pred.csp <- data_frame(clustno=mean(male.DSF.csp$clustno),houseno=mean(male.DSF.csp$houseno),paidsex=cov.seq2,
                                agegp=mean(na.omit(male.DSF.csp$agegp)),educ=mean(na.omit(male.DSF.csp$educ)),employ=mean(na.omit(male.DSF.csp$employ)),
                                travel=mean(na.omit(male.DSF.csp$travel)),mmc=mean(na.omit(male.DSF.csp$mmc)),mstatus=mean(na.omit(male.DSF.csp$mstatus)),
                                agesexgp=mean(na.omit(male.DSF.csp$agesexgp)),fertpref=mean(na.omit(male.DSF.csp$fertpref)))

sm.paidsex.mu <- link(m3.pd_sm, data=paidsex.pred.sm)
sm.paidsex.mean <- apply(sm.paidsex.mu,2,mean)
sm.paidsex.PI <- apply(sm.paidsex.mu,2,PI,prob=0.95)
csp.paidsex.mu <- link(m4.pd_csp, data=paidsex.pred.csp)
csp.paidsex.mean <- apply(csp.paidsex.mu,2,mean)
csp.paidsex.PI <- apply(csp.paidsex.mu,2,PI,prob=0.95)

sm.paidsex.sim <- sim(m3.pd_sm, data=paidsex.pred.sm)
sm.paidsex.CI <- apply(sm.paidsex.sim,2,PI,prob=0.95)
csp.paidsex.sim <- sim(m4.pd_csp, data=paidsex.pred.csp)
csp.paidsex.CI <- apply(csp.paidsex.sim,2,PI,prob=0.95)
#-------------------------------------------------------------------------------------

dev.off()
par(mfrow=c(3,3),mai = c(0.5, 0.5, 0.2, 0.1))
plot(sm ~ agegp, data=male.DSF.labelled, type="n",xlab="",ylab="")
mtext("Age",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("A",side=3,line=0); 
shade(sm.agegp.PI,cov.seq3,col="lightblue")
lines(cov.seq3,sm.agegp.mean, col="darkblue",lwd=3)
lines(cov.seq3,csp.agegp.mean, col="red3",lwd=3,lty="dotted")
shade(csp.agegp.PI,cov.seq3,col=rainbow(70, alpha = 0.3))
legend("topleft", legend=c("SM", "CSP"), col=c("darkblue", "red3"), lty=1:1, cex=0.7, lwd=2)

plot(sm ~ educ, data=male.DSF.labelled, type="n",xlab="",ylab="")
mtext("Education",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("B",side=3,line=0);
shade(sm.educ.PI,cov.seq3,col="lightblue")
lines(cov.seq3,sm.educ.mean, col="darkblue",lwd=3)
lines(cov.seq3,csp.educ.mean, col="red3",lwd=3,lty="dotted")
shade(csp.educ.PI,cov.seq3,col=rainbow(70, alpha = 0.4))
legend("topleft", legend=c("SM", "CSP"), col=c("darkblue", "red3"), lty=1:1, cex=0.7, lwd=2)

plot(sm ~ employ, data=male.DSF.labelled, type="n",xlab="",ylab="")
mtext("Employment",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("C",side=3,line=0);
shade(sm.employ.PI,cov.seq2,col="lightblue")
lines(cov.seq2,sm.employ.mean, col="darkblue",lwd=3)
lines(cov.seq2,csp.employ.mean, col="red3",lwd=3,lty="dotted")
shade(csp.employ.PI,cov.seq2,col=rainbow(70, alpha = 0.4))
legend("topleft", legend=c("SM", "CSP"), col=c("darkblue", "red3"), lty=1:1, cex=0.7, lwd=2)

plot(sm ~ travel, data=male.DSF.labelled, type="n",xlab="",ylab="")
mtext("Times away from home",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("D",side=3,line=0);
shade(sm.travel.PI,cov.seq3,col="lightblue")
lines(cov.seq3,sm.travel.mean, col="darkblue",lwd=3)
lines(cov.seq3,csp.travel.mean, col="red3",lwd=3,lty="dotted")
shade(csp.travel.PI,cov.seq3,col=rainbow(70, alpha = 0.4))
legend("topleft", legend=c("SM", "CSP"), col=c("darkblue", "red3"), lty=1:1, cex=0.7, lwd=2)

plot(sm ~ mmc, data=male.DSF.labelled, type="n",xlab="",ylab="")
mtext("Circumcision",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("E",side=3,line=0);
shade(sm.mmc.PI,cov.seq2,col="lightblue")
lines(cov.seq2,sm.mmc.mean, col="darkblue",lwd=3)
lines(cov.seq2,csp.mmc.mean, col="red3",lwd=3,lty="dotted")
shade(csp.mmc.PI,cov.seq2,col=rainbow(70, alpha = 0.4))
legend("topleft", legend=c("SM", "CSP"), col=c("darkblue", "red3"), lty=1:1, cex=0.7, lwd=2)

plot(sm ~ mstatus, data=male.DSF.labelled, type="n",xlab="",ylab="")
mtext("Marital status",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("F",side=3,line=0);
shade(sm.mstatus.PI,cov.seq2,col="lightblue")
lines(cov.seq2,sm.mstatus.mean, col="darkblue",lwd=3)
lines(cov.seq2,csp.mstatus.mean, col="red3",lwd=3,lty="dotted")
shade(csp.mstatus.PI,cov.seq2,col=rainbow(70, alpha = 0.4))
legend("topleft", legend=c("SM", "CSP"), col=c("darkblue", "red3"), lty=1:1, cex=0.7, lwd=2)

plot(sm ~ agesexgp, data=male.DSF.labelled, type="n",xlab="",ylab="")
mtext("Age at first sex",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("G",side=3,line=0)
shade(sm.agesexgp.PI,cov.seq3,col="lightblue")
lines(cov.seq3,sm.agesexgp.mean, col="darkblue",lwd=3)
lines(cov.seq3,csp.agesexgp.mean, col="red3",lwd=3,lty="dotted")
shade(csp.agesexgp.PI,cov.seq3,col=rainbow(70, alpha = 0.4))
legend("topleft", legend=c("SM", "CSP"), col=c("darkblue", "red3"), lty=1:1, cex=0.7, lwd=2)

plot(sm ~ fertpref, data=male.DSF.labelled, type="n",xlab="",ylab="")
mtext("Fertility preference",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("H",side=3,line=0)
shade(sm.fertpref.PI,cov.seq2,col="lightblue")
lines(cov.seq2,sm.fertpref.mean, col="darkblue",lwd=3)
lines(cov.seq2,csp.fertpref.mean, col="red3",lwd=3,lty="dotted")
shade(csp.fertpref.PI,cov.seq2,col=rainbow(70, alpha = 0.4))
legend("topleft", legend=c("SM", "CSP"), col=c("darkblue", "red3"), lty=1:1, cex=0.7, lwd=2)

plot(sm ~ paidsex, data=male.DSF.labelled, type="n",xlab="",ylab="")
mtext("Paid sex",side=1,line=2,cex=0.7); mtext("Probability",side=2,line=2,cex=0.7); mtext("I",side=3,line=0)
shade(sm.paidsex.PI,cov.seq2,col="lightblue")
lines(cov.seq2,sm.paidsex.mean, col="darkblue",lwd=3)
lines(cov.seq2,csp.paidsex.mean, col="red3",lwd=3,lty="dotted")
shade(csp.paidsex.PI,cov.seq2,col=rainbow(70, alpha = 0.4))
legend("topleft", legend=c("SM", "CSP"), col=c("darkblue", "red3"), lty=1:1, cex=0.7, lwd=2)

#-------------------------------------------------------------------------------------
    