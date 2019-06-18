#written by Deus Thindwa
#11/04/2019

#====================LOAD REQUIRED PACKAGES AND DATASET====================

DDHP.packages <-c("foreign","tidyverse","janitor","readstata13","rethinking","rstan","plyr")
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

#Child desire
male.DSF$fertpref <- if_else(male.DSF$fertpref=="wants no more",1,if_else(male.DSF$fertpref=="wants after 2+ years",1,
                     if_else(male.DSF$fertpref=="wants, unsure timing",1,if_else(male.DSF$fertpref=="wants within 2 years",2,NULL))))
male.DSF$fertpref <-recode_factor(male.DSF$fertpref,`1`="no",`2`="yes")

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

male.DSF %>% tabyl(fertpref) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(fertpref,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(fertpref,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$fertpref,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$fertpref,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(paidsex,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(paidsex,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(paidsex,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$paidsex,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$paidsex,male.DSF$csp, correct=TRUE)

male.label <-subset(male.DSF, select=c(clustno,houseno,sm,csp,agegp,educ,employ,travel,mmc, mstatus,agesexgp,fertpref,paidsex))
male <-subset(male.DSF, select=c(clustno,houseno,sm,csp,agegp,educ,employ,travel,mmc, mstatus,agesexgp,fertpref,paidsex))
rm(male.dhs, male.DS,male.DSF)

#====================PREPARE DATASETS FOR MODEL FITTING====================

#recode all predictors to represent integer class for binomial model fitting
male$agegp <-as.integer(recode_factor(male$agegp,`15-19`=1,`20-29`=2,`30+`=3))
male$educ <-as.integer(recode_factor(male$educ,`no education`=1,`primary`=2,`secondary`=3))
male$employ <-as.integer(recode_factor(male$employ,`none`=1,`working`=2))
male$travel <-as.integer(recode_factor(male$travel,`none`=1,`less times`=2,`more times`=3))
male$mmc <-as.integer(recode_factor(male$mmc,`no`=1,`yes`=2))
male$mstatus <-as.integer(recode_factor(male$mstatus,`single`=1,`married`=2))
male$agesexgp <-as.integer(recode_factor(male$agesexgp,`<16`=1,`16-19`=2,`20+`=3))
male$fertpref <-as.integer(recode_factor(male$fertpref,`no`=1,`yes`=2))
male$paidsex <-as.integer(recode_factor(male$paidsex,`no`=1,`yes`=2))

#integerize the enumeration area and household ids so they are contiguous.
male$clustno <- as.integer(male$clustno)
male$houseno <- as.integer(male$houseno)
sort(unique(male$clustno))
sort(unique(male$houseno))

#create separate datasets for each outcome and remove NAs
#enumeration areas restricted to maximum of 843 due to lack of enough data in cluster >843
male.sm <- subset(male,!is.na(sm)); male.sm <- male.sm[,-4]
male.csp <- subset(male,!is.na(csp)); male.csp <- male.csp[-3]
male.sm$clustno[male.sm$clustno >843] <- 843 
male.csp$clustno[male.csp$clustno >843] <- 843
rm(male)

#==============FIT MODELS FOR "SM" AND SAMPLE FROM POST.DISTR USING HMC==================

#model1: without random-effects
set.seed(1988)
sm.model1 <- map2stan(
  alist(sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+Age*agegp+Education*educ+Employment*employ+Travel*travel+Circumcision*mmc+Marriage*mstatus+Sexual_debut*agesexgp+Child_desire*fertpref+Paid_sex*paidsex,
    c(Age,Education,Employment,Travel,Circumcision,Marriage,Sexual_debut,Child_desire,Paid_sex) ~ dnorm(0,1),
    a ~ dnorm(0,1)), 
    data=as.data.frame(na.omit(male.sm)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=9)

#model2: with household random-effects variable
set.seed(1988)
sm.model2 <- map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_houseno[houseno]+Age*agegp+Education*educ+Employment*employ+Travel*travel+Circumcision*mmc+Marriage*mstatus+Sexual_debut*agesexgp+Child_desire*fertpref+Paid_sex*paidsex,
    c(Age,Education,Employment,Travel,Circumcision,Marriage,Sexual_debut,Child_desire,Paid_sex) ~ dnorm(0,1),
    a ~ dnorm(0,1), 
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    s_houseno ~ dcauchy(0,1)), 
    data=as.data.frame(na.omit(male.sm)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=8)

#model3: with enumeration area random-effects variable
set.seed(1988)
sm.model3 <- map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_clustno[clustno]+Age*agegp+Education*educ+Employment*employ+Travel*travel+Circumcision*mmc+Marriage*mstatus+Sexual_debut*agesexgp+Child_desire*fertpref+Paid_sex*paidsex,
    c(Age,Education,Employment,Travel,Circumcision,Marriage,Sexual_debut,Child_desire,Paid_sex) ~ dnorm(0,1),
    a ~ dnorm(0,1), 
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_clustno ~ dcauchy(0,1)), 
  data=as.data.frame(na.omit(male.sm)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=7)

#model4: with household and enumeration area random-effects variables
set.seed(1988)
sm.model4 <- map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_houseno[houseno]+a_clustno[clustno]+Age*agegp+Education*educ+Employment*employ+Travel*travel+Circumcision*mmc+Marriage*mstatus+Sexual_debut*agesexgp+Child_desire*fertpref+Paid_sex*paidsex,
    c(Age,Education,Employment,Travel,Circumcision,Marriage,Sexual_debut,Child_desire,Paid_sex) ~ dnorm(0,1),
    a ~ dnorm(0,1),
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_houseno ~ dcauchy(0,1),
    s_clustno ~ dcauchy(0,1)),
  data=as.data.frame(na.omit(male.sm)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=6)

#==============SERIAL MONOGAMY MODEL AND MCMC DIAGNOSTICS==================

#checking models' convergence and covariate correlation
cov.par=c("Age","Education","Employment","Travel","Circumcision","Marriage","Sexual_debut","Child_desire","Paid_sex")
plot(sm.model1, pars=cov.par)
pairs(sm.model1, pars=cov.par)
summary(sm.model1)

plot(sm.model2, pars=cov.par)
pairs(sm.model2, pars=cov.par)
summary(sm.model2)

plot(sm.model3, pars=cov.par)
pairs(sm.model3, pars=cov.par)
summary(sm.model3)

plot(sm.model4, pars=cov.par)
pairs(sm.model4, pars=cov.par)
summary(sm.model4)

#checking divergence of mcmc
divergent(sm.model1)
divergent(sm.model2)
divergent(sm.model3)
divergent(sm.model4)

#full report of sampler parameters
dev.off()
dashboard(sm.model1)
dashboard(sm.model2)
dashboard(sm.model3)
dashboard(sm.model4)

#==============FIT MODELS FOR "CSP" AND SAMPLE FROM POST.DISTR USING HMC==================

#model1: without random-effects
set.seed(1988)
csp.model1 <- map2stan(
  alist(csp ~ dbinom(1,csp_p),
        logit(csp_p) <- a+Age*agegp+Education*educ+Employment*employ+Travel*travel+Circumcision*mmc+Marriage*mstatus+Sexual_debut*agesexgp+Child_desire*fertpref+Paid_sex*paidsex,
        c(Age,Education,Employment,Travel,Circumcision,Marriage,Sexual_debut,Child_desire,Paid_sex) ~ dnorm(0,1),
        a ~ dnorm(0,1)), 
  data=as.data.frame(na.omit(male.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=9)

#model2: with household random-effects variable
set.seed(1988)
csp.model2 <- map2stan(
  alist(
    csp ~ dbinom(1,csp_p),
    logit(csp_p) <- a+a_houseno[houseno]+Age*agegp+Education*educ+Employment*employ+Travel*travel+Circumcision*mmc+Marriage*mstatus+Sexual_debut*agesexgp+Child_desire*fertpref+Paid_sex*paidsex,
    c(Age,Education,Employment,Travel,Circumcision,Marriage,Sexual_debut,Child_desire,Paid_sex) ~ dnorm(0,1),
    a ~ dnorm(0,1), 
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    s_houseno ~ dcauchy(0,1)), 
  data=as.data.frame(na.omit(male.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=8)

#model3: with enumeration area random-effects variable
set.seed(1988)
csp.model3 <- map2stan(
  alist(
    csp ~ dbinom(1,csp_p),
    logit(csp_p) <- a+a_clustno[clustno]+Age*agegp+Education*educ+Employment*employ+Travel*travel+Circumcision*mmc+Marriage*mstatus+Sexual_debut*agesexgp+Child_desire*fertpref+Paid_sex*paidsex,
    c(Age,Education,Employment,Travel,Circumcision,Marriage,Sexual_debut,Child_desire,Paid_sex) ~ dnorm(0,1),
    a ~ dnorm(0,1), 
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_clustno ~ dcauchy(0,1)), 
  data=as.data.frame(na.omit(male.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=7)

 #model4: with household and enumeration area random-effects variables
set.seed(1988)
csp.model4 <- map2stan(
  alist(
    csp ~ dbinom(1,csp_p),
    logit(csp_p) <- a+a_houseno[houseno]+a_clustno[clustno]+Age*agegp+Education*educ+Employment*employ+Travel*travel+Circumcision*mmc+Marriage*mstatus+Sexual_debut*agesexgp+Child_desire*fertpref+Paid_sex*paidsex,
    c(Age,Education,Employment,Travel,Circumcision,Marriage,Sexual_debut,Child_desire,Paid_sex) ~ dnorm(0,1),
    a ~ dnorm(0,1),
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_houseno ~ dcauchy(0,1),
    s_clustno ~ dcauchy(0,1)),
  data=as.data.frame(na.omit(male.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=6)

#==============CONCURRENT SEXUAL PARTNERSHIP MODEL AND MCMC DIAGNOSTICS==================

#checking models' convergence and covariate correlation
plot(csp.model1, pars=cov.par)
pairs(csp.model1, pars=cov.par)
summary(csp.model1)

plot(csp.model2, pars=cov.par)
pairs(csp.model2, pars=cov.par)
summary(csp.model2)

plot(csp.model3, pars=cov.par)
pairs(csp.model3, pars=cov.par)
summary(csp.model3)

plot(csp.model4, pars=cov.par)
pairs(csp.model4, pars=cov.par)
summary(csp.model4)

#checking divergence of mcmc
divergent(csp.model1)
divergent(csp.model2)
divergent(csp.model3)
divergent(csp.model4)

#full report of sampler parameters
dev.off()
dashboard(csp.model1)
dashboard(csp.model2)
dashboard(csp.model3)
dashboard(csp.model4)

#==============POSTERIOR ANALYSIS==================

#stratum-specific posterior distribution estimates for sm and csp from final models (Table 2)
set.seed(7)
sm.model3.J <- map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_clustno[clustno]+Age[agegp]+Education[educ]+Employment[employ]+Travel[travel]+Circumcision[mmc]+Marriage*mstatus+Sexual_debut[agesexgp]+Child_desire[fertpref]+Paid_sex[paidsex],
    Age[agegp] ~ dnorm(0,1),
    Education[educ] ~ dnorm(0,1),
    Employment[employ] ~ dnorm(0,1),
    Travel[travel] ~ dnorm(0,1),
    Circumcision[mmc] ~ dnorm(0,1),
    Marriage ~ dnorm(0,1),
    Sexual_debut[agesexgp] ~ dnorm(0,1),
    Child_desire[fertpref] ~ dnorm(0,1),
    Paid_sex[paidsex] ~ dnorm(0,1),
    a ~ dnorm(0,1), 
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_clustno ~ dcauchy(0,1)), 
  data=as.data.frame(na.omit(male.sm)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=7)

set.seed(6)
csp.model4.J <- map2stan(
  alist(
    csp ~ dbinom(1,csp_p),
    logit(csp_p) <- a+a_clustno[clustno]+a_houseno[houseno]+Age[agegp]+Education[educ]+Employment[employ]+Travel[travel]+Circumcision[mmc]+Marriage*mstatus+Sexual_debut[agesexgp]+Child_desire[fertpref]+Paid_sex[paidsex],
    Age[agegp] ~ dnorm(0,1),
    Education[educ] ~ dnorm(0,1),
    Employment[employ] ~ dnorm(0,1),
    Travel[travel] ~ dnorm(0,1),
    Circumcision[mmc] ~ dnorm(0,1),
    Marriage ~ dnorm(0,1),
    Sexual_debut[agesexgp] ~ dnorm(0,1),
    Child_desire[fertpref] ~ dnorm(0,1),
    Paid_sex[paidsex] ~ dnorm(0,1),
    a ~ dnorm(0,1), 
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_houseno ~ dcauchy(0,1),
    s_clustno ~ dcauchy(0,1)), 
  data=as.data.frame(na.omit(male.csp)),chains=4,iter=4000,warmup=1000,cores=3,rng_seed=7)

summary(sm.model3.J)

J3<-data.frame(extract.samples(sm.model3.J))
mean(logistic(J3$Age.1)); quantile(logistic(J3$Age.1),prob=c(0.025,0.975))
mean(logistic(J3$Age.2)); quantile(logistic(J3$Age.2),prob=c(0.025,0.975))
mean(logistic(J3$Age.3)); quantile(logistic(J3$Age.3),prob=c(0.025,0.975))

mean(logistic(J3$Education.1)); quantile(logistic(J3$Education.1),prob=c(0.025,0.975))
mean(logistic(J3$Education.2)); quantile(logistic(J3$Education.2),prob=c(0.025,0.975))
mean(logistic(J3$Education.3)); quantile(logistic(J3$Education.3),prob=c(0.025,0.975))

mean(logistic(J3$Employment.1)); quantile(logistic(J3$Employment.1),prob=c(0.025,0.975))
mean(logistic(J3$Employment.2)); quantile(logistic(J3$Employment.2),prob=c(0.025,0.975))

mean(logistic(J3$Travel.1)); quantile(logistic(J3$Travel.1),prob=c(0.025,0.975))
mean(logistic(J3$Travel.2)); quantile(logistic(J3$Travel.2),prob=c(0.025,0.975))
mean(logistic(J3$Travel.3)); quantile(logistic(J3$Travel.3),prob=c(0.025,0.975))

mean(logistic(J3$Travel.1)); quantile(logistic(J3$Travel.1),prob=c(0.025,0.975))
mean(logistic(J3$Travel.2)); quantile(logistic(J3$Travel.2),prob=c(0.025,0.975))
mean(logistic(J3$Travel.3)); quantile(logistic(J3$Travel.3),prob=c(0.025,0.975))

mean(logistic(J3$Paid_sex.1)); quantile(logistic(J3$Paid_sex.1),prob=c(0.025,0.975))
mean(logistic(J3$Paid_sex.2)); quantile(logistic(J3$Paid_sex.2),prob=c(0.025,0.975))

#posterior density and traceplots of parameters (S1 Figure)
sm.model4X <- data.frame(p=extract.samples(sm.model4))
sm.model4X <- sm.model4X[,1:9]
csp.model4X <- data.frame(p=extract.samples(csp.model4))
csp.model4X <- csp.model4X[,1:9]

dev.off()
par(mfrow=c(3,6),mai = c(0.6, 0.4, 0.2, 0.1))
plot(logistic(sm.model4X$p.Age), col="lightblue", xlab="",ylab="",main="A",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(sm.model4X$p.Age),xlab="",ylab="",main="A",col="darkblue",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(sm.model4X$p.Education), col="lightblue", xlab="",ylab="",main="B",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(sm.model4X$p.Education),xlab="",ylab="",main="B",col="darkblue",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(sm.model4X$p.Employment), col="lightblue", xlab="",ylab="",main="C",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(sm.model4X$p.Employment),xlab="",ylab="",main="C",col="darkblue",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(sm.model4X$p.Travel), col="lightblue", xlab="",ylab="",main="D",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(sm.model4X$p.Travel),xlab="",ylab="",main="D",col="darkblue",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(sm.model4X$p.Circumcision), col="lightblue", xlab="",ylab="",main="E",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(sm.model4X$p.Circumcision),xlab="",ylab="",main="E",col="darkblue",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(sm.model4X$p.Marriage), col="lightblue", xlab="",ylab="",main="F",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(sm.model4X$p.Marriage),xlab="",ylab="",main="F",col="darkblue",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(sm.model4X$p.Sexual_debut), col="lightblue", xlab="",ylab="",main="G",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(sm.model4X$p.Sexual_debut),xlab="",ylab="",main="G",col="darkblue",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(sm.model4X$p.Child_desire), col="lightblue", xlab="",ylab="",main="H",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(sm.model4X$p.Child_desire),xlab="",ylab="",main="H",col="darkblue",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(sm.model4X$p.Paid_sex), col="lightblue", xlab="",ylab="",main="I",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(sm.model4X$p.Paid_sex),xlab="",ylab="",main="I",col="darkblue",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)
#-------------------------------------------------------------------------------------
dev.off()
par(mfrow=c(3,6),mai = c(0.6, 0.4, 0.2, 0.1))
plot(logistic(csp.model4X$p.Age), col="red1", xlab="",ylab="",main="A",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(csp.model4X$p.Age),xlab="",ylab="",main="A",col="red3",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(csp.model4X$p.Education), col="red1", xlab="",ylab="",main="B",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(csp.model4X$p.Education),xlab="",ylab="",main="B",col="red3",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(csp.model4X$p.Employment), col="red1", xlab="",ylab="",main="C",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(csp.model4X$p.Employment),xlab="",ylab="",main="C",col="red3",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(csp.model4X$p.Travel), col="red1", xlab="",ylab="",main="D",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(csp.model4X$p.Travel),xlab="",ylab="",main="D",col="red3",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(csp.model4X$p.Circumcision), col="red1", xlab="",ylab="",main="E",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(csp.model4X$p.Circumcision),xlab="",ylab="",main="E",col="red3",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(csp.model4X$p.Marriage), col="red1", xlab="",ylab="",main="F",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(csp.model4X$p.Marriage),xlab="",ylab="",main="F",col="red3",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(csp.model4X$p.Sexual_debut), col="red1", xlab="",ylab="",main="G",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(csp.model4X$p.Sexual_debut),xlab="",ylab="",main="G",col="red3",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(csp.model4X$p.Child_desire), col="red1", xlab="",ylab="",main="H",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(csp.model4X$p.Child_desire),xlab="",ylab="",main="H",col="red3",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

plot(logistic(csp.model4X$p.Paid_sex), col="red1", xlab="",ylab="",main="I",type="l")
mtext("Posterior samples",side=1,line=2,cex=0.7); mtext("",side=2,line=2,cex=0.7)
dens(logistic(csp.model4X$p.Paid_sex),xlab="",ylab="",main="I",col="red3",lwd=3)
mtext("Density",side=2,line=2,cex=0.7);mtext("Probability",side=1,line=2,cex=0.7)

#model comparison and posterior means plots for each outcome (S2 Figure)
dev.off()
cov.par=c("Age","Education","Employment","Travel","Circumcision","Marriage","Sexual_debut","Child_desire","Paid_sex")
par(mfrow=c(1,2),mai=c(0.9,0,0.5,0.1))
plot(compare(sm.model1,sm.model2,sm.model3,sm.model4))
title(main="A",adj=0.3)
plot(coeftab(sm.model2,sm.model1,sm.model3,sm.model4),pars=cov.par)
title(main="B",adj=0.3)

dev.off()
par(mfrow=c(1,2),mai=c(0.9,0,0.5,0.1))
plot(compare(csp.model2,csp.model1,csp.model3,csp.model4))
title(main="A",adj=0.3)
plot(coeftab(csp.model2,csp.model1,csp.model3,csp.model4),pars=cov.par)
title(main="B",adj=0.3)

#counterfactual plot (marginal posterior density) of each predictor of sm (Figure 1)
#-------------------------------------------------------------------------------------
cov.par=c("Age","Education","Employment","Travel","Circumcision","Marriage","Sexual_debut","Child_desire","Paid_sex")
cov.seq3=seq.int(from=1, to=3, length.out=30)
cov.seq2=seq.int(from=1, to=2, length.out=30)

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
plot(sm ~ agegp, data=male.label, type="n",xlab="",ylab="")
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
    