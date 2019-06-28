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

male.label <-subset(male.DSF, select=c(clustno,houseno,sm,csp,agegp,educ,employ,travel,mmc,agesexgp,fertpref,paidsex))
male <-subset(male.DSF, select=c(clustno,houseno,sm,csp,agegp,educ,employ,travel,mmc,agesexgp,fertpref,paidsex))
rm(male.dhs, male.DS,male.DSF)

#====================PREPARE DATASETS FOR MODEL FITTING====================

#recode all predictors to represent integer class for binomial model fitting
male$agegp <-coerce_index(recode_factor(male$agegp,`15-19`=1L,`20-29`=2L,`30+`=3L))
male$educ <-coerce_index(recode_factor(male$educ,`no education`=1L,`primary`=2L,`secondary`=3L))
male$employ <-coerce_index(recode_factor(male$employ,`none`=1L,`working`=2L))
male$travel <-coerce_index(recode_factor(male$travel,`none`=1L,`less times`=2L,`more times`=3L))
male$mmc <-coerce_index(recode_factor(male$mmc,`no`=1L,`yes`=2L))
male$agesexgp <-coerce_index(recode_factor(male$agesexgp,`<16`=1L,`16-19`=2L,`20+`=3L))
male$fertpref <-coerce_index(recode_factor(male$fertpref,`no`=1L,`yes`=2L))
male$paidsex <-coerce_index(recode_factor(male$paidsex,`no`=1L,`yes`=2L))

#integerize the enumeration area and household ids so they are contiguous.
male.sm <- subset(male,!is.na(sm)); male.sm <- male.sm[,-4]
male.csp <- subset(male,!is.na(csp)); male.csp <- male.csp[-3]

male.sm$sm<-recode_factor(male.sm$sm,`0`=0L,`1`=1L)
male.csp$csp<-recode_factor(male.csp$csp,`0`=0L,`1`=1L)
male.sm$clustno <- coerce_index(male.sm$clustno)
male.csp$clustno <- coerce_index(male.csp$clustno)
male$houseno <- coerce_index(male$houseno)
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

male.sm$mstatus <- if_else(male.sm$mstatus==1L,0L,1L) #so can extract single(0) and married(1) since marriage not entering models as index variable

#refit the final selected model for SM but with strata
set.seed(7)
sm.model4.f <- map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_clustno[clustno]+a_houseno[houseno]+Age[agegp]+Education[educ]+Employment[employ]+Travel[travel]+Circumcision[mmc]+Marriage[mstatus]+Sexual_debut[agesexgp]+Child_desire[fertpref]+Paid_sex[paidsex],
    Age[agegp] ~ dnorm(0,1),
    Education[educ] ~ dnorm(0,1),
    Employment[employ] ~ dnorm(0,1),
    Travel[travel] ~ dnorm(0,1),
    Circumcision[mmc] ~ dnorm(0,1),
    Marriage[mstatus] ~ dnorm(0,1),
    Sexual_debut[agesexgp] ~ dnorm(0,1),
    Child_desire[fertpref] ~ dnorm(0,1),
    Paid_sex[paidsex] ~ dnorm(0,1),
    a ~ dnorm(0,1), 
    a_houseno[houseno] ~ dnorm(0,s_houseno),
    a_clustno[clustno] ~ dnorm(0,s_clustno),
    s_houseno ~ dcauchy(0,1),
    s_clustno ~ dcauchy(0,1)), 
  data=as.data.frame(na.omit(male.sm)),chains=1,iter=4000,warmup=1000,cores=3,rng_seed=7)

#Probability of serial monogamy and concurrent sexual partnership by covariate levels (Table 2)
sm.x<-data.frame(extract.samples(sm.model4.f))
sm.x <- subset(sm.x[,1:22])
sm.x <- sm.x[c(1:13,15:21,22,14)]
for(i in colnames(sm.x)){
      print(precis(data.frame(logistic(sm.x[[i]])),prob=0.95),justify="right",digits=3)
}
precis(data.frame(logistic(sm.x$a)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Marriage+sm.x$a)),prob=0.95,digits=3)

csp.x<-data.frame(extract.samples(csp.model4.f))
csp.x <- subset(csp.x[,1:22])
csp.x <- csp.x[c(1:13,15:21,22,14)]
for(i in colnames(csp.x)){
  print(precis(data.frame(logistic(csp.x[[i]])),prob=0.95),justify="right",digits=3)
}
precis(data.frame(logistic(csp.x$a)),prob=0.95,digits=3)
precis(data.frame(logistic(csp.x$Marriage+sm.x$a)),prob=0.95,digits=3)

#Probability of serial monogamy and concurrent sexual partnership by covariate levels (Table 2)
precis(data.frame(logistic(sm.x$Age.2-sm.x$Age.1)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Age.3-sm.x$Age.1)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Education.1)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Education.2)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Education.3)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Employment.1)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Employment.2)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Travel.1)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Travel.2)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Travel.3)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Circumcision.1)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Circumcision.2)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Sexual_debut.1)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Sexual_debut.2)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Sexual_debut.3)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Child_desire.1)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Child_desire.2)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Paid_sex.1)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Paid_sex.2)),prob=0.95,digits=3)

precis(data.frame(logistic(sm.x$a)),prob=0.95)
precis(data.frame(logistic(sm.x$Marriage+sm.x$a)),prob=0.95)

#Differences in probability of serial monogamy and concurrent sexual partnership by covariate levels (Table 2)
precis(data.frame(logistic(sm.x$Age.2-sm.x$Age.1)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Age.3-sm.x$Age.1)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Education.2-sm.x$Education.1)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Education.3-sm.x$Education.1)),prob=0.95,digits=3)
precis(data.frame(logistic(sm.x$Employment.2-sm.x$Employment.1)),prob=0.95,digits=3)



mean(logistic(sm.x$Travel.2-sm.x$Travel.1)); quantile(logistic(sm.x$Travel.2-sm.x$Travel.1),prob=c(0.025,0.975))
mean(logistic(sm.x$Travel.3-sm.x$Travel.1)); quantile(logistic(sm.x$Travel.3-sm.x$Travel.1),prob=c(0.025,0.975))

mean(exp(sm.x$Circumcision.2-sm.x$Circumcision.1)); quantile(exp(sm.x$Circumcision.2-sm.x$Circumcision.1),prob=c(0.025,0.975))
mean(logistic(sm.x$Circumcision.2-sm.x$Circumcision.1)); quantile(logistic(sm.x$Circumcision.2-sm.x$Circumcision.1),prob=c(0.025,0.975))

mean(exp(sm.x$Marriage)); quantile(exp(sm.x$Marriage),prob=c(0.025,0.975))
mean(logistic(sm.x$Marriage+sm.x$a)); quantile(logistic(sm.x$Marriage+sm.x$a),prob=c(0.025,0.975))
precis(data.frame(exp(sm.x$a)),prob=0.95)
precis(data.frame(logistic(sm.x$Marriage+sm.x$a)),prob=0.95)

mean(exp(sm.x$Sexual_debut.2-sm.x$Sexual_debut.1)); quantile(exp(sm.x$Sexual_debut.2-sm.x$Sexual_debut.1),prob=c(0.025,0.975))
mean(exp(sm.x$Sexual_debut.3-sm.x$Sexual_debut.1)); quantile(exp(sm.x$Sexual_debut.3-sm.x$Sexual_debut.1),prob=c(0.025,0.975))
mean(logistic(sm.x$Sexual_debut.2-sm.x$Sexual_debut.1)); quantile(logistic(sm.x$Sexual_debut.2-sm.x$Sexual_debut.1),prob=c(0.025,0.975))
mean(logistic(sm.x$Sexual_debut.3-sm.x$Sexual_debut.1)); quantile(logistic(sm.x$Sexual_debut.3-sm.x$Sexual_debut.1),prob=c(0.025,0.975))

mean(exp(sm.x$Child_desire.2-sm.x$Child_desire.1)); quantile(exp(sm.x$Child_desire.2-sm.x$Child_desire.1),prob=c(0.025,0.975))
mean(logistic(sm.x$Child_desire.2-sm.x$Child_desire.1)); quantile(logistic(sm.x$Child_desire.2-sm.x$Child_desire.1),prob=c(0.025,0.975))

mean(exp(sm.x$Paid_sex.2-sm.x$Paid_sex.1)); quantile(exp(sm.x$Paid_sex.2-sm.x$Paid_sex.1),prob=c(0.025,0.975))
mean(logistic(sm.x$Paid_sex.2-sm.x$Paid_sex.1)); quantile(logistic(sm.x$Paid_sex.2-sm.x$Paid_sex.1),prob=c(0.025,0.975))

#refit the final selected model for concurrent sexual partnership but with strata
set.seed(1988)
csp.model4.f <- map2stan(
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

#Relative and absolute differences between covariate categories in predicting concurrent sexual partnership (Table 2)
csp.x<-data.frame(extract.samples(csp.model4.f))
mean(exp(csp.x$Age.2-csp.x$Age.1)); quantile(exp(csp.x$Age.2-csp.x$Age.1),prob=c(0.025,0.975))
mean(exp(csp.x$Age.3-csp.x$Age.1)); quantile(exp(csp.x$Age.3-csp.x$Age.1),prob=c(0.025,0.975))
mean(logistic(csp.x$Age.2-csp.x$Age.1)); quantile(logistic(csp.x$Age.2-csp.x$Age.1),prob=c(0.025,0.975))
mean(logistic(csp.x$Age.3-csp.x$Age.1)); quantile(logistic(csp.x$Age.3-csp.x$Age.1),prob=c(0.025,0.975))

mean(exp(csp.x$Education.2-csp.x$Education.1)); quantile(exp(csp.x$Education.2-csp.x$Education.1),prob=c(0.025,0.975))
mean(exp(csp.x$Education.3-csp.x$Education.1)); quantile(exp(csp.x$Education.3-csp.x$Education.1),prob=c(0.025,0.975))
mean(logistic(csp.x$Education.2-csp.x$Education.1)); quantile(logistic(csp.x$Education.2-csp.x$Education.1),prob=c(0.025,0.975))
mean(logistic(csp.x$Education.3-csp.x$Education.1)); quantile(logistic(csp.x$Education.3-csp.x$Education.1),prob=c(0.025,0.975))

mean(exp(csp.x$Employment.2-csp.x$Employment.1)); quantile(exp(csp.x$Employment.2-csp.x$Employment.1),prob=c(0.025,0.975))
mean(logistic(csp.x$Employment.2-csp.x$Employment.1)); quantile(logistic(csp.x$Employment.2-csp.x$Employment.1),prob=c(0.025,0.975))

mean(exp(csp.x$Travel.2-csp.x$Travel.1)); quantile(exp(csp.x$Travel.2-csp.x$Travel.1),prob=c(0.025,0.975))
mean(exp(csp.x$Travel.3-csp.x$Travel.1)); quantile(exp(csp.x$Travel.3-csp.x$Travel.1),prob=c(0.025,0.975))
mean(logistic(csp.x$Travel.2-csp.x$Travel.1)); quantile(logistic(csp.x$Travel.2-csp.x$Travel.1),prob=c(0.025,0.975))
mean(logistic(csp.x$Travel.3-csp.x$Travel.1)); quantile(logistic(csp.x$Travel.3-csp.x$Travel.1),prob=c(0.025,0.975))

mean(exp(csp.x$Circumcision.2-csp.x$Circumcision.1)); quantile(exp(csp.x$Circumcision.2-csp.x$Circumcision.1),prob=c(0.025,0.975))
mean(logistic(csp.x$Circumcision.2-csp.x$Circumcision.1)); quantile(logistic(csp.x$Circumcision.2-csp.x$Circumcision.1),prob=c(0.025,0.975))

mean(exp(csp.x$Marriage)); quantile(exp(csp.x$Marriage),prob=c(0.025,0.975))
csp.x$single <- logistic(csp.x$a)
csp.x$married <-logistic(csp.x$Marriage+csp.x$a)
marriage.diff <-csp.x$married-csp.x$single 
mean(marriage.diff); quantile(marriage.diff, prob=c(0.025,0.975))

mean(exp(csp.x$Sexual_debut.2-csp.x$Sexual_debut.1)); quantile(exp(csp.x$Sexual_debut.2-csp.x$Sexual_debut.1),prob=c(0.025,0.975))
mean(exp(csp.x$Sexual_debut.3-csp.x$Sexual_debut.1)); quantile(exp(csp.x$Sexual_debut.3-csp.x$Sexual_debut.1),prob=c(0.025,0.975))
mean(logistic(csp.x$Sexual_debut.2-csp.x$Sexual_debut.1)); quantile(logistic(csp.x$Sexual_debut.2-csp.x$Sexual_debut.1),prob=c(0.025,0.975))
mean(logistic(csp.x$Sexual_debut.3-csp.x$Sexual_debut.1)); quantile(logistic(csp.x$Sexual_debut.3-csp.x$Sexual_debut.1),prob=c(0.025,0.975))

mean(exp(csp.x$Child_desire.2-csp.x$Child_desire.1)); quantile(exp(csp.x$Child_desire.2-csp.x$Child_desire.1),prob=c(0.025,0.975))
mean(logistic(csp.x$Child_desire.2-csp.x$Child_desire.1)); quantile(logistic(csp.x$Child_desire.2-csp.x$Child_desire.1),prob=c(0.025,0.975))

mean(exp(csp.x$Paid_sex.2-csp.x$Paid_sex.1)); quantile(exp(csp.x$Paid_sex.2-csp.x$Paid_sex.1),prob=c(0.025,0.975))
mean(logistic(csp.x$Paid_sex.2-csp.x$Paid_sex.1)); quantile(logistic(csp.x$Paid_sex.2-csp.x$Paid_sex.1),prob=c(0.025,0.975))

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
title(main="A",line=-1,adj=0.3)
plot(coeftab(sm.model2,sm.model1,sm.model3,sm.model4),pars=cov.par)
title(main="B",line=-1,adj=0.3)

dev.off()
par(mfrow=c(1,2),mai=c(0.9,0,0.5,0.1))
plot(compare(csp.model2,csp.model1,csp.model3,csp.model4))
title(main="A",line=-1,adj=0.3)
plot(coeftab(csp.model2,csp.model1,csp.model3,csp.model4),pars=cov.par)
title(main="B",line=-1,adj=0.3)

#counterfactual plot (marginal posterior predictions) of each predictor of sm (Figure 1)
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
#posterior predictive plot
sm.ppp.mu <- link(sm.model4)
sm.ppp.mu.mean <- apply(sm.ppp.mu,2,mean)
sm.ppp.mu.PI <- apply(sm.ppp.mu,2,PI)
sm.ppp.sim <- sim(sm.model4, n=10)
sm.ppp.CI <- apply(sm.ppp.sim,2,PI)

dev.off()
plot(sm.ppp.mu.mean ~ sm , col=rangi2, ylim=range(sm.ppp.mu.PI),xlab="observed SM",ylab="predicted SM", data=na.omit(male.sm))
abline(a=0,b=1,lty=2)
for(i in 1:nrow(na.omit(male.sm)))
    lines(rep(na.omit(male.sm$sm[i]),2), c(sm.ppp.mu.PI[1,i],sm.ppp.mu.PI[2,i]), col=rangi2)

#variance across intercept
dev.off()
par(mfrow=c(1,2))
dens(sm.x$s_houseno, xlab="variance", main="A")
dens(sm.x$s_clustno, col=rangi2, lwd=2, add=TRUE)
text(2,0.85,"EA", col=rangi2)
text(0.75, 2,"HH")

dens(csp.x$s_houseno, xlab="variance", main="B")
dens(csp.x$s_clustno, col=rangi2, lwd=2, add=TRUE)
text(2,0.85,"EA", col=rangi2)
text(0.75, 2,"HH")





    