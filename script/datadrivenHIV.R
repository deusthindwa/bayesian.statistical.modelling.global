
#Predictors of serial and concurrent heterosexual partnership risks among Malawian men: A Multilevel Bayesian analysis study.
#written by Deus Thindwa
#01/07/2019

#====================LOAD REQUIRED PACKAGES AND DATASET====================

DDHP.packages <-c("foreign","tidyverse","janitor","readstata13","rethinking","rstan","plyr","DataCombine", "pROC")
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

#model4: a model with household and enumeration area random-effects variables
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

#==============FIT MODELS FOR CONCURRENT SEXUAL PARTNERSHIP AND SAMPLE FROM POSTERIOR DISTRIBUTION USING HALMITONIAN MCMC==================

#model1: a model without random-effects variables
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

#model2: a model with household as random-effects variable
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

#model3: a model with enumeration area as random-effects variable
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

#model4: a model with household and enumeration area random-effects variables
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

#==============POSTERIOR DISTRIBUTION ASSESSMENT THROUGH TRACEPLOTS, PAIRSPLOTS==================

##iterate for all models fitted above from sm.model1p through csp.model4p
posterior.cov = c("age[1]","age[2]","age[3]","education[1]","education[2]","education[3]")
dev.off()
plot(sm.model4p) 
pairs(sm.model4p, pars=posterior.cov) 
summary(sm.model4p) 
divergent(sm.model4p) 
dashboard(sm.model4p)

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
par(mfrow=c(1,4),mai=c(0.6,0,0,0.1),cex.axis=1.5,cex.lab=1.5)
plot(compare(sm.model1p,sm.model2p,sm.model3p,sm.model4p),col="blue3")
title(main="A",line=-1,adj=0.35)
plot(coeftab(sm.model2p,sm.model1p,sm.model3p,sm.model4p),pars=cov.par1,col="blue3")
title(main="B",line=-1,adj=0.5)
plot(coeftab(sm.model2p,sm.model1p,sm.model3p,sm.model4p),pars=cov.par2,col="blue3")
title(main="",line=-1,adj=0.5)
plot(coeftab(sm.model2p,sm.model1p,sm.model3p,sm.model4p),pars=cov.par3,col="blue3")
title(main="",line=-1,adj=0.5)

#model comparisons using WAIC for concurrent sexual partnership (supplementary figure 2)
dev.off()
cov.par1=c("age[1]","age[2]","age[3]","education[1]","education[2]","education[3]","employment[1]")
cov.par2=c("employment[2]","travel_times[1]","travel_times[2]","travel_times[3]","circumcision[1]","circumcision[2]","sexual_debut[1]")
cov.par3=c("sexual_debut[2]","sexual_debut[3]","child_desire[1]","child_desire[2]","child_desire[3]","paid_sex[1]","paid_sex[2]")
par(mfrow=c(1,4),mai=c(0.6,0,0,0.1),cex.axis=1.5,cex.lab=1.5)
plot(compare(csp.model1p,csp.model2p,csp.model3p,csp.model4p),col="red3")
title(main="A",line=-1,adj=0.35)
plot(coeftab(csp.model2p,csp.model1p,csp.model3p,csp.model4p),pars=cov.par1,col="red3")
title(main="B",line=-1,adj=0.5)
plot(coeftab(csp.model2p,csp.model1p,csp.model3p,csp.model4p),pars=cov.par2,col="red3")
title(main="",line=-1,adj=0.5)
plot(coeftab(csp.model2p,csp.model1p,csp.model3p,csp.model4p),pars=cov.par3,col="red3")
title(main="",line=-1,adj=0.5)

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
  mtext(par.values[[i]],side=1,line=2,cex=0.8); mtext("Probability",side=2,line=2,cex=0.8)
  };remove(i)

for(i in colnames(csp.extract[,1:21])){
  plot(logistic(csp.extract[[i]]), col="red1", xlab="",ylab="",main="",type="l",ylim=c(0,1))
  mtext(par.values[[i]],side=1,line=2,cex=0.8); mtext("Probability",side=2,line=2,cex=0.8)
};remove(i);remove(par.values)

#Posterior predictive and variance checks  (supplementary figure 5)
dev.off()
par(mfrow=c(2,2),mai = c(0.6, 0.8, 0.3, 0.1))

sm.link <- link(sm.model4p, data=na.omit(male.sm))
sm.mean <- apply(sm.link,2,mean)
sm.auc <- data.frame(sm.mean, male.sm$sm)
sm.roc.obj <- roc(sm.auc$male.sm.sm, sm.auc$sm.mean) #create a ROC curve
auc(sm.roc.obj) #calculates AUC
plot(1-sm.roc.obj$specificities,sm.roc.obj$sensitivities, col="blue3",xlab="",ylab="",type='l', lwd=2) #plots area under the ROC curve
abline(0,1, col="black", lwd=1, lty=2)
mtext("1-specificity",side=1,line=2,cex=0.9)
mtext("sensitivity",side=2,line=3,cex=0.9)
mtext("A",side=3,line=0,cex=1.3)
text(0.6, 0.2, labels=sprintf("AUC: %0.3f", auc(sm.roc.obj)), col="blue3")

csp.link <- link(csp.model4p, data=na.omit(male.csp))
csp.mean <- apply(csp.link,2,mean)
csp.auc <- data.frame(csp.mean, male.csp$csp)
csp.roc.obj <- roc(csp.auc$male.csp.csp, csp.auc$csp.mean) #create a ROC curve
auc(csp.roc.obj) #calculates AUC
plot(1-csp.roc.obj$specificities,csp.roc.obj$sensitivities, col="red3",xlab="",ylab="",type='l', lwd=2) #plots area under the ROC curve
abline(0,1, col="black", lwd=1, lty=2)
mtext("1-specificity",side=1,line=2,cex=0.9)
mtext("sensitivity",side=2,line=3,cex=0.9)
mtext("B",side=3,line=0,cex=1.3)
text(0.6, 0.2, labels=sprintf("AUC: %0.3f", auc(csp.roc.obj)), col="red3")

dens(sm.extract$s_clustno, xlab="", lwd=2, main="",ylim=c(0,6),xlim=c(0,1))
dens(sm.extract$s_houseno, col="blue3", lwd=2, add=TRUE)
text(0.70, 4.5,"Enumeration area")
text(0.1,6,"Household",col="blue3")
mtext("variance",side=1,line=2,cex=0.9)
mtext("C",side=3,line=0,cex=1.3)

dens(csp.extract$s_clustno, xlab="", lwd=2, main="",ylim=c(0,6),xlim=c(0,1))
dens(csp.extract$s_houseno, col="red3", lwd=2, add=TRUE)
text(0.6, 2.8,"Enumeration area")
text(0.1,4.4,"Household",col="red3")
mtext("variance",side=1,line=2,cex=0.9)
mtext("D",side=3,line=0,cex=1.3)

#End Script
#=========================================================================