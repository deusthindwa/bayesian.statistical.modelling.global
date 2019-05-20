#written by Deus Thindwa
#11/04/2019

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

#====================recode outcome and potential covariates====================
#integerize the cluster and household ids
male.DSF$clustno <- as.integer(male.DSF$clustno)
male.DSF$houseno <- as.integer(male.DSF$houseno)

# MCSP (multiple and concurrent sexual partnership) = SM (serial monogamy) + CSP (concurrent sexual partnership)
male.DSF <-subset(male.DS,mcsp>=1)
male.DSF$mcsp <-if_else(male.DSF$mcsp==1,0,1)

male.DSF$sm <-if_else(is.na(male.DSF$csp),0,
                      if_else(male.DSF$csp=="no",1,NULL))
male.DSF %>% tabyl(sm, show_na=FALSE) %>% adorn_pct_formatting(digits=1)

male.DSF$csp <-if_else(is.na(male.DSF$csp),0,
                       if_else(male.DSF$csp=="yes",1,NULL))
male.DSF %>% tabyl(csp, show_na=FALSE) %>% adorn_pct_formatting(digits=1)

#Respondent male age
male.DSF$agegp <-if_else(male.DSF$age>=15 & male.DSF$age<20,0,
                         if_else(male.DSF$age>=20 & male.DSF$age<25,1,
                                 if_else(male.DSF$age>=25 & male.DSF$age<30,2,
                                         if_else(male.DSF$age>=30 & male.DSF$age<35,3,4))))
male.DSF$agegp <-recode_factor(male.DSF$agegp,`0`="15-19",`1`="20-24",`2`="25-29",`3`="30-34",`4`="35+")

#Residence
male.DSF$resid <-recode_factor(male.DSF$resid,`rural`=0,`urban`=1)
male.DSF$resid <-recode_factor(male.DSF$resid,`0`="rural",`1`="urban")

#Education
male.DSF$educ <-recode_factor(male.DSF$educ,`no education`="0",`primary`="1",`secondary`="2",`higher`="3")
male.DSF$educ <-recode_factor(male.DSF$educ,`0`="no education",`1`="primary",`2`="secondary",`3`="higher")

#Employment
male.DSF$employ <-recode_factor(male.DSF$employ,`no`="0",`in the past year`="0",`currently working`="1")
male.DSF$employ <-recode_factor(male.DSF$employ,`0`="none",`1`="working")

#Religion
male.DSF$rel <-recode_factor(male.DSF$rel,`no religion`="0",`other`="0",`anglican`="1",`catholic`="1",`ccap`="1",`other christian`="1",`seventh day adventist / baptist`="1",`muslim`="2")
male.DSF$rel <-recode_factor(male.DSF$rel,`0`="none",`1`="christianity",`2`="islam")

#Wealth index
male.DSF$windex <-recode_factor(male.DSF$windex,`poorer`="0",`poorest`="0",`middle`="1",`richer`="2",`richest`="2")
male.DSF$windex <-recode_factor(male.DSF$windex,`0`="poor",`1`="middle",`2`="rich")

#Travel away from home
male.DSF$travel <-if_else(male.DSF$travel==0,0,
                         if_else(male.DSF$travel>0 & male.DSF$travel<=6,1,2))
male.DSF$travel <-recode_factor(male.DSF$travel,`0`="none",`1`="less times",`2`="more times")

#Partner's current pregnancy status
male.DSF$partpreg <-recode_factor(male.DSF$partpreg,`no`="0",`yes`="1",`unsure`="2")
male.DSF$partpreg <-recode_factor(male.DSF$partpreg,`0`="no",`1`="yes",`2`="unsure")

#Condom used during last sex
male.DSF$condom <-recode_factor(male.DSF$condom,`no`="0",`yes`="1")
male.DSF$condom <-recode_factor(male.DSF$condom,`0`="no",`1`="yes")

#Male circumcision
male.DSF$mmc <-recode_factor(male.DSF$mmc,`no`="0",`don't know`="0",`yes`="1")
male.DSF$mmc <-recode_factor(male.DSF$mmc,`0`="no",`1`="yes")

#Marital status
male.DSF$mstatus <-recode_factor(male.DSF$mstatus,`never in union`="0",`living with partner`="1",`married`="1",`divorced`="2",`no longer living together/separated`="2",`widowed`="2")
male.DSF$mstatus <-recode_factor(male.DSF$mstatus,`0`="never married",`1`="married",`2`="seperated")

#Age at first sex (marriage age in Malawi used to be 15+ but now 18+)
male.DSF$agesexgp <-if_else(male.DSF$agesex<15,0,
                         if_else(male.DSF$agesex>=15 & male.DSF$agesex<19,1,
                                 if_else(male.DSF$agesex>=19 & male.DSF$agesex<25,2,3)))
male.DSF$agesexgp <-recode_factor(male.DSF$agesexgp,`0`="<15",`1`="15-18",`2`="19-24",`3`="25+")

#Fertility profile
male.DSF$fertprof <-recode_factor(male.DSF$fertprof,`declared infecund (respondent or partner(s))`="0",`sterilized (respondent or partner(s))`="0",
                                  `have another`="1",`no more`="1",`undecided`="1",`never had sex`="2",`man has no partner`="2")
male.DSF$fertprof <-recode_factor(male.DSF$fertprof,`0`="infertile",`1`="fertile",`2`=NULL)

#Fertility preference
male.DSF$fertpref <-recode_factor(male.DSF$fertpref,`wants no more`="0",`wants within 2 years`="1",`wants, unsure timing`="1",`wants after 2+ years`="2",`undecided`="2",
                                  `declared infecund (respondent or partner(s))`="3",`sterilized (respondent or partner(s))`="3",
                                  `never had sex`="3")
male.DSF$fertpref <-recode_factor(male.DSF$fertpref,`0`="doesnt want",`1`="within 2y",`2`="after 2y",`3`= NULL)

#Knowledge of HIV/AIDS
male.DSF$hivknw <-if_else(male.DSF$hiv_condoms=="no" & male.DSF$hiv_1part=="no",0,
                            if_else(male.DSF$hiv_condoms=="yes" & male.DSF$hiv_1part=="no",1,
                                    if_else(male.DSF$hiv_condoms=="no" & male.DSF$hiv_1part=="yes",1,
                                            if_else(male.DSF$hiv_condoms=="yes" & male.DSF$hiv_1part=="yes",2,NULL))))
male.DSF$hivknw <-recode_factor(male.DSF$hivknw,`0`="none",`1`="moderate",`2`="comprehensive")

#Sexually transmitted disease counsel from health worker
male.DSF$stdcounsel <-recode_factor(male.DSF$stdcounsel,`no`="0",`yes`="1")
male.DSF$stdcounsel <-recode_factor(male.DSF$stdcounsel,`0`="no",`1`="yes")

#Months ago to HIV test
male.DSF$hivtest <-if_else(male.DSF$hivtest>=0 & male.DSF$hivtest<7,0,
                          if_else(male.DSF$hivtest>=7 & male.DSF$hivtest<13,1,2))
male.DSF$hivtest <-recode_factor(male.DSF$hivtest,`0`="0-6mo",`1`="7-12mo",`2`=">12mo")

#Paid sex
male.DSF$paidsex <-recode_factor(male.DSF$paidsex,`no`="0",`yes`="1")
male.DSF$paidsex <-recode_factor(male.DSF$paidsex,`0`="no",`1`="yes")

#HIV result during DHS
male.DSF$hivres <-if_else(male.DSF$hivres=="hiv negative",0,
                             if_else(male.DSF$hivres=="hiv  positive",1,NULL))
male.DSF$hivres <-recode_factor(male.DSF$hivres,`0`="neg",`1`="pos")

#Wife justified to ask partner to use condom
male.DSF$sexinfl <-recode_factor(male.DSF$sexinfl,`no`="0",`yes`="1",`don't know`=NULL)
male.DSF$sexinfl <-recode_factor(male.DSF$sexinfl,`0`="no",`1`="yes")

#Recept partner's age
male.DSF$agepartgp <-if_else(male.DSF$agepart<18,0,
                         if_else(male.DSF$agepart>=18 & male.DSF$agepart<30,1,2))
male.DSF$agepartgp <-recode_factor(male.DSF$agepartgp,`0`="<18y",`1`="18-29y",`2`="30+")

#====================tabulating predictor by outcome (Table 1.0)====================
mean(male.DSF$age); sd(male.DSF$age)
ddply(male.DSF, .(mcsp==1), summarize, mean=mean(age),sd=sd(age))
ddply(male.DSF, .(is.na(sm)), summarize, mean=mean(age), sd=sd(age))
ddply(male.DSF, .(is.na(csp)), summarize, mean=mean(age), sd=sd(age))
male.DSF %>% tabyl(agegp,show_na=FALSE) %>% adorn_pct_formatting(digits=1)
male.DSF %>% tabyl(agegp,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(agegp,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$agegp,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$agegp,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(resid,show_na=FALSE) %>% adorn_pct_formatting(digits=1)
male.DSF %>% tabyl(resid,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(resid,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$resid,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$resid,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(educ,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(educ,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(educ,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$educ,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$educ,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(employ,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(employ,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(employ,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$employ,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$employ,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(rel,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(rel,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(rel,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$rel,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$rel,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(windex,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(windex,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(windex,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$windex,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$windex,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(travel,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(travel,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(travel,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$travel,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$travel,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(partpreg,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(partpreg,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(partpreg,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$partpreg,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$partpreg,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(condom,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(condom,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(condom,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$condom,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$condom,male.DSF$csp, correct=TRUE)

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

male.DSF %>% tabyl(fertprof,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(fertprof,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(fertprof,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$fertprof,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$fertprof,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(fertpref,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(fertpref,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(fertpref,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$fertpref,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$fertpref,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(hivknw,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(hivknw,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(hivknw,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$hivknw,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$hivknw,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(stdcounsel,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(stdcounsel,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(stdcounsel,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$stdcounsel,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$stdcounsel,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(hivtest,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(hivtest,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(hivtest,mcsp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$hivtest,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$hivtest,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(paidsex,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(paidsex,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(paidsex,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$paidsex,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$paidsex,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(hivres,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(hivres,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(hivres,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$hivres,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$hivres,male.DSF$csp, correct=TRUE)

male.DSF %>% tabyl(sexinfl,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(sexinfl,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(sexinfl,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$sexinfl,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$sexinfl,male.DSF$csp, correct=TRUE)

mean(male.DSF$agepart); sd(male.DSF$agepart)
ddply(male.DSF, .(mcsp==1), summarize, mean=mean(agepart),sd=sd(agepart))
ddply(male.DSF, .(is.na(sm)), summarize, mean=mean(agepart), sd=sd(agepart))
ddply(male.DSF, .(is.na(csp)), summarize, mean=mean(agepart), sd=sd(agepart))
male.DSF %>% tabyl(agepartgp,show_na=FALSE) %>% adorn_pct_formatting(digits=1) 
male.DSF %>% tabyl(agepartgp,sm,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
male.DSF %>% tabyl(agepartgp,csp,show_na=FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1) %>% adorn_ns()
chisq.test(male.DSF$agepartgp,male.DSF$sm, correct=TRUE); chisq.test(male.DSF$agepartgp,male.DSF$csp, correct=TRUE)

rm(male.dhs, male.DS)
male.DSF <-subset(male.DSF, select=c(clustno,houseno,sm,csp,agegp,resid,educ,employ,rel,windex,travel,partpreg,condom,mmc,
                                      mstatus,agesexgp,fertpref,paidsex,agepartgp))

#====================model selection procedure (Table 2.0)====================

#recode all predictors to represent numeric class for binomial model fitting
male.DSF$agegp <-as.numeric(recode_factor(male.DSF$agegp,`15-19`=0,`20-24`=1,`25-29`=2,`30-34`=3,`35+`=4))
male.DSF$resid <-as.numeric(recode_factor(male.DSF$resid,`rural`=0,`urban`=1))
male.DSF$educ <-as.numeric(recode_factor(male.DSF$educ,`no education`=0,`primary`=1,`secondary`=2,`higher`=3))
male.DSF$employ <-as.numeric(recode_factor(male.DSF$employ,`none`=0,`working`=1))
male.DSF$rel <-as.numeric(recode_factor(male.DSF$rel,`none`=0,`christianity`=1,`islam`=2))
male.DSF$windex <-as.numeric(recode_factor(male.DSF$windex,`poor`=0,`middle`=1,`rich`=2))
male.DSF$travel <-as.numeric(recode_factor(male.DSF$travel,`none`=0,`less times`=1,`more times`=2))
male.DSF$partpreg <-as.numeric(recode_factor(male.DSF$partpreg,`no`=0,`yes`=1,`unsure`=2))
male.DSF$condom <-as.numeric(recode_factor(male.DSF$condom,`no`=0,`yes`=1))
male.DSF$mmc <-as.numeric(recode_factor(male.DSF$mmc,`no`=0,`yes`=1))
male.DSF$mstatus <-as.numeric(recode_factor(male.DSF$mstatus,`never married`=0,`married`=1,`seperated`=2))
male.DSF$agesexgp <-as.numeric(recode_factor(male.DSF$agesexgp,`<15`=0,`15-18`=1,`19-24`=2,`25+`=3))
male.DSF$fertpref <-as.numeric(recode_factor(male.DSF$fertpref,`doesnt want`=0,`within 2y`=1,`after 2y`=2))
male.DSF$paidsex <-as.numeric(recode_factor(male.DSF$paidsex,`no`=0,`yes`=1))
male.DSF$agepartgp <-as.numeric(recode_factor(male.DSF$agepartgp,`<18y`=0,`18-29y`=1,`30+`=2))

#fit 4 potential binomial models and sample from posterior distribution using Halmitonian Monte Carlo
#model1: without random-effects
m1.pd_sm <-map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+b_agegp*agegp+b_resid*resid+b_educ*educ+b_employ*employ,
    c(b_agegp,b_resid,b_educ,b_employ) ~ dnorm(0,1),
    a ~ dnorm(0,1)
  ),
  data=as.data.frame(na.omit(male.DSF)),chains=4,iter=4000,warmup=1000)

m1.pd_sm <-map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+b_agegp[agegp]+b_resid[resid]+b_educ[educ]+b_employ[employ],
    b_agegp[agegp] ~ dnorm(0,1),
    b_resid[resid] ~ dnorm(0,1),
    b_educ[educ] ~ dnorm(0,1),
    b_employ[employ] ~ dnorm(0,1),
    a ~ dnorm(0,1)
  ),
  data=as.data.frame(na.omit(male.DSF)),chains=2,iter=2500,warmup=500,cores=4)


#model2: with household random-effects variable
m2.pd_sm <-map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_houseno[houseno]+b_agegp*agegp+b_resid*resid+b_educ*educ+b_employ*employ,
    c(b_agegp,b_resid,b_educ,b_employ) ~ dnorm(0,1),
    a_houseno[houseno] ~ dnorm(0,sigma_houseno),
    a ~ dnorm(0,1),
    sigma_houseno ~ dcauchy(0,1)
  ),
  data=as.data.frame(na.omit(male.DSF)),chains=2,iter=2500,warmup=500 )

m2.pd_sm <-map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_houseno[houseno]+b_agegp[agegp]+b_resid[resid]+b_educ[educ]+b_employ[employ],
    b_agegp[agegp] ~ dnorm(0,1),
    b_resid[resid] ~ dnorm(0,1),
    b_educ[educ] ~ dnorm(0,1),
    b_employ[employ] ~ dnorm(0,1),
    a_houseno[houseno] ~ dnorm(0,sigma_houseno),
    a ~ dnorm(0,1),
    sigma_houseno ~ dcauchy(0,1)
  ),
  data=as.data.frame(na.omit(male.DSF)),chains=2,iter=2500,warmup=500,cores=4)


#model3: with cluster/community random-effects variable
m3.pd_sm <-map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_clustno[clustno]+b_agegp*agegp+b_resid*resid+b_educ*educ+b_employ*employ,
    c(b_agegp,b_resid,b_educ,b_employ) ~ dnorm(0,1),
    a_clustno[clustno] ~ dnorm(0,sigma_clustno),
    a ~ dnorm(0,1),
    sigma_clustno ~ dcauchy(0,1)
  ),
  data=as.data.frame(na.omit(male.DSF)),chains=2,iter=2500,warmup=500)

m3.pd_sm <-map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_clustno[clustno]+b_agegp[agegp]+b_resid[resid]+b_educ[educ]+b_employ[employ],
    b_agegp[agegp] ~ dnorm(0,1),
    b_resid[resid] ~ dnorm(0,1),
    b_educ[educ] ~ dnorm(0,1),
    b_employ[employ] ~ dnorm(0,1),
    a_clustno[clustno] ~ dnorm(0,sigma_clustno),
    a ~ dnorm(0,1),
    sigma_clustno ~ dcauchy(0,1)
  ),
  data=as.data.frame(na.omit(male.DSF)),chains=2,iter=2500,warmup=500,cores=4)

#model4: with household and cluster/community random-effects variables
m4.pd_sm <-map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_clustno[clustno]+a_houseno[houseno]+b_agegp*agegp+b_resid*resid+b_educ*educ+b_employ*employ,
    c(b_agegp,b_resid,b_educ,b_employ) ~ dnorm(0,1),
    a_clustno[clustno] ~ dnorm(0,sigma_clustno),
    a_houseno[houseno] ~ dnorm(0,sigma_houseno),
    a ~ dnorm(0,1),
    sigma_clustno ~ dcauchy(0,1),
    sigma_houseno ~ dcauchy(0,1)
  ),
  data=as.data.frame(na.omit(male.DSF)),chains=2,iter=2500,warmup=500)

m3.pd_sm <-map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+a_houseno[houseno]+a_clustno[clustno]+b_agegp[agegp]+b_resid[resid]+b_educ[educ]+b_employ[employ],
    b_agegp[agegp] ~ dnorm(0,1),
    b_resid[resid] ~ dnorm(0,1),
    b_educ[educ] ~ dnorm(0,1),
    b_employ[employ] ~ dnorm(0,1),
    a_houseno[houseno] ~ dnorm(0,sigma_houseno),
    a_clustno[clustno] ~ dnorm(0,sigma_houseno),
    a ~ dnorm(0,1),
    sigma_houseno ~ dcauchy(0,1),
    sigma_clustno ~ dcauchy(0,1)
    
  ),
  data=as.data.frame(na.omit(male.DSF)),chains=2,iter=2500,warmup=500 )

#compare model predictions through WAIC
compare(m1.pd_sm,m2.pd_sm,m3.pd_sm,m4.pd_sm)

#trace plot to see if the chains are health
plot(m1.pd_sm)
plot(m2.pd_sm)
plot(m3.pd_sm)
plot(m4.pd_sm)

#investigate correlation between predictors
pairs(m1.pd_sm)
pairs(m2.pd_sm)
pairs(m3.pd_sm)
pairs(m4.pd_sm)

#estimate posterior mean and 95% credible intervals using maximum a posteriori
precis(m1.pd_sm, depth=2)
precis(m2.pd_sm, depth=2)
precis(m3.pd_sm, depth=2)
precis(m4.pd_sm, depth=2)

posterior1 <- extract.samples(m1.pd_sm)
p.age15_19 <- logistic(posterior1$a)
p.age20_24 <- logistic(posterior1$a+posterior1$b_agegp)
diff.age <- p.age20_24-p.age15_19
quantile(diff.age, c(0.025,0.5,0.975))


pd_sm <-map2stan(
  alist(
    sm ~ dbinom(1,sm_p),
    logit(sm_p) <- a+b_agegp*agegp+b_resid*resid+b_educ*educ+b_employ*employ+b_rel*rel+b_windex*windex+b_travel*travel+b_partpreg*partpreg+
      b_condom*condom+b_mmc*mmc+b_mstatus*mstatus+b_agesexgp*agesexgp+b_fertpref*fertpref+b_paidsex*paidsex+b_agepartgp*agepartgp,
    c(a,b_agegp,b_resid,b_educ,b_employ,b_rel,b_windex,b_travel,b_partpreg,b_condom,b_mmc,b_mstatus,b_agesexgp,b_fertpref,b_paidsex,b_agepartgp) ~ dnorm(0,10)),
  data=as.data.frame(na.omit(male.DSF)),chains=2,iter=2500,warmup=500 )











