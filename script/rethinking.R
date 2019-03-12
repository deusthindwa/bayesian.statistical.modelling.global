#author: Deus Thindwa
#first written date: 31 May 2018
#title: statistical rethinking using R

#install/help required packages
install.packages("rstan")
install.packages(c('devtools','coda','mvtnorm'))
install_github("rmcelreath/rethinking")
help(package="rethinking")

#load required packages
library(devtools)
library(rethinking)


#===============================================================
#chapter 2:smaller and bigger worlds (grid approximation of posterior distribution)
p <- seq(from=0, to=1, length.out = 1000)
priorx <- rep(1,1000)
likelihoodx <- dbinom(6, size = 9, prob = p)
posteriorx <- priorx*likelihoodx/(sum(priorx*likelihoodx))
samplex <- sample(p, prob = posteriorx, size=10000, replace = TRUE)
plot(samplex)


#===============================================================
#chapter 3:linear models (Intro, Quad approximation of posterior distribution)
data("Howell1")
d <-Howell1
flist1 <- alist(height~dnorm(mu,sigma), 
               mu <- dnorm(178,100), 
               sigma~dunif(0,50))
postdist1 <- map(flist1, data = d)
precis(postdist1)

flist2 <- alist(height~dnorm(mu,sigma), 
               mu <- a+b*weight, 
               a~dnorm(178,100), 
               b~dnorm(0,10), 
               sigma~dunif(0,50))
postdist2 <- map(flist2, data=d)
precis(postdist2)

#sampling from the posterior distribution
postsample <- extract.samples(postdist2)
postsample[1:5,]


#===============================================================
#chapter 4:further linear models
postsample <- extract.samples(postdist2)
mu.link <- function(weight) postsample$a+postsample$b*weight
weight.seq <- seq(from=10, to=60, by=1)
mu <- sapply(weight.seq, mu.link)
mu.mean <-apply(mu,2,mean)
mu.HPDI <- apply(mu,2,HPDI, prob=0.95)

#plot posterior mean height and credible interval
plot(height ~ weight, data=d, col=col.alpha(rangi2,0.5))
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)

#posterior predicted heights using SIM (not mean height)
weight.seq <- seq(from=10, to=60, by=1)
sim.height <- sim(postdist2, data=data.frame(weight=weight.seq))
shade(sim.height, weight.seq)

#SIM in detail
postsample <- extract.samples(postdist2)
weight.seq <- seq(from=10, to=60, by=1)
sim.height <- sapply(weight.seq, function(weight) rnorm(n=nrow(postsample), mean = postsample$a+postsample$b*weight, sd=postsample$sigma))
PI.height <- apply(sim.height, 2, PI)

#plot predicted interval of posterior mean height
shade(PI.height, weight.seq)

#===============================================================
#chapter 5: multivariate linear models
data(package="rethinking")
data("WaffleDivorce")
d2 <- WaffleDivorce
#fit Gaussian model and obtain a posterior distribution
wd.postdist <- alist(Divorce~dnorm(mu, sigma), 
<<<<<<< HEAD
                         mu <- a+bR*Marriage+bA*MedianAgeMarriage, 
                         a~dnorm(10,10), 
                         bR~dnorm(0,1), 
                         bA~dnorm(0,1), 
                         sigma~dunif(0,10) 
                  )
=======
                     mu <- a+bR*Marriage+bA*MedianAgeMarriage, 
                     a~dnorm(10,10), 
                     bR~dnorm(0,1), 
                     bA ~ dnorm(0,1), 
                     sigma~dunif(0,10))
>>>>>>> 986eb1cfe4f670c79dd4670b70a1ea060f44aaad
wd.postdist <- map(wd.postdist, data=d2)
precis(wd.postdist)
plot(precis(wd.postdist))

<<<<<<< HEAD
#plotting residuals
#(a) predictor on predictor
wd.postdistPP<-alist(Marriage~dnorm(mu, sigma), 
                         mu <- a + bA*MedianAgeMarriage, 
                         a ~ dnorm(10,10), 
                         bA ~ dnorm(0,1), 
                         sigma~dunif(0,10) 
                   )

wd.postdistPP<-map(wd.postdistPP, data=d2)
precis(wd.postdistPP)
plot(precis(wd.postdistPP))

#(b) compute residuals (distance of each outcome from expectation)
mu<-coef(wd.postdist)["a"]+coef(wd.postdist)["b"]*d2$MedianAgeMarriage
resid.outcome <- d2$Marriage-mu
plot(d2$MedianAgeMarriage,d2$Marriage)
lines()






=======
#1. investigate predictor residual plots
#(a) regress predictor against predictor
wd.postdistR <- alist(Marriage~dnorm(mu, sigma), 
                     mu <- a+bA*MedianAgeMarriage, 
                     a~dnorm(10,10), 
                     bA ~ dnorm(0,1), 
                     sigma~dunif(0,10))
wd.postdistR <- map(wd.postdistR, data=d2)
precis(wd.postdistR)
plot(precis(wd.postdistR))

wd.postdistA <- alist(MedianAgeMarriage~dnorm(mu, sigma), 
                       mu <- a+bR*Marriage, 
                       a~dnorm(10,10), 
                       bR ~ dnorm(0,1), 
                       sigma~dunif(0,10))
wd.postdistA <- map(wd.postdistA, data=d2)
precis(wd.postdistA)
plot(precis(wd.postdistA))

#(b) compute and plot residuals for each predictor against outcome
mu.resR <- coef(wd.postdistR)['a'] + coef(wd.postdistR)['bA']*d2$MedianAgeMarriage
m.resR <- d2$Marriage - mu.resR

mu.resA <- coef(wd.postdistA)['a'] + coef(wd.postdistA)['bR']*d2$Marriage
m.resA <- d2$MedianAgeMarriage - mu.resA

#(c) potential plots
par(mfrow=c(2,2))
plot(d2$MedianAgeMarriage, d2$Marriage)
plot(m.resR, d2$Divorce)
plot(d2$MedianAgeMarriage, d2$Marriage)
plot(m.resA, d2$Divorce)
dev.off()

#2. counterfactual plot

#3. posterior prediction plots
postcheck(wd.postdist)
dev.off()
>>>>>>> 986eb1cfe4f670c79dd4670b70a1ea060f44aaad


#===============================================================
#chapter 5: Causality terror in multivariate models
N <- 100
x.real <- rnorm(N)
x.spur <- rnorm(N, x.real)
y <- rnorm(N, x.real)
dSS <- data.frame(y, x.real, x.spur)





