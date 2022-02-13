#######problems?########################
rm(list=ls())
#####easy#################

#1

p_grid<-seq(from=0,to=1,length.out=1000)
prior<-rep(1,1000)
likelihood<-dbinom(6,size=9,prob = p_grid) #remember probability of probability, don't be fooled.
posterior<-likelihood*prior
posterior<-posterior/sum(posterior)

set.seed(100)
samples<-sample(p_grid,prob=posterior,size=1e4,replace=TRUE)

sum(posterior[p_grid<0.2])

sum(posterior[p_grid>0.8])

sum(posterior[p_grid>0.2 & p_grid < .8]) ####This stuff is for posterior

quantile(samples,.2) #####this stuff is for parameters

quantile(samples,.8) ####above

library("rethinking")

HPDI(samples,.66)

PI(samples,.66)

#M

p_grid<-seq(from=0,to=1,length.out=1000)
prior<-rep(1,1000)
likelihood<-dbinom(8, size=15,p=p_grid)
posterior<-likelihood*prior
posterior<-posterior/sum(posterior)
samples<-sample(p_grid,prob = posterior, size=1e4,replace=TRUE)
HPDI(samples,.9)

w<-rbinom(1e4,size=15,prob=samples)###samples is already averages over the posterior
####to compute the proprtion that match the data 
wpred<-sum(w==8)/1e4

p_grid<-seq(from=0,to=1,length.out=1000)
prior<-rep(1,1000)
likelihood<-dbinom(6, size=9, p=p_grid)
posterior<-likelihood*wpred
posterior<-posterior/sum(posterior)

plot(posterior)

p_grid<-seq(from=0,to=1,length.out=1000)
prior<-ifelse(p_grid<=.5,0,1)
likelihood<-dbinom(8,15,prob=p_grid)
posterior<-likelihood*prior
posterior<-posterior/sum(posterior)

plot(posterior)

samples<-sample(p_grid,1e4,prob=posterior,replace=TRUE)

w<-rbinom(1e4,15,prob=samples)
simplehist(w)

predpos<-sum(w==8)/1e4

#h

library(rethinking)
data(homeworkch3)

totalboys<-sum(birth1)+sum(birth2)
p_grid<-seq(from=0,to=1,length.out=1000)
prior<-rep(1,1000)
totalkids<-length(birth1)+length(birth2)
likelihood<-dbinom(totalboys,as.numeric(totalkids),prob=p_grid)
posterior<-likelihood*prior
posterior<-posterior/sum(posterior)

plot(posterior)

p_grid[which.max(posterior)]

#h2

samples<-sample(p_grid,1e4,prob=posterior, replace=TRUE)
HPDI(samples,prob=.97)
