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