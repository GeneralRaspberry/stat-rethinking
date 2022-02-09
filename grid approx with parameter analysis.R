p_grid<-seq(from=0,to=1,length.out=1000)
prob_p<-rep(1,1000)
prob_data<-dbinom(6,size=9,prob=p_grid)
posterior<-prob_data*prob_p
posterior<-posterior/sum(posterior)

############scoop out o0f the bucket of samples############################
samples<-sample(p_grid,prob=posterior,size=1e4,replace=TRUE)

plot(samples)

##density estimate
library(rethinking)
dens(samples)

###estimate boundary interval

sum(posterior[p_grid<0.5])

sum(samples<0.5)/1e4 ####different answer

sum(samples > 0.5 & samples <0.75)/1e4 ####interval between two boundaries

quantile(samples,0.8) #####compatibility interval, range of parameter values compatible with the model and data

quantile(samples, c(.1,.9))###midel interval

#####point interval not very useful, DISCARDS INFORMATION


p_grid<-seq(from=0,to=1,length.out=1000)

prior<-rep(1,1000)

likelihood<-dbinom(3,size=3,prob=p_grid)

posterior<-likelihood*prior
posterior<-posterior/sum(posterior)
samples<-sample(p_grid,size=1e4,replace=TRUE,prob=posterior)




p_grid[which.max(posterior)]

chainmode(samples,adj=.01)

sum(posterior*abs(.5-p_grid)) ###weighted loss

loss<-sapply(p_grid,function(d)sum(posterior*abs(d-p_grid)))

p_grid[which.min(loss)]

dbinom(0:2,size=2,prob=.7)


rbinom(1 ,size=2,prob=.7) ###sample from a binom distribution

dummy_w<-rbinom(1e5,size=2,prob=.7) #sample 10000 times
table(dummy_w)/1e5 #proportional values retrieved


dummy_w<-rbinom(1e5,size=9,prob=.1) #experiment with number of tosses
freqtable<-table(dummy_w)/1e5 

plot(freqtable)

w<-rbinom(1e4,size=9,prob=samples) ##########this is called the posterior predictive distribution, once the size has been vectorised