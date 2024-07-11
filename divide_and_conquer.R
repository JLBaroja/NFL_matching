rm(list=ls())
library('R2jags')
library('rstan')
library('parallelMCMCcombine')
set.seed(1)

use_jags <- T
use_stan <- !use_jags

# Simulate data
n_by_batch <- 100
th <- seq(0.1,0.9,0.1)
full <- NULL
for(i in 1:length(th)){
	full <- append(full,
						rbinom(n=n_by_batch,prob=th[i],size=1))
}

#full <- rbinom(n=900,prob=.35,size=1)
n_obs <- length(full)

alpha_prior <- 100
beta_prior <- 50

# Closed-form solution (full data)
sup_th <- seq(0,1,0.001)
closed_prior <- dbeta(sup_th,shape1=alpha_prior,shape2=beta_prior)
closed_post <- dbeta(sup_th,shape1=alpha_prior+sum(full==1),
										 shape2=beta_prior+sum(full==0))
beta_root <- function(th,a,b,k){
	# Calculates the Kth root of a Beta(a,b) distribution
	dens <- ((th^(a-1)*(1-th)^(b-1))/beta(a,b))^(1/k)
	return(dens)
}
# Numeric approximation FULL
if(use_jags){
	observed <- list('full','n_obs','alpha_prior','beta_prior')
	unobserved <- c('theta','theta_prior')
	write('
				model{
					theta ~ dbeta(alpha_prior,beta_prior)
					theta_prior ~ dbeta(alpha_prior,beta_prior)
					#theta_aux ~ dbeta(alpha_prior,beta_prior)
					#theta_prior <- pow(theta_aux,1/3)
					for(i in 1:n_obs){
						full[i] ~ dbern(theta)
					}
				}
				','full.bug')
	bayes <- jags(data=observed,
		parameters.to.save=unobserved,
		model.file='full.bug',
		n.chains=3,n.iter=1000,n.burnin=0,n.thin=1)
	unlink('full.bug')
	nds <- bayes$BUGSoutput$sims.list
}
if(use_stan){
	observed <- list(x=sum(full),n=length(full),a=alpha_prior,b=beta_prior)
	stan_model_code <-'
	data{
					int<lower=0> x;
					int<lower=0> n;
					int<lower=0> a;
					int<lower=0> b;
	}
	
	parameters{
					real<lower=0,upper=1> theta_prior;
					real<lower=0,upper=1> theta;
	}
	
	model{
					theta_prior ~ beta(a,b);
					theta ~ beta(a,b);
					x ~ binomial(n,theta);
	}
	'
	stan_model <- stan_model(model_code=stan_model_code)
	fit <- sampling(stan_model,data=observed,iter=1000,chains=3,seed=123)
	nds <- extract(fit)
}

# Divide and Conquer
if(use_stan){
	infer_batch <- function(obs){
alpha_prior <- 15
beta_prior <- 20
root <- 3
observed <- list(x=sum(obs),n=length(obs),k=root,a_prior=alpha_prior,b_prior=beta_prior)
stan_model_code <-'
functions{
				real beta_root_lpdf(real th, real a, real b,real k){
					return beta_lpdf(th|a,b)/k;
				}
}

data{
				int<lower=1> k;
				int<lower=0> x;
				int<lower=0> n;
				int<lower=0> a_prior;
				int<lower=0> b_prior;
}

parameters{
				//real<lower=0,upper=1> theta_prior;
				real<lower=0,upper=1> theta;
}

model{
				//theta_prior ~ beta_root(a_prior,b_prior,k);
				theta ~ beta_root(a_prior,b_prior,k);
				x ~ binomial(n,theta);
}
'
stan_model <- stan_model(model_code=stan_model_code)
fit <- sampling(stan_model,data=observed,iter=50000,chains=4,seed=123)
nds <- extract(fit)
return(nds)
	} # End of infer_batch()


}

if(use_jags){
infer_batch <- function(obs){
	n_obs <- length(obs)
	observed <- list('obs','n_obs')
	unobserved <- c('theta')
	write('
				model{
					theta ~ dbeta(1,1) # Uniform prior per batch
					for(i in 1:n_obs){
						obs[i] ~ dbern(theta)
					}
				}
				','batch.bug')
	bayes <- jags(data=observed,
		parameters.to.save=unobserved,
		model.file='batch.bug',
		n.chains=3,n.iter=1000,n.burnin=0,n.thin=1)
	unlink('batch.bug')
	nds <- bayes$BUGSoutput$sims.list
	return(list(bayes=bayes,nds=nds))
}

sample_prior <- function(){
	dummy <- 1
	observed <- list('dummy','alpha_prior','beta_prior')
	unobserved <- c('theta_prior')
	write('
				model{
					theta_prior ~ dbeta(alpha_prior,beta_prior)
				# Dummy sampling just to the JAGS running
					gamma ~ dbeta(1,1)
					dummy ~ dbern(gamma)
				}
				','batch.bug')
	bayes <- jags(data=observed,
		parameters.to.save=unobserved,
		model.file='batch.bug',
		n.chains=3,n.iter=1000,n.burnin=0,n.thin=1)
	unlink('batch.bug')
	nds <- bayes$BUGSoutput$sims.list
	return(list(bayes=bayes,nds=nds))
}


n_batches <- 3
#n_batches <- 90
shards <- vector(mode='list',length=n_batches+1)
for(b in 1:n_batches){
	#shards[[b]]$indices <- seq(1,10)+10*(b-1)
	#shards[[b]]$indices <- seq(1,300)+300*(b-1)
	shards[[b]]$indices <- seq(1,length(full)/n_batches)+length(full)/n_batches*(b-1)
	shards[[b]]$data <- full[shards[[b]]$indices]
	shards[[b]]$closed <- dbeta(sup_th,1+sum(shards[[b]]$data==1),1+sum(shards[[b]]$data==0))
	shards[[b]]$mcmc$theta <- infer_batch(shards[[b]]$data)$nds$theta
	shards[[b]]$density <- density(shards[[b]]$mcmc$theta,from=0,to=1)
}
shards[[b+1]]$mcmc$theta <- sample_prior()$nds$theta_prior	
shards[[b+1]]$density <- density(shards[[b+1]]$mcmc$theta,from=0,to=1)
shards[[b+1]]$closed <- dbeta(sup_th,alpha_prior,beta_prior)

#subarray <- array(dim=c(1,length(shards[[b]]$mcmc$theta),length(shards)))
subarray <- array(dim=c(1,length(shards[[b]]$mcmc$theta),length(shards)-1))
posterior <- rep(1,length(shards[[1]]$density$x))
for(sh in 1:(length(shards)-1)){
#for(sh in 1:(length(shards))){ # Already including prior in this counter
	posterior <- posterior * shards[[sh]]$density$y
	subarray[1,,sh] <- shards[[sh]]$mcmc$theta
}

#posterior <- consensusMCindep(subarray)
#posterior <- consensusMCcov(subarray)
posterior <- semiparamDPE(subarray)

} # End of use_jags

# Plotting
try(dev.off())
x11(width=16,height=12)
layout(matrix(1:2,nrow=2))
#plot(sup_th,((sup_th^(alpha_prior-1)*(1-sup_th)^(beta_prior-1))/beta(alpha_prior,beta_prior))^(1/3),
#	pch=21,bg='#ffffff',lwd=2,cex=1.5)
#lines(sup_th,closed_prior^(1/3),type='l',lwd=3,lty='dashed',col='red')

# Full data
hist(nds$theta,breaks=seq(0,1,length.out=131),freq=F,ylim=c(0,55))
hist(nds$theta_prior,breaks=seq(0,1,length.out=130),freq=F,col='#2189ee44',add=T)
lines(sup_th,closed_post,type='l',lwd=3)
lines(sup_th,closed_prior,type='l',lwd=2,lty='dashed')

# Divided data
plot(NULL,xlim=c(0,1),ylim=c(0,55))
for(sh in 1:(length(shards)-1)){
	lines(density(shards[[sh]]$mcmc$theta,from=0,to=1))
	lines(sup_th,shards[[sh]]$closed,lty='dotted')	
}
# Prior
lines(density(shards[[sh+1]]$mcmc$theta,from=0,to=1),lwd=3)
lines(sup_th,shards[[sh+1]]$closed,lwd=3,lty='dotted')
lines(sup_th,beta_root(sup_th,alpha_prior,beta_prior,k=n_batches),col='blue',lwd=3)
#lines(shards[[sh+1]]$density$x,posterior,lwd=3,col='red')
hist(posterior,freq=F,breaks=20,col='red',add=T)
