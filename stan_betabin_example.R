rm(list=ls())
library('rstan')
full <- rbinom(n=10,size=1,prob=0.3)
alpha_prior <- 15
beta_prior <- 20
root <- 3
observed <- list(x=sum(full),n=length(full),k=root,a_prior=alpha_prior,b_prior=beta_prior)
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
				real<lower=0,upper=1> theta_prior;
				real<lower=0,upper=1> theta;
}

model{
				theta_prior ~ beta_root(a_prior,b_prior,k);
				theta ~ beta_root(a_prior,b_prior,k);
				x ~ binomial(n,theta);
}
'
stan_model <- stan_model(model_code=stan_model_code)
fit <- sampling(stan_model,data=observed,iter=50000,chains=4,seed=123)
nds <- extract(fit)


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

try(dev.off())
x11(width=5,height=5)
brks <- seq(0,1,length.out=80)
hist(nds$theta,xlim=c(0,1),breaks=brks,freq=F,col='#aaaaaa')
hist(nds$theta_prior,add=T,breaks=brks,freq=F,col='#14c7d388')
lines(sup_th,closed_post,lwd=2)
lines(sup_th,closed_prior,lwd=2,lty='dashed')
lines(sup_th,beta_root(sup_th,alpha_prior,beta_prior,root),lwd=2,col='red')

