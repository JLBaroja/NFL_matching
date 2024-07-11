rm(list=ls())
library('rstan')
set.seed(1)


# Simulate data
n_by_batch <- 100
th <- seq(0.05,0.95,0.1)
full <- NULL
for(i in 1:length(th)){
	full <- append(full,
						rbinom(n=n_by_batch,prob=th[i],size=1))
}
#full <- rbinom(n=900,prob=.35,size=1)
n_obs <- length(full)

# Prior hyperparam
alpha_prior <- 15
beta_prior <- 20

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


# Divide and Conquer
n_batches <- 10

infer_batch <- function(obs){
#	alpha_prior <- 15
#	beta_prior <- 20
	root <- n_batches
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
	fit <- sampling(stan_model,data=observed,iter=1000,chains=3,seed=123)
	nds <- extract(fit)
	return(nds)	
} # End of infer_batch()


shards <- vector(mode='list',length=n_batches)
for(b in 1:n_batches){
	#shards[[b]]$indices <- seq(1,10)+10*(b-1)
	#shards[[b]]$indices <- seq(1,300)+300*(b-1)
	shards[[b]]$indices <- seq(1,length(full)/n_batches)+length(full)/n_batches*(b-1)
	shards[[b]]$data <- full[shards[[b]]$indices]
	shards[[b]]$closed <- dbeta(sup_th,1+sum(shards[[b]]$data==1),1+sum(shards[[b]]$data==0))
	shards[[b]]$mcmc$theta <- infer_batch(shards[[b]]$data)$theta
	#shards[[b]]$mcmc$theta <- infer_batch(shards[[b]]$data)$nds$theta
	shards[[b]]$density <- density(shards[[b]]$mcmc$theta,from=0,to=1)
}

