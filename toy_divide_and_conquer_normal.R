rm(list=ls())
library('rstan')
rstan_options(auto_write = TRUE)
options(mc.cores = 1)
set.seed(1)

# Simulate data
n_by_batch <- 100
th <- seq(5,55,10)
full <- NULL
for(i in 1:length(th)){
	full <- append(full,
						rnorm(n=n_by_batch,mean=th[i],sd=5))
}
n_obs <- length(full)

# Prior hyperparam
mean_prior <- 0
sd_prior <- 1
expo_prior <- 1

# Numeric approximation FULL
observed <- list(x=full,N=length(full),m_prior=mean_prior,sd_prior=sd_prior,expo_prior=expo_prior)
load('toy_normal_full_stan.RData')
fit <- sampling(stan_model_full_normal,data=observed,iter=1000,chains=3,seed=123)
nds <- extract(fit)

# Divide and Conquer
n_batches <- 2

infer_batch <- function(obs,stan_model){
	root <- n_batches
	observed <- list(x=obs,N=length(obs),m_prior=mean_prior,sd_prior=sd_prior,expo_prior=expo_prior,k=root)
	#observed <- list(x=sum(obs),n=length(obs),k=root,a_prior=alpha_prior,b_prior=beta_prior)
	fit <- sampling(stan_model,data=observed,iter=1000,chains=3,seed=123)
	nds <- extract(fit)
	return(nds)	
} # End of infer_batch()

load('toy_normal_divided_stan.RData')
shards <- vector(mode='list',length=n_batches)
for(b in 1:n_batches){
	shards[[b]]$indices <- seq(1,length(full)/n_batches)+length(full)/n_batches*(b-1)
	shards[[b]]$data <- full[shards[[b]]$indices]
	shards[[b]]$mcmc <- infer_batch(shards[[b]]$data,stan_model=stan_model_divided_normal)
	shards[[b]]$density <- density(shards[[b]]$mcmc$mu,from=0,to=1)
}

source('toy_plotting_normal.R')
