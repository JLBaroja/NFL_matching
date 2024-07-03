rm(list=ls())
library('rstan')
k <- rbinom(n=10,size=1,prob=0.3)
observed <- list(x=sum(k),n=length(k))
stan_model_code <-'
data{
				int<lower=0> x;
				int<lower=0> n;
}

parameters{
				real<lower=0,upper=1> theta_prior;
				real<lower=0,upper=1> theta;
}

model{
				theta_prior ~ beta(1,1);
				theta ~ beta(1,1);
				x ~ binomial(n,theta);
}
'
stan_model <- stan_model(model_code=stan_model_code)
fit <- sampling(stan_model,data=observed,iter=2000,chains=4,seed=123)
print(fit)
