library('rstan')

# Full model
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
stan_model_full_betabin <- stan_model(model_code=stan_model_code)
save(stan_model_full_betabin,file='toy_betabin_full_stan.RData')
	
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
stan_model_divided_betabin <- stan_model(model_code=stan_model_code)
save(stan_model_divided_betabin,file='toy_betabin_divided_stan.RData')
