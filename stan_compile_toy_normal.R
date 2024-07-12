# Compiles full and divided normal models (toy examples) in STAN
library('rstan')

# Full model
stan_model_code <-'
data{
	int<lower=1> N;
	real x[N];
	real m_prior;
	real sd_prior;
  real expo_prior;
}

parameters{
	real mu;
	real sigma;
}

model{
	mu ~ normal(m_prior,sd_prior);
	sigma ~ exponential(expo_prior);
	x ~ normal(mu,sigma);
}
'
stan_model_full_normal <- stan_model(model_code=stan_model_code)
save(stan_model_full_normal,file='toy_normal_full_stan.RData')

# Divided model
stan_model_code <-'
functions{
				real normal_root_lpdf(real x, real mu, real sd,real k){
					return normal_lpdf(x|mu,sd)/k;
				}
}
data{
	int<lower=1> k;
	int<lower=1> N;
	real x[N];
	real m_prior;
	real sd_prior;
  real expo_prior;
}

parameters{
	real mu;
	real sigma;
}

model{
	//mu ~ normal(m_prior,sd_prior);
	mu ~ normal_root(m_prior,sd_prior,k);
	sigma ~ exponential(expo_prior);
	x ~ normal(mu,sigma);
}
'
stan_model_divided_normal <- stan_model(model_code=stan_model_code)
save(stan_model_divided_normal,file='toy_normal_divided_stan.RData')
