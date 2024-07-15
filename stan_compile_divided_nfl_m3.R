library('rstan')
stan_code <- '
functions{
				real normal_root_lpdf(real x, real mu, real sd,real k){
					return normal_lpdf(x|mu,sd)/k;
				}
				real uniform_root_lpdf(real x, real l, real u,real k){
					return uniform_lpdf(x|l,u)/k;
				}
}
data {
	int<lower=1> k;                      // kth root of the prior
  int<lower=0> n_obs;                  // number of observations
  int<lower=1> n_tms;                  // number of teams
  int<lower=1, upper=n_tms> team[n_obs]; // team identifier
  vector[n_obs] yards_pass;            // yards passed
  vector[n_obs] yards_rush;            // yards rushed
  int<lower=0> n_pass[n_obs];          // number of passes
  int<lower=0> n_plays[n_obs];         // number of plays
}

parameters {
  real mean_alpha;                     // global intercept mean
  real mean_beta;                      // global slope mean
  real<lower=0, upper=2> sd_alpha;     // std dev of intercepts
  real<lower=0, upper=2> sd_beta;      // std dev of slopes

  vector[n_tms] alpha_raw;             // raw intercepts for each team
  vector[n_tms] beta_raw;              // raw slopes for each team
}

transformed parameters {
  vector[n_tms] alpha;                 // intercepts for each team
  vector[n_tms] beta;                  // slopes for each team

  alpha = mean_alpha + sd_alpha * alpha_raw;
  beta = mean_beta + sd_beta * beta_raw;
}

model {
 // mean_alpha ~ normal(0, 1);
 // mean_beta ~ normal(0, 1);
  mean_alpha ~ normal_root(0, 1, k);
  mean_beta ~ normal_root(0, 1, k);
 // sd_alpha ~ uniform(0, 2);
 // sd_beta ~ uniform(0, 2);
  sd_alpha ~ uniform_root(0, 2, k);
  sd_beta ~ uniform_root(0, 2, k);

  alpha_raw ~ normal(0, 1);
  beta_raw ~ normal(0, 1);

  for (i in 1:n_obs) {
    real logit_theta;
    logit_theta = alpha[team[i]] + beta[team[i]] * log(yards_pass[i] / yards_rush[i]);
    n_pass[i] ~ binomial_logit(n_plays[i], logit_theta);
  }
}
'
stan_model_divided_nfl_m3 <- stan_model(model_code=stan_code)
save(stan_model_divided_nfl_m3,file='stan_nfl_m3_divided.RData')
