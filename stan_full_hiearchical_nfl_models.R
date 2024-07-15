rm(list=ls())

# Data loading and handling
nfl <- read.csv('nfl_team_quarter_year.csv')
#nfl <- subset(nfl,quarter%in%1:4)
nfl <- subset(nfl,n_pass>0&n_rush>0&yards_pass>0&yards_rush>0)
team <- as.numeric(as.factor(nfl$team))
year <- as.numeric(as.factor(nfl$year))
quarter <- nfl$quarter
n_plays <- nfl$n_plays
n_pass <- nfl$n_pass
yards_pass <- nfl$yards_pass
yards_rush <- nfl$yards_rush
n_qrs <- length(unique(quarter))
n_tms <- length(unique(team))
n_obs <- nrow(nfl)

observed <- list(n_obs = n_obs, n_tms = n_tms, team = team, 
                  yards_pass = yards_pass, yards_rush = yards_rush, 
                  n_pass = n_pass, n_plays = n_plays)
load('stan_nfl_m3_full.RData')
fit <- sampling(stan_model_full_nfl_m3,data=observed,iter=1000,chains=3,seed=123)
nds_m3_stan <- extract(fit)
save(nds_m3_stan,file='nds_stan_full_nfl_m3.RData')
