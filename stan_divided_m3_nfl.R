rm(list=ls())
library('rstan')
nfl <- read.csv('nfl_team_quarter_year.csv')
nfl <- subset(nfl,n_pass>0&n_rush>0&yards_pass>0&yards_rush>0)
nfl$team <- as.numeric(as.factor(nfl$team))
nfl$year <- as.numeric(as.factor(nfl$year))
# Global counters
n_qrs <- length(unique(nfl$quarter))
n_tms <- length(unique(nfl$team))

infer_batch_m3 <- function(nfl){
	team <- nfl$team
	year <- nfl$year
	quarter <- nfl$quarter
	n_plays <- nfl$n_plays
	n_pass <- nfl$n_pass
	yards_pass <- nfl$yards_pass
	yards_rush <- nfl$yards_rush
	n_obs <- nrow(nfl)
	observed <- list(n_obs = n_obs, n_tms = n_tms, team = team, 
 	                 yards_pass = yards_pass, yards_rush = yards_rush, 
	                  n_pass = n_pass, n_plays = n_plays,k=n_batches)
	fit <- sampling(stan_model_divided_nfl_m3,data=observed,iter=2000,chains=3,seed=123)
	nds_m3_stan <- extract(fit)
	return(nds_m3_stan)
}

n_batches <- 10
total_obs <- nrow(nfl)
full_indx <- 1:total_obs
shards <- vector(mode='list',length=n_batches)
load('stan_nfl_m3_divided.RData')
for(b in 1:n_batches){
	shards[[b]]$indices <- sample(full_indx,size=total_obs/n_batches,replace=F)
	full_indx <- full_indx[!full_indx%in%shards[[b]]$indices]
	shards[[b]]$data <- nfl[shards[[b]]$indices,]
	shards[[b]]$nds <- infer_batch_m3(shards[[b]]$data)
}
save(shards,file='shards_m3_stan.RData')
