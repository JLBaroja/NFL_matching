rm(list=ls())
library('R2jags')

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

observed <- list('team','year','quarter',
	'n_plays','n_pass','yards_pass','yards_rush',
	'n_qrs','n_tms','n_obs')


write('
			model{
						# Priors
							alpha ~ dnorm(0,1)
							beta ~ dnorm(0,1)
						# Data	
							for(i in 1:n_obs){
											logit(theta[i]) <- alpha + beta * log(yards_pass[i]/yards_rush[i]) 
											n_pass[i] ~ dbinom(theta[i],n_plays[i]) 
							}
			}
			','model_m1.bug')
unobserved <- c('alpha','beta')
bayes_m1 <- jags(data=observed,
	parameters.to.save=unobserved,
	model.file='model_m1.bug',
	n.chains=3,n.iter=1000,n.burnin=0,n.thin=1)
unlink('model_m1.bug')
save(bayes_m1,file='bayes_m1.RData')
nds_m1 <- bayes_m1$BUGSoutput$sims.list

write('
			model{
						# Priors
							for(q in 1:n_qrs){
								alpha[q] ~ dnorm(0,1)
								beta[q] ~ dnorm(0,1)
							}
						# Data	
							for(i in 1:n_obs){
											logit(theta[i]) <- alpha[quarter[i]] + beta[quarter[i]] * log(yards_pass[i]/yards_rush[i]) 
											n_pass[i] ~ dbinom(theta[i],n_plays[i]) 
							}
			}
			','model_m2.bug')
unobserved <- c('alpha','beta')
bayes_m2 <- jags(data=observed,
	parameters.to.save=unobserved,
	model.file='model_m2.bug',
	n.chains=3,n.iter=1000,n.burnin=0,n.thin=1)
unlink('model_m2.bug')
save(bayes_m2,file='bayes_m2.RData')
nds_m2 <- bayes_m2$BUGSoutput$sims.list

write('
			model{
						# Priors
							for(t in 1:n_tms){
								alpha[t] ~ dnorm(0,1)
								beta[t] ~ dnorm(0,1)
							}
						# Data	
							for(i in 1:n_obs){
											logit(theta[i]) <- alpha[team[i]] + beta[team[i]] * log(yards_pass[i]/yards_rush[i]) 
											n_pass[i] ~ dbinom(theta[i],n_plays[i]) 
							}
			}
			','model_m3.bug')
unobserved <- c('alpha','beta')
bayes_m3 <- jags(data=observed,
	parameters.to.save=unobserved,
	model.file='model_m3.bug',
	n.chains=3,n.iter=1000,n.burnin=0,n.thin=1)
unlink('model_m3.bug')
save(bayes_m3,file='bayes_m3.RData')
nds_m3 <- bayes_m3$BUGSoutput$sims.list
