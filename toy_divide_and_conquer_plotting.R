library('parallelMCMCcombine')
subarray <- array(dim=c(1,length(shards[[1]]$mcmc$theta),length(shards)))
for(sh in 1:length(shards)){
	subarray[1,,sh] <- shards[[sh]]$mcmc$theta
}
posterior <- consensusMCindep(subarray)
#posterior <- semiparamDPE(subarray)


# Plotting
#try(dev.off())
#x11(width=16,height=12)
pdf(file='toy_divide_and_conquer.pdf',width=16,height=12)
layout(matrix(1:2,nrow=2))

# Full data
hist(nds$theta,breaks=seq(0,1,length.out=131),freq=F,ylim=c(0,55))
hist(nds$theta_prior,breaks=seq(0,1,length.out=130),freq=F,col='#2189ee44',add=T)
lines(sup_th,closed_post,type='l',lwd=3)
lines(sup_th,closed_prior,type='l',lwd=2,lty='dashed')

# Divided data
plot(NULL,xlim=c(0,1),ylim=c(0,55))
for(sh in 1:(length(shards))){
	lines(density(shards[[sh]]$mcmc$theta,from=0,to=1))
	lines(sup_th,shards[[sh]]$closed,lty='dotted')	
}
hist(posterior,freq=F,breaks=20,col='red',add=T)
dev.off()
