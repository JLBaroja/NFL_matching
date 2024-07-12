library('parallelMCMCcombine')
subarray <- array(dim=c(1,length(shards[[1]]$mcmc$mu),length(shards)))
for(sh in 1:length(shards)){
	subarray[1,,sh] <- shards[[sh]]$mcmc$mu
}
posterior <- consensusMCindep(subarray)
#posterior <- semiparamDPE(subarray)


# Plotting
#try(dev.off())
#x11(width=16,height=12)
pdf(file='toy_divide_and_conquer_normal.pdf',width=16,height=12)
layout(matrix(1:2,nrow=2))

# Full data
hist(nds$mu,breaks=seq(0,60,length.out=131),freq=F,ylim=c(0,1),col='orange')

# Divided data
plot(NULL,xlim=c(0,60),ylim=c(0,1))
for(sh in 1:(length(shards))){
  #hist(shards[[sh]]$mcmc$mu,add=T)
	lines(density(shards[[sh]]$mcmc$mu,from=0,to=60))
}
hist(posterior,freq=F,breaks=20,col='red',add=T)
dev.off()
