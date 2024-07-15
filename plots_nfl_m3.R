load('shards_m3_stan.RData')
load('nds_stan_full_nfl_m3.RData')
library('parallelMCMCcombine')

marginal_recombine <- function(node_name){
	#node_name <- 'mean_alpha'
	subarray <- array(dim=c(1,length(shards[[1]]$nds[[node_name]]),length(shards)))
	for(sh in 1:length(shards)){
		subarray[1,,sh] <- shards[[sh]]$nds[[node_name]]
	}
	CMC <- consensusMCindep(subarray)
	return(list(CMC=CMC))
#	SDE <- semiparamDPE(subarray)
#	return(list(CMC=CMC,SDE=SDE))
}

plot_parent_node <- function(node_name,
		xlimz=c(-.5,1),ylimz=c(0,45),clp=F,ann_type='none',legend=F){
	
	col_shards <- '#2dd460'
	col_full <- '#208eb7'
	col_cmc <- '#fb7810'
	plot(NULL,xlim=xlimz,ylim=ylimz,ann=F,axes=F)
	ann_col <- '#aaaaaa'
	if(ann_type=='bias'){
		abline(v=0,lty='33',col=ann_col)
		text(0+c(-1,1)*.25,rep(ylimz[2]*.95,2),c('Bias towards rushing','Bias towards passing'),
			font=3,col=ann_col)
	}
	if(ann_type=='sensitivity'){
		abline(v=1,lty='33',col=ann_col)
		text(1+c(-1,.9)*.2,rep(ylimz[2]*.95,2),c('Undermatching','Overmatching'),font=3,col=ann_col)
	}
	if(clp){clip(0,.15,-10,100)}
	for(s in 1:length(shards)){
		nd <- density(shards[[s]]$nds[[node_name]])
		lines(nd,col=col_shards,lwd=0.75)
	}
	recombine <- marginal_recombine(node_name)
	lines(density(recombine$CMC),lwd=2,col=col_cmc)
	lines(density(nds_m3_stan[[node_name]]),col=col_full,lwd=2,lty='32')
	axis(1)
	box()
	if(legend){
		legend(.1,ylimz[2]*.5,yjust=0.5,legend=c('Subposteriors','CMC recombination','Full dataset at once'),
							col=c(col_shards,col_cmc,col_full),lwd=c(0.85,2,2),lty=c('solid','solid','21'),seg.len=1.3,cex=.9,box.lty='blank')
	}
}

pdf(file='m3_parent_nodes.pdf',width=10,height=5)
par(mar=c(2,1,2,1),tck=-0.03,mgp=c(3,0.5,0),oma=c(1,4,0,0))
layout(matrix(1:4,ncol=2,byrow=T))
plot_parent_node('mean_alpha',xlimz=c(-.6,.6),ann_type='bias',legend=T)
mtext(expression(mu[alpha]),1,line=2,cex=1.5)
mtext('Hierarchical\nmeans',2,line=1,cex=1.5,font=2)
plot_parent_node('mean_beta',xlimz=c(0.4,1.6),ann_type='sensitivity')
mtext(expression(mu[beta]),1,line=2,cex=1.5)
plot_parent_node('sd_alpha',xlimz=c(0,0.15),ylimz=c(0,60),clp=T)
mtext(expression(sigma[alpha]),1,line=2,cex=1.5)
mtext('Hierarchical\ndeviations',2,line=1,cex=1.5,font=2)
plot_parent_node('sd_beta',xlimz=c(0,0.15),ylimz=c(0,60),clp=T)
mtext(expression(sigma[beta]),1,line=2,cex=1.5)
dev.off()
