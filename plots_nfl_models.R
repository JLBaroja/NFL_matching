rm(list=ls())
library('MASS')
library('car')

nfl <- read.csv('nfl_team_quarter_year.csv')
nfl <- subset(nfl,n_pass>0&n_rush>0&yards_pass>0&yards_rush>0)
#nfl <- subset(nfl,quarter%in%1:4)
quarter <- nfl$quarter
load('bayes_m1.RData')
load('bayes_m2.RData')
load('bayes_m3.RData')
nds_m1 <- bayes_m1$BUGSoutput$sims.list
nds_m2 <- bayes_m2$BUGSoutput$sims.list
nds_m3 <- bayes_m3$BUGSoutput$sims.list

colors_quarters <- c('red','blue','green','orange','cyan')
colors_quarters <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')
try(dev.off())
x11(width=17,height=8)
layout(matrix(c(1,1,1,1,2,3,4,5,0,6,0,7),nrow=4,byrow=F),
	widths=c(2,1,1),heights=c(1,2,1,2))

# Data
plot(NULL,xlim=c(-1,3),ylim=c(-1,3))
kernels <- vector(mode='list',length=5)
for(qr in 1:5){
	qr_dta <- subset(nfl,quarter==qr)
	x <- log(qr_dta$yards_pass/qr_dta$yards_rush)
	y <- log(qr_dta$n_pass/qr_dta$n_rush)
	kernels[[qr]] <- kde2d(x,y)
	points(x,y,col=paste(colors_quarters[qr],'36',sep=''),cex=1.5,pch=qr,lwd=2)
	dataEllipse(x,y,add=T,col=colors_quarters[qr],levels=0.95,pch=NA)
}
#for(qr in 1:5){
#	qr_dta <- subset(nfl,quarter==qr)
#	x <- log(qr_dta$yards_pass/qr_dta$yards_rush)
#	y <- log(qr_dta$n_pass/qr_dta$n_rush)
#	dataEllipse(x,y,add=T,col=colors_quarters[qr],levels=0.95,pch=NA)
#	#contour(kernels[[qr]],levels=1,add=T,lwd=3,drawlabels=F,col=colors_quarters[qr])
#}
legend(-1,3,legend=paste(c('1st','2nd','3rd','4th','5th'),'Quarter'),col=colors_quarters,
 pch=1:5,cex=2)
abline(0,1,lty='dashed')
abline(v=0,lty='dashed')
abline(h=0,lty='dashed')
mtext('Grouping plays by team & quarter & year',3,line=1,cex=1.5)

multi_marginals <- function(node,def=500,xlimz=c(0,1),ylimz=c(0,25),
														palette=NULL,title=NULL){
				if(is.null(palette)){palette<-rep('#000000',dim(node)[2])}
	#node <- nds_m2$alpha
	brks <- seq(-2,2,length.out=def)
	plot(NULL,xlim=xlimz,ylim=ylimz,ann=F,axes=F)
	for(i in 1:dim(node)[2]){
		hist(node[,i],breaks=brks,plot=F) -> ht
		x <- ht$mids[ht$density>0.15]
		y <- ht$density[ht$density>0.15]
		lines(x,y,col=palette[i],lwd=2)
	}
	if(!is.null(title)){mtext(title,3,line=1,cex=1.5)}
}

interval_marginals <- function(node,order=F,xlimz=c(-.5,.5),cols=NULL,rct_width=0.8){
	if(order){
		node <- node[,order(apply(node,FUN=mean,MARGIN=2))]
	}
	if(is.null(cols)){cols=rep('#000000',dim(node)[2])}
	plot(NULL,xlim=xlimz,ylim=c(0,dim(node)[2]+1))
	for(i in 1:dim(node)[2]){
		quant <- quantile(node[,i],probs=c(0.025,0.975))
		summ <- summary(node[,i])
		lines(quant,rep(i,2),lwd=2,col=cols[i])
		rect(xleft=summ['1st Qu.'],ybottom=i-rct_width/2,xright=summ['3rd Qu.'],ytop=i+rct_width/2,
			border=cols[i],lwd=2,col='#ffffff')
	}
}

par(mar=c(1,4,4,2))
multi_marginals(nds_m2$alpha,xlimz=c(-.5,.5),palette=colors_quarters,title='Intercept (Bias)')
par(mar=c(5,4,0,2))
interval_marginals(nds_m2$alpha,cols=colors_quarters)
par(mar=c(1,4,4,2))
multi_marginals(nds_m2$beta,palette=colors_quarters,title='Slope (Sensitivity)')
par(mar=c(5,4,0,2))
interval_marginals(nds_m2$beta,xlimz=c(0,1),cols=colors_quarters)
# Marginals Team
interval_marginals(nds_m3$alpha,order=T)
interval_marginals(nds_m3$beta,order=T,xlimz=c(0,1))
#multi_marginals(nds_m3$alpha,def=200,xlimz=c(-.5,.5),ylimz=c(0,12),title='Intercept (Bias)')
#multi_marginals(nds_m3$beta,,def=200,ylimz=c(0,12),title='Slope (Sensitivity)')
