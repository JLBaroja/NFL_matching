
#data_factor <- function(factor_label){
#	dset <- NULL
#	#factor_label <- 'GameId'
#	for(g in unique(nfl[,factor_label])){
#		gset <- subset(nfl,nfl[,factor_label]==g)
#	#for(g in unique(nfl$OffenseTeam)){
#	#	gset <- subset(nfl,OffenseTeam==g)
#	#for(g in sort(unique(nfl$SeasonYear))){
#	#	gset <- subset(nfl,SeasonYear==g)
#		summary_set <- data.frame(
#			fctr_level=g,
#			n_plays=nrow(gset),
#			n_pass=sum(gset$PlayType=='PASS'),
#			n_rush=sum(gset$PlayType=='RUSH'),
#			yards_pass=sum(gset$Yards[gset$PlayType=='PASS']),
#			yards_rush=sum(gset$Yards[gset$PlayType=='RUSH'])
#		)
#		dset <- rbind(dset,summary_set)
#	}
#	return(dset)
#}

rm(list=ls())
nfl <- read.csv('nfl_2014_2023.csv')
nfl$OffenseTeam[nfl$OffenseTeam=='SD'] <- 'LAC'
dset <- NULL
for(tm in sort(unique(nfl[,'OffenseTeam']))){
		for(qr in sort(unique(nfl[,'Quarter']))){
			for(yr in sort(unique(nfl[,'SeasonYear']))){
				gset <- subset(nfl,nfl[,'OffenseTeam']==tm&nfl[,'Quarter']==qr&nfl[,'SeasonYear']==yr)
				#for(g in unique(nfl$OffenseTeam)){
				#gset <- subset(nfl,OffenseTeam==g)
				#for(g in sort(unique(nfl$SeasonYear))){
				#gset <- subset(nfl,SeasonYear==g)
				summary_set <- data.frame(
								team=tm,
								quarter=qr,
								year=yr,
								n_plays=nrow(gset),
								n_pass=sum(gset$PlayType=='PASS'),
								n_rush=sum(gset$PlayType=='RUSH'),
								yards_pass=sum(gset$Yards[gset$PlayType=='PASS']),
								yards_rush=sum(gset$Yards[gset$PlayType=='RUSH'])
				)
		dset <- rbind(dset,summary_set)
		}
	}
}
write.csv(dset,file='nfl_team_quarter_year.csv',row.names=F)
