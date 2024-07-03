rm(list=ls())
game_files <- dir()[grep('pbp-',dir())]
game_files <- game_files[-grep('2013',game_files)]
extract_variables <- c('GameId','SeasonYear','PlayType','IsRush','IsPass','Yards',
											 'Quarter','Down','OffenseTeam','DefenseTeam',
											 'ToGo','YardLine','Minute','Second')
full_nfl <- NULL
cntr <- 0
for(y in game_files){
	cntr <- cntr+1
	gset <- read.csv(y,stringsAsFactors=F)
	if(cntr==1){
  	full_nfl <- gset[,extract_variables]
	}
	else{
		full_nfl <- rbind(read.csv('nfl_2014_2023.csv'),gset[,extract_variables])
	}
	write.csv(full_nfl,file='nfl_2014_2023.csv',row.names=F)
#	cat(y,dim(gset),names(gset),'\n')
  #cat(y,unique(gset$PlayType),'\n')
  #cat(y,length(unique(gset$GameId)),'\n')
	cat(y,sum(!extract_variables%in%names(gset)),'\n') 
}

rm(list=ls())
full_nfl <- read.csv('nfl_2014_2023.csv')
full_nfl <- full_nfl[which(full_nfl$PlayType%in%c('RUSH','PASS')),]
full_nfl <- full_nfl[which(full_nfl$Down!=0),]
write.csv(full_nfl,file='nfl_2014_2023.csv',row.names=F)


