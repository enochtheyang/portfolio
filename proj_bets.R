rm(list=ls())
require(stringr)
require(dplyr)
library(rpart)
library(rpart.plot)
library(e1071)
clubs<-read.csv('clubs.csv',stringsAsFactors=TRUE)
games<-read.csv('games.csv',stringsAsFactors=TRUE)
competitions<-read.csv('competitions.csv',stringsAsFactors=TRUE)

games$date<-as.Date(games$date)

games<-games[games$date>"2018-01-01",]
games$result<-unlist(lapply(games$aggregate, 
                            function (x) 
                            {ifelse((as.numeric(str_split(x,':',simplify=TRUE)[1,1])>as.numeric(str_split(x,':',simplify=TRUE)[1,2])),'Home Win',ifelse((as.numeric(str_split(x,':',simplify=TRUE)[1,1])==as.numeric(str_split(x,':',simplify=TRUE)[1,2])),'Not Home Win','Not Home Win'))
                            }))

games$result<-as.factor(games$result)

#Adding home and away clubs stats to the games data frame

home_clubs<-clubs
colnames(home_clubs)<-paste(colnames(home_clubs),'home',sep='_')

away_clubs<-clubs
colnames(away_clubs)<-paste(colnames(away_clubs),'away',sep='_')

games<-left_join(games,home_clubs,by=c('home_club_id'='club_id_home'))
games<-left_join(games,away_clubs,by=c('away_club_id'='club_id_away'))

#adding competition data
games<-left_join(games,competitions,by=c('competition_id'='competition_id'))

#dropping unnecessary columns
colnames(games)
games<-games[,!grepl('url',colnames(games))]
games<-games[,!grepl('aggregate',colnames(games))]
games<-games[,!grepl('goals',colnames(games))]
games<-select(games,-c("game_id","name_home","pretty_name_home","domestic_competition_id_home","stadium_seats_home","name_away","pretty_name_away","domestic_competition_id_away","stadium_seats_away","name","country_id","domestic_league_code","confederation","country_latitude","country_longitude"))
games <- select(games,-c('home_club_id','away_club_id','attendance'))

#svm cant handle nulls well; performing complete cases
games <- games[complete.cases(games), ]

#Separating testing and training data
train<-games[games$date<"2022-01-01",]
#svm cant handle missing values in test data; it'll just drop them
test<-games[games$date>="2022-01-01",]

#Finding Premier League

test[test$club_home_pretty_name=='Manchester United'&test$club_away_pretty_name=='Fc Everton',]
betTest <- test[test$competition_id=='GB1'&test$date>='2022-08-05',]
getwd()


betOdds <- read.csv('2022 Premier league.csv')
for (i in 1:nrow(betOdds)){
  if (betOdds[i,'home_win_odds']<=0){
    betOdds[i,'winamount'] <- ((-100/betOdds[i,'home_win_odds'])*100)}
  else{
    betOdds[i,'winamount'] <- betOdds[i,'home_win_odds']}}



#Run proj_nn to get train the predictor

Win.pred <- predict(train.net, betOdds)
Win.pred
# order the predictions by cancellation probabilities
pred <- data.frame(matchid = 1:nrow(betOdds), result = betOdds$result, 
                   Win.prob = Win.pred[,], oddsWinLoss = betOdds$winamount)
pred <- pred[order(pred$Win.prob, decreasing = TRUE), ]

# create another column in the data frame pred to store the predicted
# cancellation outcome
pred$Win.nn <- as.numeric(pred$Win.prob > .5)
pred

#Check money gain/loss
gainLoss <- 0
losses <- 0
for (i in 1:nrow(pred)){
  if (pred[i,'Win.nn']==0){
    if (pred[i,'result']=='Home Win'){
      gainLoss <- gainLoss + pred[i,'oddsWinLoss']}
    else{
      gainLoss <- gainLoss-100}
      losses <- losses +1
  }
}
gainLoss
losses
betOdds
