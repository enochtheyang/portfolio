
rm(list=ls())
library(neuralnet)
require(stringr)
require(dplyr)
library(rpart)
library(rpart.plot)
library(e1071)
setwd("C:/Users/Tweek/Documents/MSBX 5415/dcereijo-player-scores/data")
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
gamesNumeric <- games[, sapply(games, is.numeric)]
gamesNumeric$result <- games[,'result']
gamesNumeric$date <- games[,'date']

#Separating testing and training data
train<-gamesNumeric[gamesNumeric$date<"2022-01-01",]
train <- select(train, -'date')
#svm cant handle missing values in test data; it'll just drop them
test<-gamesNumeric[gamesNumeric$date>="2022-01-01",]
test <- select(test, -'date')
str(test)
str(train)


#--------------------------------------------------------------------------------------------
# NN prediction

library(nnet)
set.seed(88)
train.net <- nnet(result ~ ., data = test,
                  linout = FALSE, size = 2, maxit=100)

Win.pred <- predict(train.net, test)
Win.pred
# order the predictions by cancellation probabilities
pred <- data.frame(matchid = 1:nrow(test), result = test$result, 
                   Win.prob = Win.pred[,])
pred <- pred[order(pred$Win.prob, decreasing = TRUE), ]

# create another column in the data frame pred to store the predicted
# cancellation outcome
pred$Win.nn <- as.numeric(pred$Win.prob > .5)

# confusion matrix of the prediction
table(pred$result, pred$Win.nn)

#Accuracy
(table(pred$result, pred$Win.nn)[1,1]+table(pred$result, pred$Win.nn)[2,2])/nrow(pred)

#win prediction sensitivity
table(pred$result, pred$Win.nn)[1,1]/(table(pred$result, pred$Win.nn)[1,1]+table(pred$result, pred$Win.nn)[1,2])

#win prediction precision
table(pred$result, pred$Win.nn)[1,1]/(table(pred$result, pred$Win.nn)[1,1]+table(pred$result, pred$Win.nn)[2,1])

#loss prediction precision
table(pred$result, pred$Win.nn)[2,2]/(table(pred$result, pred$Win.nn)[2,2]+table(pred$result, pred$Win.nn)[1,2])

#One hidden layer
#Size 2 = .7248085
#Size 7 = .5571597
#Size 8 = .6484973
#Size 10 = .7218621


#This didn't work, dunno why
train.net <- neuralnet(result ~ ., 
                         data = test,
                         linear.output = FALSE, hidden = 2)
plot(train.net)
