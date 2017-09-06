games <- read.csv("College_Football_Games_4.csv")

# (\(\d{1,2}\)\s)
games$Winner <- gsub("(\\(\\d{1,2}\\)\\s)","",games$Winner)
games$Loser <- gsub("(\\(\\d{1,2}\\)\\s)","",games$Loser)

teams.in.both.columns <- intersect(games$Winner,games$Loser)
length(teams.in.both.columns)
teams.in.winners.not.losers <- setdiff(games$Winner,games$Loser)
length(teams.in.winners.not.losers)
teams.in.losers.not.winners <- setdiff(games$Loser,games$Winner)
length(teams.in.losers.not.winners)
teams.in.both.columns <- c(teams.in.both.columns,teams.in.winners.not.losers, teams.in.losers.not.winners)
length(teams.in.both.columns)

#Don't forget about West Texas State -> UTEP and Southwest Tx -> Texas St

colnames(games)[6] <- "Winner_Pts"
colnames(games)[9] <- "Loser_Pts"

games$E_Winner <- c(0)
games$E_Loser <- c(0)

dates <- as.character(unique(games$Date))
games$Date <- as.character(games$Date)

rating.history <- matrix(nrow = length(dates),ncol = length(teams.in.both.columns))

rownames(rating.history) <- dates
colnames(rating.history) <- teams.in.both.columns

# teams.df <- data.frame(teams.in.both.columns,c(0),c(1300),c(0),c(1869),c(0),c(0),c(0))
teams.df <- data.frame(integer(length(teams.in.both.columns)),
                       rep(1300,length(teams.in.both.columns)),
                       rep(1869,length(teams.in.both.columns)),
                       integer(length(teams.in.both.columns)),
                       integer(length(teams.in.both.columns)),
                       integer(length(teams.in.both.columns)))

colnames(teams.df) <- c("Game_Index","Rating","Latest_Year","Wins","Losses","Ties")
rownames(teams.df) <- teams.in.both.columns

initial.rating <- 1500

#Factor used to compute k factor
ne <- 50/sqrt(1 + (2200 - initial.rating)^2/100000)

games.in.season <- 12

#the higher the factor, the more the ratings fluctuate for a game
#for CFB, it's about 24.55
k.factor <- 800 / (ne + games.in.season)

model.correct <- 0
model.wrong <- 0

head(games)
head(teams.df)

for (i in 1:nrow(games)) {

  winner.initial.rating <- teams.df[games[i,]$Winner, "Rating"]
  loser.initial.rating <- teams.df[games[i,]$Loser, "Rating"]

  if (teams.df[games[i,]$Winner, "Game_Index"] != 0 
      & teams.df[games[i,]$Winner, "Latest_Year"] != games[i,]$Season) {
    
    winner.initial.rating <- teams.df[games[i,]$Winner, "Rating"] * 0.66 + 1500 * 0.34
    teams.df[games[i,]$Winner, "Latest_Year"] <- games[i,]$Season
  }
  
  if (teams.df[games[i,]$Loser, "Game_Index"] != 0
     & teams.df[games[i,]$Loser, "Latest_Year"] != games[i,]$Season) {
    
    loser.initial.rating <- teams.df[games[i,]$Loser, "Rating"] * 0.66 + 1500 * 0.34
    teams.df[games[i,]$Loser, "Latest_Year"] <- games[i,]$Season
  }
  
  winner.score <- games[i,]$Winner_Pts
  loser.score <- games[i,]$Loser_Pts

  if (winner.initial.rating > loser.initial.rating & winner.score > loser.score) {
    model.correct <- model.correct + 1
  }else if (winner.initial.rating < loser.initial.rating & winner.score > loser.score) {
    model.wrong <- model.wrong + 1
  }
  
  abs.mov <- abs(winner.score - loser.score)

  winner.minus.loser <- winner.initial.rating - loser.initial.rating
  loser.minus.winner <- -winner.minus.loser

  #expected home and away win percentages
  e.winner <- 1/(1 + 10^(loser.minus.winner/400))
  e.loser <- 1/(1 + 10^(winner.minus.loser/400))

  games[i,]$E_Winner <- e.winner
  games[i,]$E_Loser <- e.loser
  
  s.winner <- 1
  s.loser <- 0

  winner.rating <- 0
  loser.rating <- 0
  
  if (winner.score == loser.score) {
    
    winner.rating <- winner.initial.rating
    loser.rating <- loser.initial.rating
    
    teams.df[games[i,]$Winner, "Ties"] <- teams.df[games[i,]$Winner, "Ties"] + 1
    teams.df[games[i,]$Loser, "Ties"] <- teams.df[games[i,]$Loser, "Ties"] + 1
  }else{
    winner.rating <- winner.initial.rating + log(abs.mov + 1) * k.factor * (s.winner - e.winner)
    loser.rating <- loser.initial.rating + log(abs.mov + 1) * k.factor * (s.loser - e.loser)
    
    teams.df[games[i,]$Winner, "Wins"] <- teams.df[games[i,]$Winner, "Wins"] + 1
    teams.df[games[i,]$Loser, "Losses"] <- teams.df[games[i,]$Loser, "Losses"] + 1
  }
  
  teams.df[games[i,]$Winner, "Rating"] <- winner.rating
  teams.df[games[i,]$Loser, "Rating"] <- loser.rating

  teams.df[games[i,]$Winner, "Game_Index"] <- teams.df[games[i,]$Winner, "Game_Index"] + 1
  teams.df[games[i,]$Loser, "Game_Index"] <- teams.df[games[i,]$Loser, "Game_Index"] + 1
  
  rating.history[games[i,]$Date,games[i,]$Winner] <- winner.rating
  rating.history[games[i,]$Date,games[i,]$Loser] <- loser.rating
}

teams.df[order(-teams.df$Rating),]
teams.df[order(teams.df$Rating),]

recent.teams.df <- subset(teams.df,teams.df$Latest_Year > 2014)
recent.teams.df <- recent.teams.df[order(-recent.teams.df$Rating),]

recent.teams.df
recent.teams.df[order(recent.teams.df$Rating),]

fbs.teams <- rownames(recent.teams.df)

fbs.history <- rating.history[,fbs.teams]

avg.elo.all.time <- matrix(nrow = length(fbs.teams), ncol = 1)
rownames(avg.elo.all.time) <- fbs.teams
for (j in 1:length(fbs.teams)) {
  
  avg.elo.all.time[j,] <- mean(fbs.history[,j], na.rm = TRUE)
}
avg.elo.all.time[order(avg.elo.all.time[,1],decreasing = TRUE),]

# scen_a <- subset(games,games$E_Winner - games$E_Loser > 0.25)
# scen_b <- subset(games,games$E_Winner - games$E_Loser < 0.25 & games,games$E_Winner - games$E_Loser > 0)
# scen_c <- subset(games,games$E_Winner - games$E_Loser < 0 & games,games$E_Winner - games$E_Loser > -0.25)
# scen_d <- subset(games,games$E_Winner - games$E_Loser < -0.25)
# scen_e <- subset(games,games$E_Winner - games$E_Loser > 0)
# scen_f <- subset(games,games$E_Winner - games$E_Loser < 0)


# matplot(rating.history)
# matplot(fbs.history)
# plot(fbs.history[,"Texas"],pch="*")
#
# plot(fbs.history[,"Texas A&M"],pch="*",ylim=c(yfloor,yceil))
# abline(h=1500, untf=FALSE, lty=2)