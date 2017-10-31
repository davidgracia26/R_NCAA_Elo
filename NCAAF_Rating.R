games <- read.csv("College_Football_Games_4.csv")

CalculateRatingDifference <- function(teamsDf, team1, team2){
  teamsDf[team1,]$Rating - teamsDf[team2,]$Rating
}

# Removes rankings i.e. (25) from the team name
games$Winner <- gsub("(\\(\\d{1,2}\\)\\s)","",games$Winner)
games$Loser <- gsub("(\\(\\d{1,2}\\)\\s)","",games$Loser)

teams.in.both.columns <- intersect(games$Winner,games$Loser)
teams.in.winners.not.losers <- setdiff(games$Winner,games$Loser)
teams.in.losers.not.winners <- setdiff(games$Loser,games$Winner)
teams.in.both.columns <- c(teams.in.both.columns,
                           teams.in.winners.not.losers, 
                           teams.in.losers.not.winners)

#Don't forget about West Texas State -> UTEP and Southwest Tx -> Texas St

colnames(games)[6] <- "Winner_Pts"
colnames(games)[9] <- "Loser_Pts"

games$E_Winner <- c(0)
games$E_Loser <- c(0)
games$Winner_Rating <- c(0)
games$Loser_Rating <- c(0)
games$Winner_ML <- c(0)
games$Loser_ML <- c(0)
games$Model_Correct <- c(0)
games$Model_Wrong <- c(0)


dates <- as.character(unique(games$Date))
games$Date <- as.character(games$Date)

rating.history <- matrix(nrow = length(dates), ncol = length(teams.in.both.columns))

rownames(rating.history) <- dates
colnames(rating.history) <- teams.in.both.columns

teams.df <- data.frame(teams.in.both.columns,
                       integer(length(teams.in.both.columns)),
                       rep(1300,length(teams.in.both.columns)),
                       rep(1869,length(teams.in.both.columns)),
                       integer(length(teams.in.both.columns)),
                       integer(length(teams.in.both.columns)),
                       integer(length(teams.in.both.columns)),
                       integer(length(teams.in.both.columns)),
                       integer(length(teams.in.both.columns)))

colnames(teams.df) <- c("Name",
                        "Game_Index",
                        "Rating",
                        "Latest_Year",
                        "Wins",
                        "Losses",
                        "Ties",
                        "Season_Init_Rtg",
                        "Rtg_Change")

initial.rating <- 1500

#Factor used to compute k factor
ne <- 50/sqrt(1 + (2200 - initial.rating)^2/100000)

games.in.season <- 12

#the higher the factor, the more the ratings fluctuate for a game
#for CFB, it's about 24.55
k.factor <- 800 / (ne + games.in.season)

model.correct <- 0
model.wrong <- 0

winner.ratings <- integer(nrow(games))
loser.ratings <- integer(nrow(games))

e.loser.values <- integer(nrow(games))
e.winner.values <- integer(nrow(games))

dates.for.history <- character(2*nrow(games))
teams.for.history <- character(2*nrow(games))
ratings.for.history <- integer(2*nrow(games))

for (i in 1:nrow(games)) {

  # print(i)
  
  winner.teams.df.index <- which(teams.df[,]$Name == games[i,]$Winner)
  loser.teams.df.index <- which(teams.df[,]$Name == games[i,]$Loser)
  
  winner.initial.rating <- teams.df[winner.teams.df.index,]$Rating
  loser.initial.rating <- teams.df[loser.teams.df.index,]$Rating

  if (teams.df[winner.teams.df.index,]$Game_Index == 0 
      & teams.df[winner.teams.df.index,]$Latest_Year != games[i,]$Season) {
    
    teams.df[winner.teams.df.index,]$Season_Init_Rtg <- winner.initial.rating
    teams.df[winner.teams.df.index,]$Latest_Year <- games[i,]$Season
  }else if (teams.df[winner.teams.df.index,]$Game_Index  > 0
            & teams.df[winner.teams.df.index,]$Latest_Year != games[i,]$Season) {
    
    winner.initial.rating <- teams.df[winner.teams.df.index,]$Rating * 0.66 + 1500 * 0.34
    teams.df[winner.teams.df.index,]$Season_Init_Rtg <- winner.initial.rating
    teams.df[winner.teams.df.index,]$Latest_Year <- games[i,]$Season
  }
  
  if (teams.df[loser.teams.df.index,]$Game_Index == 0
      & teams.df[loser.teams.df.index,]$Latest_Year != games[i,]$Season) {
    
    teams.df[loser.teams.df.index,]$Season_Init_Rtg <- loser.initial.rating
    teams.df[loser.teams.df.index,]$Latest_Year <- games[i,]$Season
  }else if (teams.df[loser.teams.df.index,]$Game_Index > 0
            & teams.df[loser.teams.df.index,]$Latest_Year != games[i,]$Season) {
    
    loser.initial.rating <- teams.df[loser.teams.df.index,]$Rating * 0.66 + 1500 * 0.34
    teams.df[loser.teams.df.index,]$Season_Init_Rtg <- loser.initial.rating
    teams.df[loser.teams.df.index,]$Latest_Year <- games[i,]$Season
  }
  
  winner.score <- games[i,]$Winner_Pts
  loser.score <- games[i,]$Loser_Pts

  if (winner.initial.rating > loser.initial.rating & winner.score > loser.score) {
    model.correct[i] <- TRUE
  }else if (winner.initial.rating < loser.initial.rating & winner.score > loser.score) {
    model.wrong[i] <- TRUE
  }
  
  abs.mov <- abs(winner.score - loser.score)

  winner.minus.loser <- winner.initial.rating - loser.initial.rating
  loser.minus.winner <- -winner.minus.loser

  #expected home and away win percentages
  e.winner <- 1/(1 + 10^(loser.minus.winner/400))
  e.loser <- 1/(1 + 10^(winner.minus.loser/400))

  e.winner.values[i] <- e.winner
  e.loser.values[i] <- e.loser
  
  s.winner <- 1
  s.loser <- 0

  winner.rating <- 0
  loser.rating <- 0
  
  if (winner.score == loser.score) {
    
    winner.rating <- winner.initial.rating
    loser.rating <- loser.initial.rating
    
    teams.df[winner.teams.df.index,]$Ties <- teams.df[winner.teams.df.index,]$Ties + 1
    teams.df[loser.teams.df.index,]$Ties <- teams.df[loser.teams.df.index,]$Ties + 1
  }else{
    winner.rating <- winner.initial.rating + log(abs.mov + 1) * (2.2/((winner.initial.rating - loser.initial.rating)*0.001 + 2.2)) * k.factor * (s.winner - e.winner)
    loser.rating <- loser.initial.rating + log(abs.mov + 1) * (2.2/((winner.initial.rating - loser.initial.rating)*0.001 + 2.2))* k.factor * (s.loser - e.loser)

    # winner.rating <- winner.initial.rating + k.factor * (s.winner - e.winner)
    # loser.rating <- loser.initial.rating + k.factor * (s.loser - e.loser)
    
    teams.df[winner.teams.df.index, "Wins"] <- teams.df[winner.teams.df.index, "Wins"] + 1
    teams.df[loser.teams.df.index, "Losses"] <- teams.df[loser.teams.df.index, "Losses"] + 1
  }
  
  teams.df[winner.teams.df.index, "Rating"] <- winner.rating
  teams.df[loser.teams.df.index, "Rating"] <- loser.rating

  teams.df[winner.teams.df.index, "Game_Index"] <- teams.df[winner.teams.df.index, "Game_Index"] + 1
  teams.df[loser.teams.df.index, "Game_Index"] <- teams.df[loser.teams.df.index, "Game_Index"] + 1
  
  dates.for.history[2*i - 1] <- games[i,]$Date
  dates.for.history[2*i] <- games[i,]$Date
  
  teams.for.history[2*i - 1] <- games[i,]$Winner
  teams.for.history[2*i] <- games[i,]$Loser
  
  ratings.for.history[2*i - 1] <- winner.rating
  ratings.for.history[2*i] <- loser.rating
}

games$Winner_Rating <- winner.ratings
games$Loser_Rating <- loser.ratings

games$E_Winner <- e.winner.values
games$E_Loser <- e.loser.values

games$Model_Correct <- model.correct
games$Model_Wrong <- model.wrong

teams.df$Rtg_Change <- teams.df$Rating - teams.df$Season_Init_Rtg

ratings.history <- data.frame(dates.for.history,teams.for.history,ratings.for.history)

recent.teams.df <- subset(teams.df, teams.df$Latest_Year >= (games[nrow(games),]$Season - 1))

recent.teams <- as.character(recent.teams.df$Name)

teams.df[order(-teams.df$Rating),]
teams.df[order(teams.df$Rating),]

# recent.teams.df <- subset(teams.df,teams.df$Latest_Year > 2014)

recent.teams.df[order(recent.teams.df$Rating),]
recent.teams.df <- recent.teams.df[order(-recent.teams.df$Rating),]
recent.teams.df

# fbs.teams <- rownames(recent.teams.df)
# 
# fbs.history <- rating.history[,fbs.teams]
# 
# avg.elo.all.time <- matrix(nrow = length(fbs.teams), ncol = 1)
# rownames(avg.elo.all.time) <- fbs.teams
# 
# for (j in 1:length(fbs.teams)) {
#   avg.elo.all.time[j,] <- mean(fbs.history[,j], na.rm = TRUE)
# }
# avg.elo.all.time[order(avg.elo.all.time[,1], decreasing = TRUE),]