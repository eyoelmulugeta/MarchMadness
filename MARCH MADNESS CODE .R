library(tidyr)
library(plyr)
library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(glmnet)
library(pROC)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggcorrplot)
library(shiny)

rm(list =ls())


setwd("~/Documents/Data 332")

df <-read.csv("~/Documents/Data 332/2023 Game Data copy.csv")
colnames(df)[3] = "TEAM"
colnames(df)[39] = "WIN"

df_1 <- df %>%
  select(SEED, TEAM,WIN ) %>%
  arrange(SEED, TEAM, WIN)

south_region <- df_1 %>%
  filter(TEAM %in% c("Alabama", "Arizona", "Baylor", "Virginia", "San Diego St.", "Creighton", "Missouri", "Maryland", "West Virginia", "Utah St.", "North Carolina St.", "College of Charleston", "Furman", "UC Santa Barbara", "Princeton", "Texas A&M Corpus Chris")) %>%
  distinct()

midwest_region <- df_1 %>%
  filter(TEAM %in% c("Houston", "Texas", "Xavier", "Indiana", "Miami FL", "Iowa St.", "Texas A&M", "Iowa", "Auburn", "Penn St.", "Pittsburgh", "Drake", "Kent St.", "Kennesaw St.", "Colgate", "Northern Kentucky")) %>%
  distinct()

west_region <- df_1 %>%
  filter(TEAM %in% c("Kansas", "UCLA", "Gonzaga", "Connecticut", "Saint Mary's", "TCU", "Northwestern", "Arkansas", "Illinois", "Boise St.", "Arizona St.", "VCU", "Iona", "Grand Canyon", "UNC Asheville", "Howard")) %>%
  distinct()

east_region <- df_1 %>%
  filter(TEAM %in% c("Purdue", "Marquette", "Kansas St.", "Tennessee", "Duke", "Kentucky", "Michigan St.", "Memphis", "Florida Atlantic", "USC", "Providence", "Oral Roberts", "Louisiana Lafayette", "Montana St.", "Vermont", "Fairleigh Dickinson")) %>%
  distinct()


# Create the 4 ggplots
plot1 <- ggplot(east_region, aes(x=TEAM, y=WIN)) + 
  geom_col(fill="#4285F4", width=0.5) +
  xlab("Team") +
  ylab("Win Percentage") +
  ggtitle("Win Percentage by Team for Eastern region")+
  geom_col() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

plot2 <- ggplot(south_region, aes(x = TEAM, y = WIN, fill = "pink")) + 
  geom_col(width = 0.5) +
  xlab("Team") +
  ylab("Win Percentage") +
  ggtitle("Win Percentage by Team for Southern Region") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_fill_manual(values = "orange")

plot3 <- ggplot(midwest_region, aes(x = TEAM, y = WIN, fill = "pink")) + 
  geom_col(width = 0.5) +
  xlab("Team") +
  ylab("Win Percentage") +
  ggtitle("Win Percentage by Team for Midwest Region") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_fill_manual(values = "blue")

plot4 <- ggplot(west_region, aes(x = TEAM, y = WIN, fill = "pink")) + 
  geom_col(width = 0.5) +
  xlab("Team") +
  ylab("Win Percentage") +
  ggtitle("Win Percentage by Team for Western region") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_fill_manual(values = "purple")

# Facet wrap the 4 ggplots using grid.arrange
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)


# Create a correlation matrix



# Melt the correlation matrix and specify ID variables
team_corr <- cor(df_1[,c("SEED", "WIN")])
team_corr_melt <- melt(team_corr, id.vars = c("SEED", "WIN"))

# Create the plot
ggplot(team_corr_melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low="blue", mid="white", high="red", 
                       midpoint=0, limit=c(-1,1), space="Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed()


# create a new dataframe for the matchups

### SOUTH REGION PREDICTION CODE 

# Create a data frame with the team names, seeds, and win percentages
teams <- data.frame(
  Team = c("Alabama", "Texas A&M Corpus Christi", "Maryland", "West Virginia",
           "San Diego State", "College of Charleston", "Virginia", "Furman",
           "Creighton", "North Carolina State", "Baylor", "UC Santa Barbara",
           "Missouri", "Utah State", "Arizona", "Princeton"),
  Seed = c(1, 16, 8, 9, 5, 12, 4, 13, 6, 11, 3, 14, 7, 10, 2, 15),
  WinPct = c(85.29, 65.52, 63.64, 57.58, 81.25, 90.91, 78.13, 77.42,
             63.64, 69.70, 68.75, 78.13, 72.73, 75.76, 82.35, 70.37)
)

# Define a function to predict the winner of a game
predict_winner <- function(team1, team2) {
  team1_row <- teams[teams$Team == team1,]
  team2_row <- teams[teams$Team == team2,]
  if (team1_row$Seed < team2_row$Seed) {
    return(team1)
  } else if (team2_row$Seed < team1_row$Seed) {
    return(team2)
  } else if (team1_row$WinPct > team2_row$WinPct) {
    return(team1)
  } else {
    return(team2)
  }
}

# Predict the winners of the games
games <- list(
  c("Alabama", "Texas A&M Corpus Christi"),
  c("Maryland", "West Virginia"),
  c("San Diego State", "College of Charleston"),
  c("Virginia", "Furman"),
  c("Creighton", "North Carolina State"),
  c("Baylor", "UC Santa Barbara"),
  c("Missouri", "Utah State"),
  c("Arizona", "Princeton")
)
winners <- sapply(games, function(x) predict_winner(x[1], x[2]))

# Print the predicted winners
cat("Predicted winners:\n")
for (i in 1:length(winners)) {
  cat(games[[i]][1], "vs", games[[i]][2], ":", winners[i], "\n")
}

# Create a data frame with the team names, seeds, and win percentages
teams <- data.frame(
  Team = c("Alabama", "San Diego State", "Baylor", "Arizona"),
  Seed = c(1, 5, 3, 2),
  WinPct = c(85.29, 81.25, 68.75, 82.35)
)

# Set up the matchups for the next round
matchups <- data.frame(
  Team1 = c("Alabama", "Baylor"),
  Team2 = c("San Diego State", "Arizona")
)

# Loop through the matchups and predict the winner
for (i in 1:nrow(matchups)) {
  # Get the team names for this matchup
  team1 <- matchups[i, "Team1"]
  team2 <- matchups[i, "Team2"]
  
  # Get the seeds and win percentages for each team
  team1_seed <- teams[teams$Team == team1, "Seed"]
  team1_pct <- teams[teams$Team == team1, "WinPct"]
  team2_seed <- teams[teams$Team == team2, "Seed"]
  team2_pct <- teams[teams$Team == team2, "WinPct"]
  
  # Calculate the expected win probabilities
  team1_prob <- team1_pct / (team1_pct + team2_pct)
  team2_prob <- team2_pct / (team1_pct + team2_pct)
  
  # Use a random number to determine the winner
  rand <- runif(1)
  if (rand < team1_prob) {
    winner <- team1
  } else {
    winner <- team2
  }
  
  # Print the predicted winner of this matchup
  cat(paste0("Winner of ", team1, " vs ", team2, ": ", winner, "\n"))
}

# Create a data frame with the team names, seeds, and win percentages
teams <- data.frame(
  Team = c("Alabama", "Maryland", "San Diego State", "Virginia", "Creighton", "Baylor", "Missouri", "Arizona"),
  Seed = c(1, 8, 5, 4, 6, 3, 7, 2),
  WinPct = c(85.29, 63.64, 81.25, 78.12, 68.75, 75.00, 72.73, 82.35)
)

# Define the matchups
matchups <- list(
  c("Alabama", "Maryland"),
  c("San Diego State", "Virginia"),
  c("Creighton", "Baylor"),
  c("Missouri", "Arizona")
)

# Loop through the matchups and determine the winner
for (i in 1:length(matchups)) {
  # Get the team names for this matchup
  team1 <- matchups[[i]][1]
  team2 <- matchups[[i]][2]
  
  # Get the seeds and win percentages for each team
  team1_seed <- teams[teams$Team == team1, "Seed"]
  team1_pct <- teams[teams$Team == team1, "WinPct"]
  team2_seed <- teams[teams$Team == team2, "Seed"]
  team2_pct <- teams[teams$Team == team2, "WinPct"]
  
  # Calculate the expected win probabilities
  team1_prob <- team1_pct / (team1_pct + team2_pct)
  team2_prob <- team2_pct / (team1_pct + team2_pct)
  
  # Use a random number to determine the winner
  rand <- runif(1)
  if (rand < team1_prob) {
    winner <- team1
  } else {
    winner <- team2
  }
  
  # Print the winner of this matchup
  cat(paste0(winner, " wins matchup ", i, "\n"))
}

# Create a data frame with the team names, seeds, and win percentages
teams <- data.frame(
  Team = c("Alabama", "San Diego State", "Baylor", "Arizona"),
  Seed = c(1, 5, 3, 2),
  WinPct = c(85.29, 81.25, 68.75, 82.35)
)

# Set up the matchups for the next round
matchups <- data.frame(
  Team1 = c("Alabama", "Baylor"),
  Team2 = c("San Diego State", "Arizona")
)

# Loop through the matchups and predict the winner
for (i in 1:nrow(matchups)) {
  # Get the team names for this matchup
  team1 <- matchups[i, "Team1"]
  team2 <- matchups[i, "Team2"]
  
  # Get the seeds and win percentages for each team
  team1_seed <- teams[teams$Team == team1, "Seed"]
  team1_pct <- teams[teams$Team == team1, "WinPct"]
  team2_seed <- teams[teams$Team == team2, "Seed"]
  team2_pct <- teams[teams$Team == team2, "WinPct"]
  
  # Calculate the expected win probabilities
  team1_prob <- team1_pct / (team1_pct + team2_pct)
  team2_prob <- team2_pct / (team1_pct + team2_pct)
  
  # Use a random number to determine the winner
  rand <- runif(1)
  if (rand < team1_prob) {
    winner <- team1
  } else {
    winner <- team2
  }
  
  # Print the predicted winner of this matchup
  cat(paste0("Winner of ", team1, " vs ", team2, ": ", winner, "\n"))
}


# Predict the final game
finalists <- c("Alabama", "Arizona")
final_winner <- predict_winner(finalists[1], finalists[2])
cat("\nPredicted final game winner:", final_winner, "\n")


### MIDWEST REGION CODE # Create a data frame with seed, team name, and win percentage
teams <- data.frame(
  seed = 1:16,
  team = c("Houston", "Texas", "Xavier", "Indiana", "Miami (FL)", "Iowa St.", "Texas A&M", "Iowa",
           "Auburn", "Penn St.", "Pittsburgh", "Drake", "Kent St.", "Kennesaw St.", "Colgate", "Northern Kentucky"),
  win = c(91.17647, 76.47059, 73.52941, 66.66667, 78.12500, 59.37500, 73.52941, 59.37500,
          62.50000, 62.85714, 66.66667, 78.78788, 81.25000, 74.19355, 75.75758, 62.50000)
)

# Define the matchups
matchups <- list(
  c("Houston", "Northern Kentucky"),
  c("Iowa", "Auburn"),
  c("Miami (FL)", "Drake"),
  c("Indiana", "Kent St."),
  c("Iowa St.", "Pittsburgh"),
  c("Xavier", "Kennesaw St."),
  c("Texas A&M", "Penn St."),
  c("Texas", "Colgate")
)

# Predict winners for each matchup
winners <- vector()
for (m in matchups) {
  team1 <- teams[teams$team == m[1], ]
  team2 <- teams[teams$team == m[2], ]
  if (team1$seed < team2$seed) {
    winners <- c(winners, team1$team)
  } else {
    winners <- c(winners, team2$team)
  }
}

# Print the predicted winners
print(winners)

# Winners of each matchup
semifinals <- c("Houston", "Miami (FL)", "Xavier", "Texas")

# Winning probabilities for each semifinalist
semifinal_probs <- data.frame(
  TEAM = semifinals,
  WIN_PROB = c(91.17647, 78.125, 73.52941, 76.47059)
)

# Function to simulate a game between two teams
simulate_game <- function(team1, team2) {
  # Find the winning percentage for each team
  win_prob1 <- semifinal_probs$WIN_PROB[semifinal_probs$TEAM == team1]
  win_prob2 <- semifinal_probs$WIN_PROB[semifinal_probs$TEAM == team2]
  
  # Simulate the outcome of the game
  if(runif(1) < win_prob1 / (win_prob1 + win_prob2)) {
    return(team1)
  } else {
    return(team2)
  }
}

# Simulate the semifinals
finalists <- c(simulate_game(semifinals[1], semifinals[2]),
               simulate_game(semifinals[3], semifinals[4]))

# Winning probabilities for each finalist
final_probs <- data.frame(
  TEAM = finalists,
  WIN_PROB = semifinal_probs$WIN_PROB[semifinal_probs$TEAM %in% finalists]
)

# Simulate the final
champion <- simulate_game(finalists[1], finalists[2])

# Print the champion
cat("The champion is:", champion)

##### WEST REGION 

# Create a data frame with the teams and their information
teams <- data.frame(
  seed = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
  name = c("Kansas", "UCLA", "Gonzaga", "Connecticut", "Saint Mary's", "TCU", "Northwestern", "Arkansas", "Illinois", "Boise St.", "Arizona St.", "VCU", "Iona", "Grand Canyon", "UNC Asheville", "Howard"),
  win_pct = c(79.41176, 85.29412, 84.37500, 75.75758, 78.12500, 63.63636, 65.62500, 60.60606, 62.50000, 71.87500, 64.70588, 79.41176, 79.41176, 65.62500, 78.12500, 61.29032)
)

# Create a function that takes two team names and returns the predicted winner based on seed and win percentage
predict_winner <- function(team1, team2) {
  # Get the information for each team
  team1_info <- teams[teams$name == team1, ]
  team2_info <- teams[teams$name == team2, ]
  
  # If either team is not found, return an error message
  if (nrow(team1_info) == 0) {
    stop(paste("Team not found:", team1))
  }
  if (nrow(team2_info) == 0) {
    stop(paste("Team not found:", team2))
  }
  
  # Determine the predicted winner based on seed and win percentage
  if (team1_info$seed < team2_info$seed ||
      (team1_info$seed == team2_info$seed && team1_info$win_pct > team2_info$win_pct)) {
    return(team1)
  } else {
    return(team2)
  }
}

# Test the function with some example matchups
predict_winner("Kansas", "Howard")
predict_winner("Arkansas", "Illinois")  
predict_winner("Saint Mary's", "VCU")  
predict_winner("Connecticut", "Iona")  
predict_winner("TCU", "Arizona St.")  
predict_winner("Gonzaga", "Grand Canyon")  
predict_winner("Northwestern", "Boise St.") 
predict_winner("UCLA", "UNC Asheville") 

# Define the matchup data
matchups <- data.frame(
  team1 = c("Kansas", "Saint Mary's", "Arizona", "Boise St."),
  team2 = c("Illinois", "UConn", "Gonzaga", "UCLA"),
  seed1 = c(1, 5, 11, 10),
  seed2 = c(9, 4, 3, 2),
  win_pct1 = c(79.41176, 78.12500, 64.70588, 71.87500),
  win_pct2 = c(62.50000, 75.75758, 84.37500, 85.29412)
)

# Predict the winners
matchups$winner <- ifelse(matchups$seed1 < matchups$seed2 & matchups$win_pct1 > matchups$win_pct2, matchups$team1, 
                          ifelse(matchups$seed1 > matchups$seed2 & matchups$win_pct1 < matchups$win_pct2, matchups$team2, 
                                 ifelse(matchups$win_pct1 > matchups$win_pct2, matchups$team1, matchups$team2)))

# Print the predicted winners
print(matchups$winner)


# Create a data frame with the provided data
team_data <- data.frame(
  seed = 1:16,
  team = c("Kansas", "UCLA", "Gonzaga", "Connecticut", "Saint Mary's",
           "TCU", "Northwestern", "Arkansas", "Illinois", "Boise St.",
           "Arizona St.", "VCU", "Iona", "Grand Canyon", "UNC Asheville", "Howard"),
  win_pct = c(79.4, 85.3, 84.4, 75.8, 78.1, 63.6, 65.6, 60.6, 62.5, 71.9,
              64.7, 79.4, 79.4, 65.6, 78.1, 61.3)
)

# Predict winner between Kansas and Saint Mary's
matchup1 <- team_data[c(1,5),]
if (matchup1[1,"win_pct"] > matchup1[2,"win_pct"]) {
  print(paste("Kansas is predicted to win against Saint Mary's"))
} else {
  print(paste("Saint Mary's is predicted to win against Kansas"))
}

# Predict winner between Gonzaga and UCLA
matchup2 <- team_data[c(2,3),]
if (matchup2[1,"win_pct"] > matchup2[2,"win_pct"]) {
  print(paste("Gonzaga is predicted to win against UCLA"))
} else {
  print(paste("UCLA is predicted to win against Gonzaga"))
}

# Predict winner between Kansas and Gonzaga (assuming they win their matchups)
final_matchup <- matchup1[1:2,]  # Use Kansas and Gonzaga from previous matchups
if (final_matchup[1,"win_pct"] > final_matchup[2,"win_pct"]) {
  print(paste("Gonzaga is predicted to win the final against Kansas"))
} else {
  print(paste("Kansas is predicted to win the final against Gonzaga"))
}

###EAST REGION 
# Create a dataframe with the provided data
# Create a dataframe from the provided data
data <- data.frame(
  SEED = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
  TEAM = c("Purdue", "Marquette", "Kansas St.", "Tennessee", "Duke", "Kentucky", "Michigan St.", "Memphis", "Florida Atlantic", "USC", "Providence", "Oral Roberts", "Louisiana Lafayette", "Montana St.", "Vermont", "Fairleigh Dickinson"),
  WIN = c(85.29412, 82.35294, 71.87500, 69.69697, 76.47059, 65.62500, 61.29032, 76.47059, 90.62500, 68.75000, 65.62500, 86.66667, 76.66667, 71.87500, 68.75000, 53.12500)
)

# Function to predict winner based on win percentage
predict_winner <- function(team1, team2) {
  if (team1$WIN > team2$WIN) {
    return(team1$TEAM)
  } else {
    return(team2$TEAM)
  }
}

# Bracket matchups
matchups <- data.frame(
  Team1 = c("Purdue", "Memphis", "Duke", "Tennessee", "Kentucky", "Kansas St.", "Michigan St.", "Marquette"),
  Team2 = c("Fairleigh Dickinson", "Florida Atlantic", "Oral Roberts", "Louisiana Lafayette", "Providence", "Montana St.", "USC", "Vermont")
)

# Predict winners for each matchup
winners <- sapply(1:nrow(matchups), function(i) predict_winner(data[data$TEAM == matchups$Team1[i], ], data[data$TEAM == matchups$Team2[i], ]))

# Display predicted winners
cat("Predicted Winners:\n")
cat(paste("Matchup", 1:nrow(matchups), ":", matchups$Team1, "vs", matchups$Team2, "=>", winners, "\n"))

# Predict winner for final matchup
final_matchup <- data.frame(
  Team1 = c(winners[1], winners[2]),
  Team2 = c(winners[3], winners[4])
)
final_winner <- predict_winner(data[data$TEAM == final_matchup$Team1[1], ], data[data$TEAM == final_matchup$Team2[1], ])
cat("\nPredicted Winner for Final Matchup: ", final_winner, "\n")

# Create a dataframe from the provided data
data <- data.frame(
  SEED = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
  TEAM = c("Purdue", "Marquette", "Kansas St.", "Tennessee", "Duke", "Kentucky", "Michigan St.", "Memphis", "Florida Atlantic", "USC", "Providence", "Oral Roberts", "Louisiana Lafayette", "Montana St.", "Vermont", "Fairleigh Dickinson"),
  WIN = c(85.29412, 82.35294, 71.87500, 69.69697, 76.47059, 65.62500, 61.29032, 76.47059, 90.62500, 68.75000, 65.62500, 86.66667, 76.66667, 71.87500, 68.75000, 53.12500)
)

# Function to predict winner based on win percentage
predict_winner <- function(team1, team2) {
  if (team1$WIN > team2$WIN) {
    return(team1$TEAM)
  } else {
    return(team2$TEAM)
  }
}

# Bracket matchups
matchups <- data.frame(
  Team1 = c("Purdue", "Oral Roberts", "Providence", "USC"),
  Team2 = c("Florida Atlantic", "Louisiana Lafayette", "Montana St.", "Marquette")
)

# Predict winners for each matchup
winners <- sapply(1:nrow(matchups), function(i) predict_winner(data[data$TEAM == matchups$Team1[i], ], data[data$TEAM == matchups$Team2[i], ]))

# Display predicted winners
cat("Predicted Winners:\n")
cat(paste("Matchup", 1:nrow(matchups), ":", matchups$Team1, "vs", matchups$Team2, "=>", winners, "\n"))

# Create a dataframe from the provided data
data <- data.frame(
  SEED = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
  TEAM = c("Purdue", "Marquette", "Kansas St.", "Tennessee", "Duke", "Kentucky", "Michigan St.", "Memphis", "Florida Atlantic", "USC", "Providence", "Oral Roberts", "Louisiana Lafayette", "Montana St.", "Vermont", "Fairleigh Dickinson"),
  WIN = c(85.29412, 82.35294, 71.87500, 69.69697, 76.47059, 65.62500, 61.29032, 76.47059, 90.62500, 68.75000, 65.62500, 86.66667, 76.66667, 71.87500, 68.75000, 53.12500)
)

# Function to predict winner based on win percentage
predict_winner <- function(team1, team2) {
  if (team1$WIN > team2$WIN) {
    return(team1$TEAM)
  } else {
    return(team2$TEAM)
  }
}

# Bracket matchups
matchup1 <- data.frame(
  Team1 = "Florida Atlantic",
  Team2 = "Oral Roberts"
)

matchup2 <- data.frame(
  Team1 = "Montana St.",
  Team2 = "Marquette"
)

# Predict winners for each matchup
winner1 <- predict_winner(data[data$TEAM == matchup1$Team1, ], data[data$TEAM == matchup1$Team2, ])
winner2 <- predict_winner(data[data$TEAM == matchup2$Team1, ], data[data$TEAM == matchup2$Team2, ])

# Display predicted winners
cat("Predicted Winners:\n")
cat("Matchup 1:", matchup1$Team1, "vs", matchup1$Team2, "=>", winner1$TEAM, "\n")
cat("Matchup 2:", matchup2$Team1, "vs", matchup2$Team2, "=>", winner2$TEAM, "\n")


# Create a dataframe from the provided data
data <- data.frame(
  SEED = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
  TEAM = c("Purdue", "Marquette", "Kansas St.", "Tennessee", "Duke", "Kentucky", "Michigan St.", "Memphis", "Florida Atlantic", "USC", "Providence", "Oral Roberts", "Louisiana Lafayette", "Montana St.", "Vermont", "Fairleigh Dickinson"),
  WIN = c(85.29412, 82.35294, 71.87500, 69.69697, 76.47059, 65.62500, 61.29032, 76.47059, 90.62500, 68.75000, 65.62500, 86.66667, 76.66667, 71.87500, 68.75000, 53.12500)
)

# Function to predict winner based on win percentage
predict_winner <- function(team1, team2) {
  ifelse(team1$WIN > team2$WIN, team1$TEAM, team2$TEAM)
}

# Bracket matchups
matchup1 <- data[data$TEAM == "Florida Atlantic" | data$TEAM == "Oral Roberts", ]
matchup2 <- data[data$TEAM == "Montana St." | data$TEAM == "Marquette", ]

# Predict winners for each matchup
winner1 <- predict_winner(matchup1[1, ], matchup1[2, ])
winner2 <- predict_winner(matchup2[1, ], matchup2[2, ])

# Display predicted winners
cat("Predicted Winners:\n")
cat("Matchup 1:", matchup1[1, ]$TEAM, "vs", matchup1[2, ]$TEAM, "=>", winner1, "\n")
cat("Matchup 2:", matchup2[1, ]$TEAM, "vs", matchup2[2, ]$TEAM, "=>", winner2, "\n")

# Create a dataframe from the provided data
data <- data.frame(
  SEED = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
  TEAM = c("Purdue", "Marquette", "Kansas St.", "Tennessee", "Duke", "Kentucky", "Michigan St.", "Memphis", "Florida Atlantic", "USC", "Providence", "Oral Roberts", "Louisiana Lafayette", "Montana St.", "Vermont", "Fairleigh Dickinson"),
  WIN = c(85.29412, 82.35294, 71.87500, 69.69697, 76.47059, 65.62500, 61.29032, 76.47059, 90.62500, 68.75000, 65.62500, 86.66667, 76.66667, 71.87500, 68.75000, 53.12500)
)

# Function to predict winner based on win percentage
predict_winner <- function(team1, team2) {
  ifelse(team1$WIN > team2$WIN, team1$TEAM, team2$TEAM)
}

# Filter teams for the matchup
matchup <- data[data$TEAM == "Florida Atlantic" | data$TEAM == "Marquette", ]

# Predict winner
winner <- predict_winner(matchup[1, ], matchup[2, ])

# Display predicted winner
cat("Predicted Winner:\n")
cat("Matchup:", matchup[1, ]$TEAM, "vs", matchup[2, ]$TEAM, "=>", winner, "\n")

###SEMI FINAL 
# Data for Houston vs Florida Atlantic
houston_seed <- 1
houston_win <- 91.17647
florida_atlantic_seed <- 9
florida_atlantic_win <- 90.62500

# Data for Gonzaga vs Alabama
gonzaga_seed <- 3
gonzaga_win <- 84.37500
alabama_seed <- 1
alabama_win <- 85.29412

# Function to calculate probability of winning based on seed and win percentage
calculate_probability <- function(seed1, win1, seed2, win2) {
  probability1 <- (seed2 / (seed1 + seed2)) * (win1 / 100)
  probability2 <- (seed1 / (seed1 + seed2)) * (win2 / 100)
  return(list(probability1 = probability1, probability2 = probability2))
}

# Calculate probabilities for Houston vs Florida Atlantic
houston_vs_florida_atlantic <- calculate_probability(houston_seed, houston_win, florida_atlantic_seed, florida_atlantic_win)

# Calculate probabilities for Gonzaga vs Alabama
gonzaga_vs_alabama <- calculate_probability(gonzaga_seed, gonzaga_win, alabama_seed, alabama_win)

# Predict winners for each bracket
if (houston_vs_florida_atlantic$probability1 + gonzaga_vs_alabama$probability1 > houston_vs_florida_atlantic$probability2 + gonzaga_vs_alabama$probability2) {
  winner1 <- "Houston"
} else {
  winner1 <- "Florida Atlantic"
}

if (houston_vs_florida_atlantic$probability1 + gonzaga_vs_alabama$probability1 > houston_vs_florida_atlantic$probability2 + gonzaga_vs_alabama$probability2) {
  winner2 <- "Gonzaga"
} else {
  winner2 <- "Alabama"
}

# Print predicted winners for each bracket
cat("Predicted winner for Houston vs Florida Atlantic: ", winner1, "\n")
cat("Predicted winner for Gonzaga vs Alabama: ", winner2, "\n")

###FInal 
# Define the seed and win percentage data for Gonzaga and Huston
gonzaga_seed <- 3
gonzaga_win_percentage <- 84.37500

huston_seed <- 1
huston_win_percentage <- 91.17647

# Predict the winner based on seed and win percentage
if (gonzaga_seed < huston_seed) {
  winner <- "Gonzaga"
} else if (gonzaga_seed > huston_seed) {
  winner <- "Huston"
} else {
  if (gonzaga_win_percentage > huston_win_percentage) {
    winner <- "Gonzaga"
  } else {
    winner <- "Huston"
  }
}

# Print the predicted winner
cat("Predicted winner between Gonzaga and Huston: ", winner)


###SHINY APP

# Define the user interface
ui <- fluidPage(
  titlePanel("Team Comparison and Prediction for Selected team"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team1", "Team 1:", choices = df$TEAM),
      selectInput("team2", "Team 2:", choices = df$TEAM),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      h4("Prediction:"),
      textOutput("result"),
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Get the selected teams from the user input
  team1 <- reactive({ df %>% filter(TEAM == input$team1) })
  team2 <- reactive({ df %>% filter(TEAM == input$team2) })
  
  # Calculate the predicted winner
  winner <- eventReactive(input$submit, {
    if (team1()$SEED < team2()$SEED) {
      return(paste0(team1()$TEAM, " is predicted to win!"))
    } else if (team1()$SEED > team2()$SEED) {
      return(paste0(team2()$TEAM, " is predicted to win!"))
    } else {
      if (team1()$WIN > team2()$WIN) {
        return(paste0(team1()$TEAM, " is predicted to win!"))
      } else {
        return(paste0(team2()$TEAM, " is predicted to win!"))
      }
    }
  })
  
  # Output the predicted winner to the UI
  output$result <- renderText({ winner() })
  
  # Create a scatter plot of the selected teams
  output$plot <- renderPlot({
    ggplot(data = rbind(team1(), team2()), aes(x = SEED, y = WIN, color = TEAM)) +
      geom_point(size = 3) +
      labs(x = "Seed", y = "Win Percentage", title = "Comparison of Selected Teams") +
      theme_bw()
  })
}

# Run the app
shinyApp(ui, server)


# Define UI for Shiny app

# Define UI ----

ui <- fluidPage(
  titlePanel("NCAA March Madness Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("region", 
                  label = "Select a region to display correlation matrix:",
                  choices = c("South", "Midwest", "West", "East"),
                  selected = "South")
    ),
    
    mainPanel(
      plotOutput("corr_plot"),
      plotOutput("prediction_plot")
    )
  )
)

# Define server ----
server <- function(input, output) {
  
  # Create reactive data frames based on user input
  region_data <- reactive({
    if(input$region == "South") {
      south_region
    } else if(input$region == "Midwest") {
      midwest_region
    } else if(input$region == "West") {
      west_region
    } else {
      east_region
    }
  })
  
  # Create correlation matrix plot
  output$corr_plot <- renderPlot({
    team_corr <- cor(df_1[,c("SEED", "WIN")])
    team_corr_melt <- melt(team_corr)
    
    ggplot(team_corr_melt, aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile() +
      scale_fill_gradient2(low="blue", mid="white", high="red", 
                           midpoint=0, limit=c(-1,1), space="Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                       size = 10, hjust = 1)) +
      coord_fixed()
  })
  
  # Create prediction plot
  # Create prediction plot
  output$prediction_plot <- renderPlot({
    ggplot(region_data(), aes(x = as.numeric(WIN), y = SEED, color = WIN, label = TEAM)) +
      geom_point(size = 3) +
      geom_text(nudge_x = 0.2, check_overlap = TRUE) +
      labs(title = "Predicted Winner by Seed and Win-Loss Record",
           x = "Win-Loss Record", y = "Seed")
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)


