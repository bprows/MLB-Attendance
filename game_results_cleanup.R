## Attendance data set up

all_teams <- c( "ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", "HOU", "KCR",
                "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "OAK", "PHI", "PIT", "SDP", "SEA",
                "SFG", "STL", "TBR", "TEX", "TOR", "WSN")

# all_games_both <- NULL
# for (team in all_teams) {
#   all_games_both <- rbind(all_games_both, team_results_bref(team, 2019))
# }
# 
# dim(all_games_both)
# 
# write.csv(all_games_both, "All_Games_Raw2019.csv",row.names=F)


all_games_both <- read.csv("All_Games_Raw2019.csv")

str(all_games_both)

# Fix Date to extract month of season or use game# as proxy. 
all_games_both$day_of_week <- 
  sapply(all_games_both$Date, FUN = function(x) strsplit(x,split=",")[[1]][1])

all_games_both$weekend <- ifelse(all_games_both$day_of_week %in%
                                   c("Friday","Saturday","Sunday"), 1, 0)


# Fix Record. Extract wins, loses, win%
test_records <- all_games_both$Record[1:15]
test_records
strsplit(test_records[3],split="-")[[1]][1]

all_games_both$Wins <- as.numeric(
  sapply(all_games_both$Record, FUN = function(x) strsplit(x,split="-")[[1]][1]))
all_games_both$Loses <- as.numeric(
  sapply(all_games_both$Record, FUN = function(x) strsplit(x,split="-")[[1]][2]))
all_games_both$Win_Prct <- all_games_both$Wins / 
  (all_games_both$Wins + all_games_both$Loses)

# Fix GB
table(all_games_both$GB)
all_games_both$Games_Back <- all_games_both$GB
all_games_both$Games_Back[all_games_both$GB == "Tied"] <- 0.0
all_games_both$Games_Back[nchar(all_games_both$GB) > 4] <- paste0("-",
                                                                  substr(all_games_both$Games_Back[nchar(all_games_both$GB) > 4],
                                                                         start = nchar(all_games_both$Games_Back[nchar(all_games_both$GB) > 4])-2,
                                                                         stop=nchar(all_games_both$Games_Back[nchar(all_games_both$GB) > 4])))

all_games_both$Games_Back <- as.numeric(all_games_both$Games_Back)

# Add in Interleague games
al <- c( "BAL", "BOS", "CHW",
         "CLE", "DET", "HOU", "KCR",
         "LAA", "MIN", "NYY", "OAK",
         "SEA", "TBR", "TEX", "TOR")

all_games_both$League <- ifelse(all_games_both$Tm %in% al, "AL", "NL")


# Add in division rivals
west <- c( "ARI", "COL", "HOU", "LAA", "LAD",  
           "OAK", "SDP", "SEA", "SFG", "TEX")

east <- c( "ATL", "BAL", "BOS", "MIA", "NYM", 
           "NYY", "PHI", "TBR", "TOR", "WSN")

all_games_both$Division <- ifelse(all_games_both$Tm %in% west, "West",
                                  ifelse(all_games_both$Tm %in% east, "East", "Central"))


home_games <- all_games_both[all_games_both$H_A == "H", ]
away_games <- all_games_both[all_games_both$H_A == "A", ]

head(away_games)

away_games_clean <- away_games %>%
  select(Gm, Date, Tm, Opp, R, RA, Rank, Streak, 
         cLI, Wins, Loses, Win_Prct, Games_Back, 
         League, Division)

home_games_clean <- home_games %>%
  select(Gm, Date, Tm, Opp, R, RA, Rank, Streak,
         cLI, day_of_week, weekend, Wins,
         Loses, Win_Prct, Games_Back, League, 
         Division, Attendance)

head(away_games_clean)

cleaned_games <- home_games_clean %>%
  inner_join(away_games_clean, by=c("Opp" = "Tm", "Date" = "Date"), 
             suffix=c("","_Opp"))

write.csv(cleaned_games, "cleaned_games.csv", row.names = F)
