
library(tidyverse)
library(dplyr)
library(readr)

#####################
# S55=2021
#####################

seasonCurrent <- 55
yearCurrent <- 2021

##Read in Player Master

player_master <- read_delim("player_master.csv", 
                            ";", escape_double = FALSE, col_types = cols(Birthcity = col_skip(), 
                                                                         Birthstate = col_skip(), `Date Of Birth` = col_skip(), 
                                                                         `First Name` = col_skip(), FranchiseId = col_skip(), 
                                                                         Height = col_skip(), Nationality_One = col_skip(), 
                                                                         Nationality_Three = col_skip(), Nationality_Two = col_skip(), 
                                                                         `Nick Name` = col_skip(), Retired = col_skip(), 
                                                                         TeamId = col_skip(), Weight = col_skip()), 
                            trim_ws = TRUE)
View(player_master)

##Read in the team_Data.csv
team_data <- read_delim("team_data.csv",
                        ";", escape_double = FALSE, col_types = cols(`Conference Id` = col_skip(),
                                                                     `Parent Team 1` = col_skip(), `Parent Team 2` = col_skip(),
                                                                     `Parent Team 3` = col_skip(), `Parent Team 4` = col_skip(),
                                                                     `Parent Team 5` = col_skip(), `Parent Team 6` = col_skip(),
                                                                     `Parent Team 7` = col_skip(), `Parent Team 8` = col_skip(),
                                                                     `Primary Color` = col_skip(), `Secondary Color` = col_skip(),
                                                                     `Text Color` = col_skip(), `LeagueId` = col_skip(), `Name` = col_skip(), `Nickname` = col_skip(), `Division Id` = col_skip()), trim_ws = TRUE)
View(team_data)


##Read in the player_skater_career_stats_rs

player_skater_career_stats_rs <- read_delim("player_skater_career_stats_rs.csv",
                                            ";", escape_double = FALSE, col_types = cols(GR = col_skip(),
                                                                                         GvA = col_skip(), `League Id` = col_skip(),
                                                                                         PPTOI = col_skip(), SHTOI = col_skip(),
                                                                                         TOI = col_skip(), TkA = col_skip()
                                                                                         ), trim_ws = TRUE)
View(player_skater_career_stats_rs)

##Read in player_goalie_Career_stats_rs

player_goalie_career_stats_rs <- read_delim("player_goalie_career_stats_rs.csv", 
                                            ";", escape_double = FALSE, col_types = cols(GR = col_skip(), 
                                                                                         `League Id` = col_skip()), trim_ws = TRUE)
View(player_goalie_career_stats_rs)


##Read in TOI
library(readr)
TOI <- read_csv("S55_TOI.csv", col_types = cols(GP = col_skip(), 
                                                Pos = col_skip(), TOI = col_skip(), TOI_min = col_skip()))
View(TOI)

######################################################################

##Filter goalie and skater by the current season

player_goalie_career_stats_rs <- filter(player_goalie_career_stats_rs, Year==yearCurrent)
player_skater_career_stats_rs <- filter(player_skater_career_stats_rs, Year==yearCurrent)


#get PId to PlayerID for merge
names(TOI)[names(TOI) == "PId"] <- "PlayerId"


#getting player name and TOI(for skaters) into the dataframe
player_skater_rs <- merge(player_skater_career_stats_rs, TOI, by="PlayerId")
player_goalie_rs <- merge(player_goalie_career_stats_rs, player_master, by="PlayerId")

#rename TeamId to merge
names(player_goalie_rs)[names(player_goalie_rs) == "Team Id"] <- "TeamId"
names(player_goalie_rs)[names(player_goalie_rs) == "Last Name"] <- "Player Name"
names(player_goalie_rs)[names(player_goalie_rs) == "T/OL"] <- "OTL"

#getting team name into goalie df
player_goalie_rs <- merge(player_goalie_rs, team_data, by="TeamId")


#####################################
#rearranging goalie columns for the right order to output

player_goalie_rs$LeagueId <- c(1) 
player_goalie_rs$Season <- c(seasonCurrent) 
player_goalie_rs$isPlayoffs <- c(0)   
player_goalie_rs$SubtotalForId <- ""   
player_goalie_rs$GoalieId <- "" 
player_goalie_rs$PIM <- ""  
player_goalie_rs$A <- "" 
player_goalie_rs$PSA <- "" 
player_goalie_rs$Starts <- "" 
player_goalie_rs$Backups <- ""  
player_goalie_rs$PSS <- ""

## Getting the teamId that is being used by the bojobox

player_goalie_rs <- merge(player_goalie_rs, TeamAcronym, by="Abbr")
player_goalie_rs <- subset(player_goalie_rs, select = -c(TeamId.x))
names(player_goalie_rs)[names(player_goalie_rs) == "TeamId.y"] <- "TeamId"

#rearrange columns for correct output
player_goalie_rs <- player_goalie_rs[c("PlayerId", "GoalieId", "Player Name", "LeagueId", "TeamId", "SubtotalForId", "Season", "isPlayoffs"
                                       ,"GP", "W", "L","OTL", "Min", "PIM", "SO", "GA", "SA","A","ENG", "PSA", "Starts", "Backups", "PSS"
                                       )]


###########################################
#getting Skater csv ready

names(player_skater_rs)[names(player_skater_rs) == "Team Id"] <- "TeamId"
player_skater_rs <- subset(player_skater_rs, select = -c(TeamId))

names(player_skater_rs)[names(player_skater_rs) == "Team"] <- "Abbr"
player_skater_rs <- merge(player_skater_rs, TeamAcronym, by="Abbr")


player_skater_rs$LeagueId <- c(1) 
player_skater_rs$Season <- c(seasonCurrent) 
player_skater_rs$isPlayoffs <- c(0) 
player_skater_rs$SkaterId <- "" 
player_skater_rs$SubtotalForId <- ""
player_skater_rs$PenaltyMajors <- ""
player_skater_rs$HitsTaken <- ""
player_skater_rs$OwnShotsBlocked <- ""
player_skater_rs$OwnShotsMissed <- ""
player_skater_rs$PkMinutes <- ""
player_skater_rs$PKshots <- ""
player_skater_rs$PPMinutes <- ""
player_skater_rs$PPshots <- ""
player_skater_rs$GameTyingGoals <- ""
player_skater_rs$PPMinutes <- ""
player_skater_rs$EmptyNetGoals <- ""
player_skater_rs$HatTrickks <- ""
player_skater_rs$PenaltyShotGoals <- ""
player_skater_rs$PenaltyShotAttempts <- ""
player_skater_rs$FightsDraw <- ""

names(player_skater_rs)[names(player_skater_rs) == "PP G"] <- "PPG" 
names(player_skater_rs)[names(player_skater_rs) == "PP A"] <- "PPA"  

names(player_skater_rs)[names(player_skater_rs) == "SH G"] <- "SHG" 
names(player_skater_rs)[names(player_skater_rs) == "SH A"] <- "SHA" 

player_skater_rs$PPP <- player_skater_rs$PPG + player_skater_rs$PPA
player_skater_rs$SHP <- player_skater_rs$SHG + player_skater_rs$SHA

names(player_skater_rs)[names(player_skater_rs) == "Fights Won"] <- "FightsWon"
player_skater_rs$FightsLost <- player_skater_rs$Fights - player_skater_rs$FightsWon

player_skater_rs$P <- player_skater_rs$G + player_skater_rs$A

player_skater_rs <- player_skater_rs[c("PlayerId", "SkaterId", "Name", "LeagueId", "TeamId", "SubtotalForId", "Season", "isPlayoffs", "GP", "G", "A", "P", "+/-", "PIM", "PenaltyMajors",
                                       "HIT", "HitsTaken", "SOG", "OwnShotsBlocked", "OwnShotsMissed", "SB", "MP", "PPG", "PPA", "PPP", "PPshots", "PPMinutes","SHG", "SHA", "SHP" ,"PKshots", "PkMinutes", 
                                       "GWG", "GameTyingGoals", "FO", "EmptyNetGoals", "HatTrickks", "PenaltyShotGoals", "PenaltyShotAttempts", "FightsWon", "FightsLost",  "FightsDraw", "FOW")]

