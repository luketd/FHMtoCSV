
library(tidyverse)
library(dplyr)
library(readr)

#####################
# S59=2025
#####################

seasonCurrent <- 59
yearCurrent <- 2025

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

##Read in the team_Data.csv
team_data <- read_delim("team_data.csv",
                        ";", escape_double = FALSE, col_types = cols(`Conference Id` = col_skip(),
                                                                     `Parent Team 1` = col_skip(), `Parent Team 2` = col_skip(),
                                                                     `Parent Team 3` = col_skip(), `Parent Team 4` = col_skip(),
                                                                     `Parent Team 5` = col_skip(), `Parent Team 6` = col_skip(),
                                                                     `Parent Team 7` = col_skip(), `Parent Team 8` = col_skip(),
                                                                     `Primary Color` = col_skip(), `Secondary Color` = col_skip(),
                                                                     `Text Color` = col_skip(), `LeagueId` = col_skip(), `Name` = col_skip(), `Nickname` = col_skip(), `Division Id` = col_skip()), trim_ws = TRUE)


##Read in the player_skater_career_stats_rs

player_skater_career_stats_rs <- read_delim("player_skater_career_stats_rs.csv",
                                            ";", escape_double = FALSE, col_types = cols(GR = col_skip(),
                                                                                         GvA = col_skip(), `League Id` = col_skip(),
                                                                                         TkA = col_skip()
                                                                                         ), trim_ws = TRUE)

##Read in player_goalie_Career_stats_rs

player_goalie_career_stats_rs <- read_delim("player_goalie_career_stats_rs.csv", 
                                            ";", escape_double = FALSE, col_types = cols(GR = col_skip(), 
                                                                                         `League Id` = col_skip()), trim_ws = TRUE)


TeamAcronym <- read_csv("TeamAcronym.csv")

####################################################################################################

##Filter goalie and skater by the current season

player_goalie_career_stats_rs <- filter(player_goalie_career_stats_rs, Year==yearCurrent)
player_skater_career_stats_rs <- filter(player_skater_career_stats_rs, Year==yearCurrent)


#getting player name and TOI(for skaters) into the dataframe
player_skater_rs <- merge(player_skater_career_stats_rs, player_master, by="PlayerId")
player_goalie_rs <- merge(player_goalie_career_stats_rs, player_master, by="PlayerId")

#rename TeamId to merge
names(player_goalie_rs)[names(player_goalie_rs) == "Team Id"] <- "TeamId"
names(player_goalie_rs)[names(player_goalie_rs) == "Last Name"] <- "Player Name"
names(player_goalie_rs)[names(player_goalie_rs) == "T/OL"] <- "OTL"

#getting team name into goalie df
player_goalie_rs <- merge(player_goalie_rs, team_data, by="TeamId")

###########################################################################################################
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

player_goalie_rs$Min <- player_goalie_rs$Min/60

#rearrange columns for correct output
player_goalie_rs <- player_goalie_rs[c("PlayerId", "GoalieId", "Player Name", "LeagueId", "TeamId", "SubtotalForId", "Season", "isPlayoffs"
                                       ,"GP", "W", "L","OTL", "Min", "PIM", "SO", "GA", "SA","A","ENG", "PSA", "Starts", "Backups", "PSS"
                                       )]


#####################################################################################################
#getting Skater csv ready

names(player_skater_rs)[names(player_skater_rs) == "Team Id"] <- "TeamId"
player_skater_rs <- merge(player_skater_rs, team_data, by="TeamId")
player_skater_rs <- subset(player_skater_rs, select = -c(TeamId))

names(player_skater_rs)[names(player_skater_rs) == "Team"] <- "Abbr"
player_skater_rs <- merge(player_skater_rs, TeamAcronym, by="Abbr")
names(player_skater_rs)[names(player_skater_rs) == "FO W"] <- "FOW"
names(player_skater_rs)[names(player_skater_rs) == "Last Name"] <- "Name"


player_skater_rs$LeagueId <- c(1) 
player_skater_rs$Season <- c(seasonCurrent) 
player_skater_rs$isPlayoffs <- c(0) 
player_skater_rs$SkaterId <- "" 
player_skater_rs$SubtotalForId <- ""
player_skater_rs$PenaltyMajors <- ""
player_skater_rs$HitsTaken <- ""
player_skater_rs$OwnShotsBlocked <- ""
player_skater_rs$OwnShotsMissed <- ""
player_skater_rs$PKshots <- ""
player_skater_rs$PPshots <- ""
player_skater_rs$GameTyingGoals <- ""
player_skater_rs$PPMinutes <- ""
player_skater_rs$EmptyNetGoals <- ""
player_skater_rs$HatTrickks <- ""
player_skater_rs$PenaltyShotGoals <- ""
player_skater_rs$PenaltyShotAttempts <- ""
player_skater_rs$FightsDraw <- ""


## Getting the teamId that is being used by the bojobox

player_skater_rs <- merge(player_skater_rs, TeamAcronym, by="Abbr")
player_skater_rs <- subset(player_skater_rs, select = -c(TeamId.x))
names(player_skater_rs)[names(player_skater_rs) == "TeamId.y"] <- "TeamId"

##################################################
#Renaming columns in order to get PPG and PKG, and Points

names(player_skater_rs)[names(player_skater_rs) == "PP G"] <- "PPG" 
names(player_skater_rs)[names(player_skater_rs) == "PP A"] <- "PPA"  

names(player_skater_rs)[names(player_skater_rs) == "SH G"] <- "SHG" 
names(player_skater_rs)[names(player_skater_rs) == "SH A"] <- "SHA" 

player_skater_rs$PPP <- player_skater_rs$PPG + player_skater_rs$PPA
player_skater_rs$SHP <- player_skater_rs$SHG + player_skater_rs$SHA

names(player_skater_rs)[names(player_skater_rs) == "Fights Won"] <- "FightsWon"
player_skater_rs$FightsLost <- player_skater_rs$Fights - player_skater_rs$FightsWon

player_skater_rs$P <- player_skater_rs$G + player_skater_rs$A


######################################
#Getting the TOI for all situations
player_skater_rs$MP <- player_skater_rs$TOI/60 + player_skater_rs$PPTOI/60 + player_skater_rs$SHTOI/60

player_skater_rs$PPMinutes  <- player_skater_rs$PPTOI/60

player_skater_rs$PkMinutes  <- player_skater_rs$SHTOI/60

player_skater_rs <- player_skater_rs[c("PlayerId", "SkaterId", "Name", "LeagueId", "TeamId", "SubtotalForId", "Season", "isPlayoffs", "GP", "G", "A", "P", "+/-", "PIM", "PenaltyMajors",
                                       "HIT", "HitsTaken", "SOG", "OwnShotsBlocked", "OwnShotsMissed", "SB", "MP", "PPG", "PPA", "PPP", "PPshots", "PPMinutes","SHG", "SHA", "SHP" ,"PKshots", "PkMinutes", 
                                       "GWG", "GameTyingGoals", "FO", "EmptyNetGoals", "HatTrickks", "PenaltyShotGoals", "PenaltyShotAttempts", "FightsWon", "FightsLost",  "FightsDraw", "FOW")]


#################################################################################################################
#################################################################################################################
#Getting playoff data for skaters and goalies

player_skater_career_stats_po <- read_delim("player_skater_career_stats_po.csv",
                                            ";", escape_double = FALSE, col_types = cols(GR = col_skip(),
                                                                                         GvA = col_skip(), `League Id` = col_skip(),
                                                                                         TkA = col_skip()
                                            ), trim_ws = TRUE)



player_goalie_career_stats_po <- read_delim("player_goalie_career_stats_po.csv", 
                                            ";", escape_double = FALSE, col_types = cols(GR = col_skip(), 
                                                                                         `League Id` = col_skip()), trim_ws = TRUE)

player_goalie_po <- filter(player_goalie_career_stats_po, Year==yearCurrent)
player_skater_po <- filter(player_skater_career_stats_po, Year==yearCurrent)


player_goalie_po <- merge(player_goalie_po, player_master, by="PlayerId")
player_skater_po <- merge(player_skater_po, player_master, by="PlayerId")


#rename TeamId to merge
names(player_goalie_po)[names(player_goalie_po) == "Team Id"] <- "TeamId"
names(player_goalie_po)[names(player_goalie_po) == "Last Name"] <- "Player Name"
names(player_goalie_po)[names(player_goalie_po) == "T/OL"] <- "OTL"

#getting team name into goalie df
player_goalie_po <- merge(player_goalie_po, team_data, by="TeamId")


###########################################################################################################
#rearranging playoff goalie columns for the right order to output

player_goalie_po$LeagueId <- c(1) 
player_goalie_po$Season <- c(seasonCurrent) 
player_goalie_po$isPlayoffs <- c(1)   
player_goalie_po$SubtotalForId <- ""   
player_goalie_po$GoalieId <- "" 
player_goalie_po$PIM <- ""  
player_goalie_po$A <- "" 
player_goalie_po$PSA <- "" 
player_goalie_po$Starts <- "" 
player_goalie_po$Backups <- ""  
player_goalie_po$PSS <- ""

## Getting the teamId that is being used by the bojobox

player_goalie_po <- merge(player_goalie_po, TeamAcronym, by="Abbr")
player_goalie_po <- subset(player_goalie_po, select = -c(TeamId.x))
names(player_goalie_po)[names(player_goalie_po) == "TeamId.y"] <- "TeamId"


player_goalie_po$Min <- player_goalie_po$Min/60

#rearrange columns for correct output
player_goalie_po <- player_goalie_po[c("PlayerId", "GoalieId", "Player Name", "LeagueId", "TeamId", "SubtotalForId", "Season", "isPlayoffs"
                                       ,"GP", "W", "L","OTL", "Min", "PIM", "SO", "GA", "SA","A","ENG", "PSA", "Starts", "Backups", "PSS"
)]



#####################################################################################################
#getting playoff Skater csv ready

names(player_skater_po)[names(player_skater_po) == "Team Id"] <- "TeamId"
player_skater_po <- merge(player_skater_po, team_data, by="TeamId")

player_skater_po <- subset(player_skater_po, select = -c(TeamId))

names(player_skater_po)[names(player_skater_po) == "Team"] <- "Abbr"
player_skater_po <- merge(player_skater_po, TeamAcronym, by="Abbr")
names(player_skater_po)[names(player_skater_po) == "Last Name"] <- "Name"
names(player_skater_po)[names(player_skater_po) == "FO W"] <- "FOW"
names(player_skater_po)[names(player_skater_po) == "Last Name"] <- "Name"


player_skater_po$LeagueId <- c(1) 
player_skater_po$Season <- c(seasonCurrent) 
player_skater_po$isPlayoffs <- c(1) 
player_skater_po$SkaterId <- "" 
player_skater_po$SubtotalForId <- ""
player_skater_po$PenaltyMajors <- ""
player_skater_po$HitsTaken <- ""
player_skater_po$OwnShotsBlocked <- ""
player_skater_po$OwnShotsMissed <- ""
player_skater_po$PKshots <- ""
player_skater_po$PPshots <- ""
player_skater_po$GameTyingGoals <- ""
player_skater_po$PPMinutes <- ""
player_skater_po$EmptyNetGoals <- ""
player_skater_po$HatTrickks <- ""
player_skater_po$PenaltyShotGoals <- ""
player_skater_po$PenaltyShotAttempts <- ""
player_skater_po$FightsDraw <- ""

###################################################
#Getting PPP, SHP, Points, and Fights

names(player_skater_po)[names(player_skater_po) == "PP G"] <- "PPG" 
names(player_skater_po)[names(player_skater_po) == "PP A"] <- "PPA"  

names(player_skater_po)[names(player_skater_po) == "SH G"] <- "SHG" 
names(player_skater_po)[names(player_skater_po) == "SH A"] <- "SHA" 

player_skater_po$PPP <- player_skater_po$PPG + player_skater_po$PPA
player_skater_po$SHP <- player_skater_po$SHG + player_skater_po$SHA

names(player_skater_po)[names(player_skater_po) == "Fights Won"] <- "FightsWon"
player_skater_po$FightsLost <- player_skater_po$Fights - player_skater_po$FightsWon

player_skater_po$P <- player_skater_po$G + player_skater_po$A


######################################
#Getting the TOI for all situations
player_skater_po$MP <- player_skater_po$TOI/60 + player_skater_po$PPTOI/60 + player_skater_po$SHTOI/60

player_skater_po$PPMinutes  <- player_skater_po$PPTOI/60

player_skater_po$PkMinutes  <- player_skater_po$SHTOI/60


player_skater_po <- player_skater_po[c("PlayerId", "SkaterId", "Name", "LeagueId", "TeamId", "SubtotalForId", "Season", "isPlayoffs", "GP", "G", "A", "P", "+/-", "PIM", "PenaltyMajors",
                                       "HIT", "HitsTaken", "SOG", "OwnShotsBlocked", "OwnShotsMissed", "SB", "MP", "PPG", "PPA", "PPP", "PPshots", "PPMinutes","SHG", "SHA", "SHP" ,"PKshots", "PkMinutes", 
                                       "GWG", "GameTyingGoals", "FO", "EmptyNetGoals", "HatTrickks", "PenaltyShotGoals", "PenaltyShotAttempts", "FightsWon", "FightsLost",  "FightsDraw", "FOW")]



##############################################
#exporting to CSV

SkatersCombined <- rbind(player_skater_rs, player_skater_po)
GoaliesCombined <- rbind(player_goalie_rs, player_goalie_po)


write.csv(SkatersCombined,"C:\\Users\\Luke\\Desktop\\SHL\\S59Skaters.csv")
write.csv(GoaliesCombined,"C:\\Users\\Luke\\Desktop\\SHL\\S59goalie.csv")

