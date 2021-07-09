library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)



skater_seasons <- read_csv("D:/FHMtoBojo/FHMtoCSV/SHL_SeasonbySeason/skater_seasons.csv")
View(skater_seasons)

names(skater_seasons)[names(skater_seasons) == "Player Name"] <- "PlayerName"

skater_seasons <- skater_seasons %>% filter(LeagueId == 1, is.na(SubtotalForId))


lundberg <- skater_seasons %>% filter(PlayerName=="Jonathan Lundberg")

lundberg <- lundberg %>% filter( is.na(SubtotalForId))


career_seasons <- skater_seasons %>% group_by(PlayerName, isPlayoffs) %>% 
  summarise(GamesPlayed = sum(GamesPlayed),Goals = sum(Goals),Assists = sum(Assists),Points = sum(Points), PlusMinus = sum(PlusMinus),
           PIM = sum(PenaltyMinutes), Hits =  sum(Hits), HitsTaken = sum(HitsTaken), Shots =  sum(Shots), SB = sum(ShotsBlocked),
           MP = sum(MinutesPlayed), AVTOI = (sum(MinutesPlayed)/sum(GamesPlayed)), PPG = sum(PPGoals), PPA = sum(PPAssists)
            , PPP = sum(PPPoints), PPS = sum(PPShots), PPM = sum(PPMinutes), PKG = sum(PKGoals), PKA = sum(PKAssists),
           PKP = sum(PKPoints), PKS = sum(PKShots), PKM = sum(PKMinutes), GWG = sum(GameWinningGoals), 
           GTG = sum(GameTyingGoals), FOT = sum(FaceoffsTotal), FOW = sum(FaceoffWins), HT = sum(HatTricks))


career_seasons<- career_seasons %>% filter(GamesPlayed>=150, MP>1000)

ggplot(data=career_seasons, aes(x=Points, y=GamesPlayed, fill=PlayerName +geom_jitter(aes(size=Points))))
