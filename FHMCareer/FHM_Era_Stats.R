#############
#Last X Seasons


library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)



FHM_Career <- read_delim("player_skater_career_stats_rs.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)
View(FHM_Career)


FHM_Career_G <- read_delim("player_goalie_career_stats_rs.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
View(FHM_Career_G)

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




names(FHM_Career)[names(FHM_Career) == "PP G"] <- "PPG"
names(FHM_Career)[names(FHM_Career) == "PP A"] <- "PPA"
names(FHM_Career)[names(FHM_Career) == "SH G"] <- "SHG"
names(FHM_Career)[names(FHM_Career) == "SH A"] <- "SHA"
names(FHM_Career)[names(FHM_Career) == "FO W"] <- "FOW"
names(FHM_Career)[names(FHM_Career) == "Fights Won"] <- "FightsWon"
names(FHM_Career)[names(FHM_Career) == "Team Id"] <- "TeamId"

FHM_Career <- merge(FHM_Career, player_master, by="PlayerId")
FHM_Career <- merge(FHM_Career, team_data, by="TeamId")

names(FHM_Career)[names(FHM_Career) == "Last Name"] <- "Name"

#################################################################
#Adding Points, PPP, SHP into the dataframe

FHM_Career$P <- FHM_Career$G + FHM_Career$A
FHM_Career$PPP <- FHM_Career$PPG + FHM_Career$PPA
FHM_Career$SHP <- FHM_Career$SHG + FHM_Career$SHA

FHM_Org <- select(FHM_Career, "PlayerId", "Name", "Abbr" ,everything())

Scoring <- FHM_Org %>%
  group_by(Name) %>%
  summarize(sum(GP),sum(G), sum(A),sum(P), sum(PPG), sum(PPA), sum(PPG), sum(SHG), sum(SHA), sum(SHP), sum(SOG), sum(GWG), sum(sum(G)/sum(SOG)))

Defensive <- FHM_Org %>%
  group_by(Name) %>%
  summarize(sum(GP), sum(SB), sum(HIT), sum(GvA), sum(TkA), sum(FightsWon), sum(FO), sum(FOW) )

typeof(Scoring$`sum(G)`)

ggplot(data=Scoring, aes(x='sum(G)', y='sum(A)')) +geom_jitter(aes(size='sum(P)'))



###########################################################################
#Adding goalie stats
names(FHM_Career_G)[names(FHM_Career_G) == "Team Id"] <- "TeamId"
names(FHM_Career_G)[names(FHM_Career_G) == "League Id"] <- "LeagueId"
names(FHM_Career_G)[names(FHM_Career_G) == "T/OL"] <- "OTL"

FHM_Career_G <- merge(FHM_Career_G, player_master, by="PlayerId")
FHM_Career_G <- merge(FHM_Career_G, team_data, by="TeamId")

#####################################
#team stats

Team_Stats <- FHM_Org %>%
  group_by(Abbr) %>%
  summarize(sum(GP),sum(G), sum(A),sum(P), sum(PPG), sum(PPA), sum(PPG), sum(SHG), sum(SHA), sum(SHP), sum(SOG), sum(GWG), sum(sum(G)/sum(SOG)))

Team_Stats_G <- FHM_Career_G %>%
  group_by(Abbr) %>%
  summarize(sum(Min), sum(W), sum(L), sum(OTL), sum(ENG), sum(SO), sum(GA), sum(SA), sum(GR), (sum(SA) - sum(GA) ) / sum(SA))

Team_Stats_Tot <- merge(Team_Stats_G, Team_Stats, by="Abbr")

#+ scale_fill_manual(values=c("#08102c", "#164833", "#b91a2e", "#d9ad00", "#33c2ef", "#3e7098", "#eb7d0b", "#97181b", "#990000", "#006a36", "#8623ea", "#d4af37", "#00839a", "#616161", 
#"#195aa5", "#d10923"))


ggplot(data=Team_Stats_Tot, aes(x='sum(G)', y='(sum(SA)-sum(GA))/sum(SA)', fill=Abbr,color=Abbr)) +geom_jitter(aes())+ scale_color_manual(values=c("#08102c", "#164833", "#b91a2e", "#d9ad00", "#33c2ef", "#3e7098", "#eb7d0b", "#97181b", "#990000",
                                                                                                                                                   "#006a36", "#8623ea", "#d4af37", "#00839a", "#616161", "#195aa5", "#d10923"))
