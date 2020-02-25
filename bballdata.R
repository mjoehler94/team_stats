# bballdata.R ----------------------------------------------

# packages
# library(googlesheets)
library(googledrive)
library(tidyverse)
library(scales)
library(RColorBrewer)
library(readxl)

# clear workspace
rm(list = ls())

# set directory
setwd("~/Documents/CS_Learning/R_practice/team_stats")

# source cleaning files
source("datacleaning.R")

# # read in data from google drive
# my_drive <- drive_find(pattern = "Basketball", type='spreadsheet')
# # my_drive
# 
# # read the file from google drive and write it to a local excel file
# drive_download(my_drive[1,], overwrite = TRUE)
# drive_download(my_drive[2,], overwrite = TRUE)


# initialize data frames
bball_df <- read_excel("data/Basketball 2019.xlsx", sheet = "Team Stats")
# bball_df <- read_excel("Basketball Winter 2020.xlsx", sheet = "Team Stats")
bball_df$Game <- as.factor(bball_df$Game)

bball_df

# bball_matt <- read_excel("Basketball 2019.xlsx", sheet = "Matt")
# bball_matt


# The palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# plot box stats for a single game
# get season leaders for each stat
game_box_stats <- bball_df %>%
  group_by(Game,Player) %>%
  summarise(Total_Points = sum(Points),
            Total_Assists = sum(Assists),
            Total_Rebounds = sum(Rebounds),
            Total_Steals = sum(Steals),
            Total_Blocks = sum(Blocks),
            # Total_Turnovers = sum(Turnovers)
  ) 
game_box_stats

# plot box stats for each player for a single game
game_num <- 5
dodge_text <- position_dodge(width=.75)
game_box_stats %>%
  filter(Game == game_num) %>%
  pivot_longer(cols = names(game_box_stats)[-c(1,2)],
             names_to='Skill') %>%
  ggplot(aes(x = Player, y = value, fill = Skill)) + 
  geom_col(aes(fill=Skill), position = dodge_text) + 
  geom_text(aes(y=value+1,label=ifelse(value>0,value,"")), position = dodge_text) + 
  scale_y_continuous(breaks= pretty_breaks()) + 
  scale_fill_brewer(palette="Dark2")+
  labs(title="Box Stats by Player") 


# points per game by player (bar)
ggplot(bball_df, aes(x = Player, y = Points, fill = Game)) +
  geom_bar(stat = 'identity', position = dodge_text) + 
  scale_y_continuous(breaks= pretty_breaks()) + 
  geom_text(aes(label = ifelse(Points > 0, Points, ""), y = Points + 1),
            position=dodge_text) + 
  geom_text(aes(y=-0.5, label = Game),position = dodge_text) + 
  labs(title = "Points Per Game by Player") + 
  theme_bw()

# points by player per game (box)
ggplot(bball_df, aes(x = Player, y = Points)) + 
  geom_boxplot() +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1) + 
  scale_y_continuous(breaks= pretty_breaks()) + 
  labs(title = 'Points per Game by Player')



# points per game as percentage
ggplot(bball_df, aes(x = Game, y = Points, fill = Player)) +
  geom_bar(stat = 'identity') + 
  geom_text(aes(label=ifelse(Points>0,paste(Points,"- ",Player),"")),size = 3, position = position_stack(vjust = 0.5)) + 
  labs(title = "Points Per Game by Player") +
  scale_y_continuous(breaks = pretty_breaks())

# extract column names from data frame
cols <- names(bball_df)
cols


# get season leaders for each stat
season_summary <- bball_df %>%
  group_by(Player) %>%
  summarise(Total_Points = sum(Points),
            Total_Assists = sum(Assists),
            Total_Rebounds = sum(Rebounds),
            Total_Steals = sum(Steals),
            Total_Blocks = sum(Blocks),
            Total_Turnovers = sum(Turnovers)
            ) 
season_summary

shooting_summary <- bball_df %>%
  group_by(Player) %>%
  summarise(FG_Percentage = sum(`FG Made`) * 100 / sum(`FG Attempted`),
            Total_FG_Attempts = sum(`FG Attempted`),
            Total_FG_Made = sum(`FG Made`),
            
            Three_PT_Percentage = sum(`3PT Made`) * 100 / sum(`3PT Attempted`),
            Total_3PT_Attempts = sum(`3PT Attempted`),
            Total_3PT_Made = sum(`3PT Made`),
            
            FT_Percentage = sum(`FT Made`) * 100 / sum(`FT Attempted`),
            Total_FT_Attempts = sum(`FT Attempted`),
            Total_FT_Made = sum(`FT Made`)
) 
shooting_summary

# chart the shooting for each player
individual_shooting_percentage <- shooting_summary %>%
  select(Player, contains("Percentage")) %>%
  pivot_longer(cols=c("FG_Percentage",
                        "Three_PT_Percentage",
                        "FT_Percentage"),
               names_to='Category')
individual_shooting_percentage                              

# plot of shooting percentage
# ggplot(individual_shooting_percentage, aes(x=Player, y=value, fill=Category)) + 
#   geom_bar(stat='identity', position='dodge') + 
#   labs(title='Shooting Percenatges by Category for Each Player') +
#   geom_text(aes(label=ifelse(value > 0,round(value),""), y = value + 3), position = dodge_text) 


# TODO: write a function to generate individual shooting data
# plot of shooting volume
individual_shooting_volume <- shooting_summary %>%
  select(Player, contains("Attempt"), contains('Made')) %>%
  pivot_longer(cols=c("Total_FG_Attempts",
                      "Total_FG_Made",
                      "Total_3PT_Attempts",
                      "Total_3PT_Made",
                      "Total_FT_Attempts",
                      "Total_FT_Made"),
               names_to='SubCategory')
individual_shooting_volume$Category <- rep(c('FG', '3PT', 'FT'), each = 2, times = 8)  
individual_shooting_volume$Result <- rep(c('Attempted', 'Made'), times = 24)                  
individual_shooting_volume

# plot of shooting volume by category for each player
ggplot(individual_shooting_volume,
       aes(x=Player, y=value, fill=Result)) + 
  geom_bar(stat = 'identity',position='dodge') +
  geom_text(aes(label=value, y = value + 0.6), position = dodge_text) + 
  facet_wrap(~ Category) + 
  scale_y_continuous(breaks= pretty_breaks()) + 
  labs(title='Shooting Volume by Category for Each Player') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))


# using my function
all_shots <- make_shooting_data(bball_df)

ggplot(all_shots, aes(x=Player, fill = factor(Result, level= c("Missed", "Made")))) + 
  geom_bar() +
  facet_wrap(~Shot_Type) +
  scale_y_continuous(breaks = pretty_breaks()) + 
  labs(fill = "Result", title="Shooting Accuracy by Player", y = 'Shot Count') +
  geom_text(stat='count',
            position = position_stack(vjust = 0.5),
            aes(label=..count..)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))



# make tables and charts for team performance at a game level
team_shooting_summary <- bball_df %>%
  group_by(Game) %>%
  summarise(FG_Percentage = sum(`FG Made`) * 100 / sum(`FG Attempted`),
            Total_FG_Attempts = sum(`FG Attempted`),
            
            Three_PT_Percentage = sum(`3PT Made`) * 100 / sum(`3PT Attempted`),
            Total_3PT_Attempts = sum(`3PT Attempted`),
            
            FT_Percentage = sum(`FT Made`) * 100 / sum(`FT Attempted`),
            Total_FT_Attempts = sum(`FT Attempted`)
  ) 
team_shooting_summary

