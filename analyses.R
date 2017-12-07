library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(scales)
library(foreach)
library(doParallel)
library(lubridate)
source('./functions.R')

################################################################################
# data cleaning
################################################################################
load('dfDetail.RData')
load('dfMerged.RData')
## remove empty data
dfDetail = dfDetail[-which(lapply(dfDetail, is.null) %>% unlist())]
dfTables = lapply(dfDetail, function(x) x$tables)

SRvec = lapply(dfDetail, function(x) x$SR) %>% unlist()
mains = lapply(dfDetail, function(x) x$main) %>% unlist()
tags = names(dfDetail)
## players basic information
dfPlayer = data.frame(btags = tags, SR = SRvec, Main = str_to_upper(mains)) %>%
  filter(!is.na(SR))
tiers = lapply(SRvec, assignTier) %>% unlist()
dfPlayer$Tier = factor(tiers, levels = c('Bronze', 'Silver', 'Gold', 'Platinum',
                                         'Diamond', 'Master', 'Grandmaster'))

## need a heroes (stats) table
heroes = c('doomfist', 'genji', 'mccree', 'pharah', 'reaper', 'soldier-76', 'sombra', 'tracer',
           'bastion', 'hanzo', 'junkrat', 'mei', 'torbjorn', 'widowmaker',
           'dva', 'orisa', 'reinhardt', 'roadhog', 'winston', 'zarya',
           'ana', 'lucio', 'mercy', 'moira', 'symmetra', 'zenyatta')
## add hero difficulties
difficulty = c(3, 3, 2, 1, 1, 1, 3, 2,
               1, 3, 2, 3, 2, 2,
               2, 2, 1, 1, 2, 3,
               3, 2, 1, 2, 2, 3)
difficulty = factor(difficulty, labels = c('Easy', 'Medium', 'Hard'))
classes = rep(c('offense', 'defense', 'tank', 'support'), c(8, 6, 6, 6))
dfHero = data.frame(hero = str_to_upper(heroes), class = str_to_title(classes), difficulty = difficulty)
dfPlayer = dfPlayer %>% left_join(dfHero, by = c('Main' = 'hero')) %>% na.omit()

## filter out players that don't have SR
dfMerged = lapply(dfMerged, function(df) {
  filter(df, Player %in% dfPlayer$btags) %>%
    mutate(hero = str_to_upper(hero))
})

## reformat objective time and time spent on fire
dfMerged$Combat = dfMerged$Combat %>% rename(`Fire Time` = `Time Spent on Fire`) %>%
  tbl_df()
idx = which(str_count(dfMerged$Combat$`Fire Time`, ':') == 1)
dfMerged$Combat$`Fire Time`[idx] = paste0('00:', dfMerged$Combat$`Fire Time`[idx])
idx = which(str_count(dfMerged$Combat$`Objective Time`, ':') == 1)
dfMerged$Combat$`Objective Time`[idx] = paste0('00:', dfMerged$Combat$`Objective Time`[idx])

## table combat
combat = dfMerged$Game %>% filter(hero == 'ALL HEROES', num(`Games Played`) > 50) %>%
  left_join(dfPlayer, by = c('Player' = 'btags')) %>% 
  filter(SR > 500) %>%
  ## inner_join(dfMerged$Combat %>% select(Player, hero, `Fire Time`, `Objective Time`),
  inner_join(dfMerged$Combat, 
             by = c('Player', 'hero')) %>%
  mutate(`Fire Time` = hms(`Fire Time`), `Objective Time` = hms(`Objective Time`)) %>%
  mutate(`Time Played` = hours(`Time Played` %>% str_replace('hours', '') %>% as.numeric()))
################################################################################
## analyses
################################################################################

## win percentage vs. SR
ggdat = dfMerged$Game %>% filter(hero == 'ALL HEROES', as.numeric(`Games Played`) > 50) %>%
  mutate(`Win Percentage` = round(as.numeric(`Games Won`) / as.numeric(`Games Played`) * 100, digits = 2)) %>%
  left_join(dfPlayer, by = c('Player' = 'btags'))
p = ggplot(ggdat, aes(SR, `Win Percentage`, color = Tier)) +
  ## geom_text(aes(label = Player))
  geom_point(alpha = 0.7)
## percentage of time spent on fire vs. SR
p = ggplot(combat, aes(Tier, `Fire Time` / `Time Played` * 100, color = Tier)) +
  geom_boxplot(width = 0.7) + ylab('Percentage of Time Spent on Fire')
plot_custom(p)
p = ggplot(combat, aes(SR, `Fire Time` / `Time Played` * 100, color = Tier)) +
  geom_point(alpha = 0.5) + geom_smooth(color = 'black') + ylab('Percentage of Time Spent on Fire')
plot_custom(p, base_size = 10)
## percentage of time spent on objective vs. SR 
p = ggplot(combat, aes(Tier, `Objective Time` / `Time Played` * 100, color = Tier)) +
  geom_boxplot(width = 0.7) + ylab('Percentage of Objective Time')
plot_custom(p)
## kill/death ratio vs. SR
p = ggplot(combat, aes(SR, num(Eliminations) / num(Deaths), color = Tier)) +
  geom_point(alpha = 0.5) + geom_smooth(color = 'black')
plot_custom(p, legend.pos = 'right')



## create a column plot of the percentage of players in each category

## histogram of SR
ggplot(dfPlayer, aes(SR)) + geom_histogram()

## scatter of most elims in game vs. SR
