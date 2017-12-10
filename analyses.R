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
dfPlayer = dfPlayer %>% left_join(dfHero, by = c('Main' = 'hero')) %>%
  na.omit() %>% filter(SR > 500)

## filter out players that don't have SR or SR lower than 500
dfMerged = lapply(dfMerged, function(df) {
  filter(df, Player %in% dfPlayer$btags) %>%
    mutate(hero = str_to_upper(hero))
})

## reformat objective time and time spent on fire
dfMerged$Combat = dfMerged$Combat %>% rename(`Fire Time` = `Time Spent on Fire`)
idx = which(str_count(dfMerged$Combat$`Fire Time`, ':') == 1)
dfMerged$Combat$`Fire Time`[idx] = paste0('00:', dfMerged$Combat$`Fire Time`[idx])
idx = which(str_count(dfMerged$Combat$`Objective Time`, ':') == 1)
dfMerged$Combat$`Objective Time`[idx] = paste0('00:', dfMerged$Combat$`Objective Time`[idx])
dfMerged$Combat = dfMerged$Combat %>%
  mutate(`Fire Time` = hms(`Fire Time`) %>% period_to_seconds(),
         `Objective Time` = hms(`Objective Time`) %>% period_to_seconds())
dfMerged$Game = dfMerged$Game %>%
  mutate(`Time Played` = parseTimePlayed(`Time Played`))
## hold off this part until later
## dfMerged$Game = dfMerged$Game %>%
##   mutate(`Time Played` = hours(`Time Played` %>% str_replace('hours', '') %>% as.numeric()))

save(dfHero, dfPlayer, dfMerged, file = 'data_cleaned.RData')

## table combat
combat = dfMerged$Game %>% filter(hero == 'ALL HEROES', num(`Games Played`) > 50) %>%
  left_join(dfPlayer, by = c('Player' = 'btags')) %>% 
  mutate(`Win Percentage` = num(`Games Won`) / num(`Games Played`) * 100) %>%
  inner_join(dfMerged$Combat %>% filter(hero == 'ALL HEROES'), by = c('Player'))

## table of hero usage
heroUsage = dfMerged$Game %>% filter(!(hero == 'ALL HEROES')) %>% 
  mutate(hero = hero %>% str_replace('TORBJÖRN', 'TORBJORN') %>%
           str_replace("SOLDIER: 76", 'SOLDIER-76') %>%
           str_replace('D.VA', 'DVA') %>%
           str_replace("LÚCIO", 'LUCIO')) %>%
  left_join(dfPlayer %>% select(btags, SR, Tier), by = c('Player' = 'btags')) %>%
  left_join(dfHero, by = 'hero') %>%
  arrange(Player, desc(`Time Played`)) %>%
  group_by(Player) %>% mutate(rank = row_number(), totalTime = sum(`Time Played`))

## combat = dfMerged$Game %>% filter(hero == 'GENJI', num(`Games Played`) >= 10) %>%
##   left_join(dfPlayer, by = c('Player' = 'btags')) %>% 
##   ## inner_join(dfMerged$Combat %>% select(Player, hero, `Fire Time`, `Objective Time`),
##   inner_join(dfMerged$Combat %>% filter(hero == 'GENJI'), by = c('Player'))
################################################################################
## analyses
################################################################################

## histogram of SR
p = ggplot(dfPlayer, aes(SR)) +
  geom_histogram(aes(y = ..density..), fill = 'lightblue', color = 'white') +
  geom_line(stat = 'density', color = 'magenta') +
  ggtitle('Histogram of SR')
p1 = plot_custom(p)

## win percentage vs. SR
p = ggplot(combat, aes(SR, `Win Percentage`, color = Tier)) +
  ## geom_text(aes(label = Player))
  geom_point(alpha = 0.7) + geom_smooth(color = 'black')
p1 = plot_custom(p)

## percentage of time spent on fire vs. SR
p = ggplot(combat, aes(Tier, `Fire Time` / `Time Played` * 100, color = Tier)) +
  geom_boxplot(width = 0.7) + ylab('Percentage of Time Spent on Fire')
plot_custom(p)
p = ggplot(combat, aes(SR, `Fire Time` / `Time Played` * 100, color = Tier)) +
  geom_point(alpha = 0.5) + geom_smooth(color = 'black') + ylab('Percentage of Time Spent on Fire')
p2 = plot_custom(p, base_size = 10)

## percentage of time spent on objective vs. SR 
p = ggplot(combat, aes(SR, `Objective Time` / `Time Played` * 100, color = Tier)) +
  geom_point(alpha = 0.5) + ylab('Percentage of Objective Time') +
  geom_smooth(color = 'black')
plot_custom(p)
p = ggplot(combat, aes(Tier, `Objective Time` / `Time Played` * 100, color = Tier)) +
  geom_boxplot(width = 0.7) + ylab('Percentage of Objective Time')
p3 = plot_custom(p)
## kill/death ratio vs. SR
p = ggplot(combat, aes(SR, num(Eliminations) / num(Deaths), color = Tier)) +
  geom_point(alpha = 0.5) + geom_smooth(color = 'black')
p4 = plot_custom(p, legend.pos = 'right')

## similar plots can be done for solo kills and melee final blows
p = ggplot(combat, aes(SR, num(`Melee Final Blows`) / num(`Games Played`), color = Tier)) +
  geom_point(alpha = 0.5) + geom_smooth(color = 'black')
p5 = plot_custom(p, legend.pos = 'right')
p5

p = ggplot(combat, aes(SR, num(`Hero Damage Done`) / num(`Games Played`), color = Tier)) +
  geom_point(alpha = 0.5) + geom_smooth(color = 'black')
p6 = plot_custom(p, legend.pos = 'right')
p6

## create a column plot of the percentage of players in each category


## scatter of most elims in game vs. SR
