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

theme_Publication <- function(base_size=12, legend.pos = "bottom") {
      (theme_foundation(base_size=base_size)
       + theme(plot.title = element_text(face = "bold",
                                         size = rel(1.2), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.y = element_text(angle=90,vjust =2),
               axis.title.x = element_text(vjust = -0.2),
               axis.text = element_text(), 
               axis.line = element_line(colour="black"),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour="#f0f0f0"),
               panel.grid.minor = element_blank(),
               legend.key = element_rect(colour = NA),
               legend.position = legend.pos,
               legend.direction = "horizontal",
               legend.key.size= unit(0.2, "cm"),
               legend.margin = unit(0, "cm"),
               legend.title = element_text(face="italic"),
               plot.margin=unit(c(10,5,5,5),"mm"),
               strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
               strip.text = element_text(face="bold")
          ))
}

scale_fill_Publication <- function(...){
      library(scales)
      discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33", "#FFA500", "#3cb371", "#1E90FF")), ...)
}

scale_colour_Publication <- function(...){
      library(scales)
      discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33", "#FFA500", "#3cb371", "#1E90FF")), ...)
}

################################################################################
# data cleaning
################################################################################
load('dfDetail.RData')
## remove empty data
dfDetail = dfDetail[-which(lapply(dfDetail, is.null) %>% unlist())]
## reformat tables
dfDetail = lapply(dfDetail, function(x) {
  x$table = lapply(x$table, function(df) {
    ## tmp = df[2]
    ## row.names(tmp) = df[, 1]
    ## return(tmp)
    names(df) = c('key', 'value')
    df = df %>% spread(key = key, value = value)
    return(df)
  })
  return(x)
})
dfTables = lapply(dfDetail, function(x) x$table)

SRvec = lapply(dfDetail, function(x) x$SR) %>% unlist()
mains = lapply(dfDetail, function(x) x$main) %>% unlist()
tags = names(dfDetail)
## players basic information
dfPlayer = data.frame(btags = tags, SR = SRvec, Main = str_to_upper(mains)) %>%
  filter(!is.na(SR))
tiers = lapply(SRvec, assignTier) %>% unlist()
dfPlayer$Tier = tiers

## need a heroes (stats) table
heroes = c('doomfist', 'genji', 'mccree', 'pharah', 'reaper', 'soldier-76', 'sombra', 'tracer',
           'bastion', 'hanzo', 'junkrat', 'mei', 'torbjorn', 'widowmaker',
           'dva', 'orisa', 'reinhardt', 'roadhog', 'winston', 'zarya',
           'ana', 'lucio', 'mercy', 'moira', 'symmetra', 'zenyatta')
classes = rep(c('offense', 'defense', 'tank', 'support'), c(8, 6, 6, 6))
dfHero = data.frame(hero = str_to_upper(heroes), class = str_to_title(classes))
dfPlayer = dfPlayer %>% left_join(dfHero, by = c('Main' = 'hero')) %>% na.omit()











## create a column plot of the percentage of players in each category

## histogram of SR
ggplot(dfPlayer, aes(SR)) + geom_histogram()

## scatter of most elims in game vs. SR
ggdat = lapply(1:length(dfTables), function(i) {
  elims = dfTables[[i]][['ALL HEROES-Best']]['Solo Kills - Most in Game',]
  if(is.null(elims)) return(NULL)
  df.tmp = dfPlayer[i, ] %>% mutate(Elims = as.numeric(elims)) 
  return(df.tmp)
}) %>% bind_rows()
ggplot(ggdat, aes(SR, Elims, color = class)) + geom_point(alpha = 0.5)
