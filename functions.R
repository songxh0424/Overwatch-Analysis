## assign SR tier to players
assignTier = function(SR) {
  tier = switch(sum(SR - 500 * c(0, 3:8) >= 0),
                '1' = 'Bronze',
                '2' = 'Silver',
                '3' = 'Gold',
                '4' = 'Platinum',
                '5' = 'Diamond',
                '6' = 'Master',
                '7' = 'Grandmaster'
                )
  return(tier)
}

## convert string time to period object
toPeriod = function(str_vec) {
  out = sapply(str_vec, function(str) {
    if(is.na(str)) return(NA)
    tmp = switch(str_count(str, ':'),
           '1'= ms(str),
           '2'= hms(str)
           )
    if(is.null(tmp)) return(NA)
    return(tmp)
  })
  return(names(out))
}

## convert numbers like 1,000,000 to numeric
num = . %>% str_replace_all(',', '') %>% as.numeric()

## convert strings like '1 minute' to seconds
parseTimePlayed = function(string) {
  words = str_split(string, '[:blank:]+')[[1]]
  units = num(words[1])
  time = switch(words[2] %>% str_to_lower(),
                'hour' = ,
                'hours' = hours(units) %>% period_to_seconds(),
                'minute' = ,
                'minutes' = minutes(units) %>% period_to_seconds(),
                'second' = ,
                'seconds' = seconds(units)
                )
  if(is.null(time)) return(NA)
  return(time)
}
parseTimePlayed = parseTimePlayed %>% Vectorize()

## plotting and theming functions
theme_Publication <- function(base_size=10, legend.pos = "bottom") {
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
               ## legend.direction = "horizontal",
               ## legend.key.size= unit(0.2, "cm"),
               ## legend.margin = unit(0.1, "cm"),
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

plot_custom <- function(p, saveTo = NULL, base_size=10, legend.pos = "right") {
  out = p + theme_Publication(base_size, legend.pos) + scale_fill_Publication() + scale_colour_Publication()
  if(is.null(saveTo)) return(out)
  ggsave(saveTo, out)
  return(out)
}
