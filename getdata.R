source('./scraper.R')
library(foreach)
library(doParallel)

cores = 24
cores2 = 8
## check if RData exists, if not, run getTags to get btags
if(file.exists('dfPlayer.RData')) {
  load('./dfPlayer.RData')
} else {
  btags = getTags(n.per.page = 10, cores = cores)
  save(btags, file = 'dfPlayer.RData')
}

## check if RData exists, if not, run getTable to get tables and SR 
## try out foreach in mario 
if(file.exists('dfDetail.RData')) {
  load('./dfDetail.RData')
} else {
  registerDoParallel(cores = cores)
  dfDetail = foreach(btag = btags) %dopar%
    tryCatch(getTable(btag),
            error = function(e) {print(sprintf('Player %s data unavailable!', btag)); return(NULL)})
            ## finally = {print(sprintf('player %s finished', i))})
  names(dfDetail) = btags 
  save(dfDetail, file = 'dfDetail.RData')
}

if(file.exists('dfMerged.RData')) {
  load('./dfMerged.RData')
} else {
  registerDoParallel(cores = cores2)
  tableNames = c('Hero Specific', 'Combat', 'Assists', 'Best', 'Average', 'Match Awards', 'Game', 'Miscellaneous')
  dfMerged = foreach(nm = tableNames) %dopar%
    tryCatch(lapply(dfDetail, function(x) (x$tables)[[nm]]) %>% bind_rows(),
             error = function(e) {print(sprintf('Table %s not created', nm)); return(NULL)})
  names(dfMerged) = tableNames
  save(dfMerged, file = 'dfMerged.RData')
}

