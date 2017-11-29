source('./scraper.R')
library(foreach)
library(doParallel)

cores = 24
## check if RData exists, if not, run getTags to get btags
if(file.exists('dfPlayer.RData')) {
  load('./dfPlayer.RData')
} else {
  btags = getTags(n.per.page = 10, cores = cores)
  save(btags, file = 'dfPlayer.RData')
}

## check if RData exists, if not, run getTable to get tables and SR 
## try out foreach in mario 
if(!file.exists('dfDetail.RData')) {
  registerDoParallel(cores = cores)
  dfDetail = foreach(btag = btags) %dopar%
    tryCatch(getTable(btag),
            error = function(e) {print(sprintf('Player %s data unavailable!', btag)); return(NULL)})
            ## finally = {print(sprintf('player %s finished', i))})
  names(dfDetail) = btags 
  save(dfDetail, file = 'dfDetail.RData')
}
