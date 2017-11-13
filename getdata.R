source('./scraper.R')
library(foreach)
library(doParallel)

cores = 26
dfPlayer = getTags(cores = cores)
save(dfPlayer, file = 'dfPlayer.RData')

## try out foreach in luigi
## load('./dfPlayer.RData')
registerDoParallel(cores = cores)
dfDetail = foreach(b = dfPlayer$btag) %dopar%
  tryCatch(getTable(b), error = function(e) {print(sprintf('Player %s data unavailable!', b)); return(NULL)})
save(dfDetail, file = 'dfDetail.RData')
