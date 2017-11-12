source('./scraper.R')
library(foreach)
library(doParallel)

dfPlayer = getTags()
save(dfPlayer, file = 'dfPlayer.RData')
## mclapply not working
## dfDetail = mclapply(dfPlayer$btag, getTable, mc.cores = cores)
## dfDetail = lapply(dfPlayer$btag, getTable)

## try out foreach in luigi
## load('./dfPlayer.RData')
cores = 26
registerDoParallel(cores = cores)
dfDetail = foreach(b = dfPlayer$btag) %dopar%
  tryCatch(getTable(b), error = function(e) {print(sprintf('Player %s data unavailable!', b)); return(NULL)})
save(dfDetail, file = 'dfDetail.RData')
