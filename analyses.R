source('./scraper.R')

cores = 2
dfPlayer = getTags(cores = cores)
dfDetail = mclapply(dfPlayer$btag, getTags, mc.cores = cores)
