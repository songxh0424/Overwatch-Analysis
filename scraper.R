library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(rvest)
library(parallel)
library(stringr)
library(foreach)
library(doParallel)

## get a massive amount of battle tags
genurl = function(page) {
  url = sprintf("https://overwatchtracker.com/leaderboards/pc/global/CompetitiveRank?page=%d&mode=1", page)
  return(url)
}

## ## determine which server is the player on, based on SR
## whichServer = function(btag, sr) {

## }

getTags = function(pages = 1:1650, n.per.page = 10, thres = 50, cores = 2) {
  tags = lapply(pages, function(p) {
    url = genurl(p)
    nodes = p %>% genurl() %>% read_html()
    rows = nodes %>% html_nodes('tbody') %>% html_nodes('tr')
    ## remove the ad row
    if(length(rows) == 101) {
      rows = rows[-35]
    } else {
      warning(sprintf('Page %s has wrong number of rows', p))
    }
    ## keep only rows that have at least 50 games
    games = rows %>% html_nodes('.align-right') %>% html_text() %>% str_trim() %>% as.numeric()
    idx = which(games >= thres)
    ## get the battle tags
    tag = rows %>% html_nodes('a') %>% html_text()
    tag = tag[str_trim(tag) != '']
    tag = str_replace_all(tag, "#", "-")
    ## get the ranking and SR and games
    ranking = rows %>% html_node('td') %>% html_text() %>% as.numeric()
    sr = rows %>% html_node('span') %>% html_text() %>% as.numeric()
    ## keep only rows that have the same SR as the one on US server
    ## registerDoParallel(cores = cores)
    ## idx.match = foreach(i = idx) %dopar%
    ##   tryCatch({
    ##     url_us = "https://playoverwatch.com/en-gb/career/pc/us/" %>% paste0(tag[i])
    ##     webHTML = read_html(url_us)
    ##     SR = webHTML %>% html_node('.u-align-center') %>% html_text() %>% as.numeric()
    ##     SR == sr[i]
    ##   }, error = function(e) return(FALSE))
    idx.match = lapply(idx, function(i) { 
      tryCatch({
        url_us = "https://playoverwatch.com/en-gb/career/pc/us/" %>% paste0(tag[i])
        webHTML = read_html(url_us)
        SR = webHTML %>% html_node('.u-align-center') %>% html_text() %>% as.numeric()
        SR == sr[i]
      }, error = function(e) return(FALSE))
    })
    ## }, mc.cores = cores)
    idx = idx[which(idx.match %>% unlist())]
    idx = sample(idx, min(length(idx), n.per.page))
    return(data.frame(btag = tag, ranking = ranking, sr = sr, games = games)[idx,])
  }) %>% bind_rows()
  return(tags)
}
## get tables based on battle tag
getTable = function(btag) {
  url_profile = "https://playoverwatch.com/en-gb/career/pc/us/" %>% paste0(btag)
  webHTML = tryCatch({read_html(url_profile)}, error=function(err) "Error")
  # Get playmode
  innerNodes = webHTML %>% html_nodes("#competitive")
  
  # Get selectors, could be different between quick and comp
  idNodes = innerNodes %>% html_nodes("select > option")
  idAttr = idNodes %>% html_attr("value")
  heroName = idNodes %>% html_text()
  
  # table sections
  tabSection = innerNodes %>% html_nodes("div[data-group-id='stats']")
  # Loop over all tables
  tables = lapply(1:length(tabSection), function(whichTable) {
    ## Get table id
    tabID = tabSection[whichTable] %>% html_attr("data-category-id")
    tabSel = paste0("div[data-category-id=", "'", tabID, "'", "]") # select the given table
                                        # Get table information
    dataTables = innerNodes %>% html_nodes(tabSel) %>% html_nodes("table.data-table")
    allTables = dataTables %>% html_table
    ## Now, up to you how you want to store the data... Will just store as vector of data frames
    whichHero = heroName[grep(tabID, idAttr)]
                                        # just going to set the names of each data frame in the list
    names(allTables) = rep(paste0(btag, "-", whichHero, "-",as.character(unlist(lapply(allTables, function(x) { names(x)[1]})))), 1, length(allTables))
                                        # store in proper one
    ## tables = c(tables, allTables)
    return(allTables)
  })
  tables = unlist(tables, recursive = FALSE)
  names(tables) = str_replace_all(names(tables), paste0(btag, '-'), '')
  return(tables)
}
