library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(rvest)
library(parallel)
library(stringr)

## get a massive amount of battle tags
genurl = function(page) {
  url = sprintf("https://overwatchtracker.com/leaderboards/pc/global/CompetitiveRank?page=%d&mode=1", page)
  return(url)
}

getTags = function(pages = 1:1650, random = TRUE, n.per.page = 10, cores = 2) {
  tags = mclapply(pages, function(p) {
    url = genurl(p)
    nodes = p %>% genurl() %>% read_html()
    rows = nodes %>% html_nodes('tbody') %>% html_nodes('tr')
    ## remove the ad row
    if(length(rows) == 101) {
      rows = rows[-35]
    } else {
      warning(sprintf('Page %s has wrong number of rows', p))
    }
    if(random) {
      idx = sample(1:min(100, length(rows)), n.per.page)
      rows = rows[idx]
    }
    ## get the battle tags
    tag = rows %>% html_nodes('a') %>% html_text()
    tag = tag[tag != '']
    tag = str_replace_all(tag, "#", "-")
    ## get the ranking and SR and SR
    ranking = rows %>% html_node('td') %>% html_text() %>% as.numeric()
    sr = rows %>% html_nodes('span') %>% html_text() %>% as.numeric()
    return(data.table(btag = tag, ranking = ranking, sr = sr))
  }, mc.cores = cores) %>% bind_rows() 
  return(tags)
}
## get tables based on battle tag
getTable = function(btag) {
  url = "https://playoverwatch.com/en-gb/career/pc/us/" %>% paste0(btag)
  webHTML = tryCatch({read_html(url)}, error=function(err) "Error")
  # Get playmode
  innerNodes = webHTML %>% html_nodes("#competitive")
  
  # Get selectors, could be different between quick and comp
  idNodes = innerNodes %>% html_nodes("select > option")
  idAttr = idNodes %>% html_attr("value")
  heroName = idNodes %>% html_text()
  
  # table sections
  tabSection = innerNodes %>% html_nodes("div[data-group-id='stats']")
  # Loop over all tables
  tables = list()
  for (whichTable in 1:length(tabSection)) {
    # Get table id
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
    tables = c(tables, allTables)
  }
  return(tables)
}
