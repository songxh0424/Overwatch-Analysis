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


## get a bunch of random battle tags, ignore rankings and sr
## the data on overwatchtracker can't be trusted
getTags = function(pages = 1:1650, random = TRUE, n.per.page = 10, cores = 24) {
  registerDoParallel(cores = cores)
  ## tags = lapply(pages, function(p) {
  tags = foreach(p = pages) %dopar%
    tryCatch({
      url = genurl(p)
      nodes = p %>% genurl() %>% read_html()
      rows = nodes %>% html_nodes('tbody') %>% html_nodes('tr')
      ## remove the ad row
      if(length(rows) == 101) {
        rows = rows[-35]
      } else {
        warning(sprintf('Page %s has wrong number of rows', p))
      }
      ## get the battle tags
      tag = rows %>% html_nodes('a') %>% html_text()
      tag = tag[tag != '']
      tag = str_replace_all(tag, "#", "-")
      idx = 1:100
      if(random) {
        idx = sample(idx, min(n.per.page, length(idx)))
      }
      return(tag[idx])
    }, error = function(e) {print(sprintf('Page %s has error', p)); return(NULL)})
  ## } %>% unlist()
  tags = unlist(tags)
  return(tags)
}

getTable = function(btag) {
  url_profile = "https://playoverwatch.com/en-us/career/pc/" %>% paste0(btag)
  webHTML = read_html(url_profile)
  # get sr
  SR = webHTML %>% html_node('.u-align-center') %>% html_text() %>% as.numeric()
  ## get main
  main = webHTML %>% html_node('style') %>% html_text() %>% str_extract_all('hero/\\w+/career')
  main = main[[1]][2] %>% str_replace('hero/', '') %>% str_replace('/career', '')
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
  return(list(SR = SR, main = main, tables = tables))
}
