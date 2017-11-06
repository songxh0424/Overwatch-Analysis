library(readr)
library(dplyr)
library(tidyr)
library(data.table)

video = read_delim('./data/USvideos.csv', ',', trim_ws = TRUE)

api_key = 'AIzaSyBaxgANPZkRYcEeNRRYOAKd_4AUbghNsfc'

library(rvest)

# storage info
quickTables = NULL
compTables = NULL
btag = "iddqd-2884" # storage name

url = "https://playoverwatch.com/en-gb/career/pc/us/iddqd-2884"

# Obtain URL, if url found return the data, else "err message"
webHTML = tryCatch({read_html(url)}, error=function(err) "Error")

# Game category - obtained from inspection (2016-07-19)
gameCat = c("#quickplay", "#competitive")

## Loop over game cats
for (whichGC in 1:length(gameCat)) {
  # Get playmode
  innerNodes = webHTML %>% html_nodes(gameCat[1])
  
  # Get selectors, could be different between quick and comp
  idNodes = innerNodes %>% html_nodes("select > option")
  idAttr = idNodes %>% html_attr("value")
  heroName = idNodes %>% html_text()
  
  # table sections
  tabSection = innerNodes %>% html_nodes("div[data-group-id='stats']")
  # Loop over all tables
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
    if (gameCat[whichGC] == "#quick-play") {
      quickTables = c(quickTables, allTables)
    } else {
      compTables = c(compTables, allTables)
    }
  }
}
