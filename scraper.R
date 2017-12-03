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
  main = webHTML %>% html_node('style') %>% html_text() %>% str_extract_all('hero/[\\w-]+/career')
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

    names(allTables) = as.character(unlist(lapply(allTables, function(x) {names(x)[1]})))
    whichHero = heroName[grep(tabID, idAttr)]
    # convert tables to wide format
    allTables = lapply(allTables, function(df) {
      names(df) = c('key', 'value')
      df = df %>% spread(key = key, value = value) %>%
        mutate(Player = btag, hero = whichHero)
      return(df)
    })
    ## tables = c(tables, allTables)
    return(allTables)
  })
  tableNames = c('Hero Specific', 'Combat', 'Assists', 'Best', 'Average', 'Match Awards', 'Game', 'Miscellaneous')

  mergedTables = lapply(tableNames, function(nm) {
    largeTb = lapply(tables, function(tb) {
      if(is.null(tb[[nm]])) return(NULL)
      tmp = tb[[nm]] %>% mutate_all(as.character)
      names(tmp) = names(tmp) %>%
        str_replace_all('Final Blow\\b', 'Final Blows') %>%
        str_replace_all('Solo Kill\\b', 'Solo Kills') %>%
        str_replace_all('Environmental Kill\\b', 'Environmental Kills') %>%
        str_replace_all('Multikill\\b', 'Multikills') %>%
        str_replace_all('Death\\b', 'Deaths') %>%
        str_replace_all('Elimination\\b', 'Eliminations') %>%
        str_replace_all('Critical Hit\\b', 'Critical Hits') %>%
        str_replace_all('Generator\\b', 'Generators') %>%
        str_replace_all('Turret\\b', 'Turrets') %>%
        str_replace_all('Objective Kill\\b', 'Objective Kills') %>%
        str_replace_all('Assist\\b', 'Assists') %>%
        str_replace_all('^Game\\b', 'Games')
      return(tmp)
    })
    return(bind_rows(largeTb))
  })
  names(mergedTables) = tableNames

  return(list(SR = SR, main = main, tables = mergedTables))
}
