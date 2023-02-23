### extracting for each MDPI journal their accepted/rejected papers by year

library(tidyverse)
library(rvest)
library(jsonlite)

setwd("2023/IF and journal stats/")


## getting the list of journals
journals <- read_csv("../Special Issues/Data/journals.csv")

## function to retrieve the journal stats
getstats <- function(journal) {
  
  cat(journal)
  
  # download the page
  pg <- read_html(sprintf("https://www.mdpi.com/journal/%s/stats", journal))
                  
                  
  # split the page in a list of strings
  pg_split <-  pg %>% 
    toString() %>% 
    str_split(pattern = "\n") %>% 
    pluck(1)
                  
  #### N papers ###
  # find the line where the info is stored
  line_where_json_stored <- pg_split %>% 
    str_match("var papersYearlyElements") %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble() %>% 
    filter(!is.na(V1)) %>% 
    pull(rowname)
  
  # extract the correct line and the json within it
  json <- pg_split[as.integer(line_where_json_stored)] %>% 
    str_extract(pattern = "\\{(?:[^{}]|(\\{(?:[^{}]|)*\\}))*\\}") 
  
  Npap <- fromJSON(json) %>% 
    as_tibble() %>% 
    flatten() %>% 
    unnest(col = everything()) %>% 
    mutate(type = c("drop", "accepted", "rejected")) %>% 
    select(type, everything()) %>% 
    filter(type !="drop") %>% 
    pivot_longer(-type, names_to = "year", values_to = "N") %>% 
    filter(N != 0) %>% 
    pivot_wider(names_from = type, values_from = N) %>% 
    mutate(rejection_rate = rejected/(rejected+accepted),
           journal = journal)
  
  #### IF ###
  # find the line where the info is stored
  line_where_json_stored <- pg_split %>% 
    str_match("var ifYearlyElements") %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble() %>% 
    filter(!is.na(V1)) %>% 
    pull(rowname)
  
  # extract the correct line and the json within it
  json <- pg_split[as.integer(line_where_json_stored)] %>% 
    str_extract(pattern = "\\{(?:[^{}]|(\\{(?:[^{}]|)*\\}))*\\}") 
  
  tryCatch({
    IFstat <- fromJSON(json) %>% 
      as_tibble() %>% 
      flatten() %>% 
      unnest(col = everything()) %>% 
      mutate(type = c("IF", "citations", "citable_items")) %>% 
      select(type, everything()) %>% 
      filter(type !="drop") %>% 
      pivot_longer(-type, names_to = "year", values_to = "N") %>% 
      filter(N != 0) %>% 
      pivot_wider(names_from = type, values_from = N)
    
    Npap %>% left_join(IFstat)
  }, error = function(e){
    Npap
    })
}

## function to actually run the scrape
data <- pmap_dfr(journals %>% select(journal), getstats)

data %>% 
  select(journal, year, everything()) %>% 
  write_csv("Data/rejection_rates_and_IF.csv")