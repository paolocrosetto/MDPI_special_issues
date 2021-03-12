#### Scraping the MDPI websites 
#### to get all special issues data
####

#### Paolo Crosetto
#### March 2021


#### This script generates two datasets
####
#### journals.csv       contains IF, number of articles, year founded, title and url key for each journal
#### SIs.csv            contains the special issues of each journal, by year and date

# libraries
library(tidyverse)
library(rvest)
library(purrr)
library(stringr)

#### 1. getting the list of MDPI journals ####
pg <- read_html("https://www.mdpi.com/about/journals")


# get full jo titles
title <- html_text(html_nodes(pg, ".lean div"))

# get year founded
yearfunded <- html_nodes(pg, "td.show-for-medium-up:nth-child(3)") %>% 
  html_text() %>% 
  as.numeric()

# get number of articles 
articles <- html_nodes(pg, ".show-for-large-up+ td.show-for-medium-up") %>% 
  html_text() %>% 
  as.numeric()
  

# journal short names or urls
url <- pg %>% 
  html_nodes(".journal-name-cell") %>% 
  html_nodes("a") %>% 
  html_attr("href")

# impact factor
IF <- pg %>% 
  html_nodes("td.show-for-large-up:nth-child(5)") %>% 
  html_text() %>% 
  as.numeric()

# compiling all the data into a data frame
journals <- tibble(title, url, yearfunded, articles, IF)

# data cleaning: title
journals <- journals %>% 
  separate(title, into = c("drop","title","drop2"), sep = "\n") %>% 
  select(-starts_with("drop"))

# data cleaning: journal name
journals <- journals %>% 
  separate(url, into = c("drop", "drop2", "journal"), sep = "/") %>% 
  select(-starts_with("drop"))


# filtering only journals with an IF
# !!!: this is crucial to cut the runtime of the whoe script. 
journals <- journals %>% 
  filter(!is.na(IF)) %>% 
  arrange(-articles)

# export journal dataset to csv
journals %>% write_csv("journals.csv")

#### 2. scraping to get the number of special issues per journal

# this is the general format of SI URLs on MDPI website
url_base <- "https://www.mdpi.com/journal/%s/special_issues?page_count=100&search=&section_id=0&sort=deadline&view=all&page_no=%s"

# first: find the maximum amount of pages for each journal 
# beware: this needs ~3 minutes
pages <- map_df(journals$journal, function(i){
  cat(i,'\n')
  pg <- read_html(sprintf(url_base, i,1))
  totalN <- html_nodes(pg, "#middle-column .medium-6:nth-child(1)") %>% html_text()
  npages = str_sub(totalN, -4, -2) %>% as.integer()
  p = data.frame(journal = i, 
                 pages = npages)
  p
})


# second: use this to know how much to iterate for each journal
iter <- tibble(journal = rep(pages$journal, pages$pages)) %>%  
         group_by(journal) %>%
        mutate(page = seq_along(journal))


# third: scrape the special issue page(s) for each journal and append it to the dataset
# beware: this needs ~15min
SI <- map2_df(iter$journal, iter$page, function(i,j) {
  
  # simple but effective progress indicator
  cat(i,j,"\n")
  
  pg <- read_html(sprintf(url_base, i,j))
  
  journal = i
  dates = html_nodes(pg, "#middle-column strong") %>% html_text()
  
  totalN <- html_nodes(pg, "#middle-column .medium-6:nth-child(1)") %>% html_text()
  
  if (!rlang::is_empty(dates)) {
    p = data.frame(journal = i,
               dates = html_nodes(pg, "#middle-column strong") %>% html_text(),
               stringsAsFactors=FALSE)
  } else {
    p = data.frame(journal = i,
                   dates = NA,
                   stringsAsFactors=FALSE)
  }
  
  p
  
})

# cleaning the Special Issue dataset of empty pages (there should be none, but it happens that a scraped page has 0 items in it)
SI <- SI %>% 
  filter(!is.na(dates)) %>% 
  as_tibble()

# export to csv
SI %>% write_csv("SIs.csv")