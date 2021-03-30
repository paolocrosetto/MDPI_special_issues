## scraping the publication history of articles at MDPI

# libraries
library(tidyverse)
library(rvest)
library(purrr)
library(stringr)

# prototype on one page
pg <- read_html("https://www.mdpi.com/1996-1073/14/1/155")

pg <- read_html("https://www.mdpi.com/1996-1073/14/1/246")

journal <- html_text(html_nodes(pg, ".bib-identity em:nth-child(1)"))
  
year <- html_text(html_nodes(pg, ".bib-identity b")) %>% as.integer()

volume <-  html_text(html_nodes(pg, ".bib-identity b+ em")) %>% as.integer()
  
DOI <- html_text(html_nodes(pg, ".bib-identity a"))

history <- html_text(html_nodes(pg, ".pubhistory"))

partSI <- html_text(html_nodes(pg, ".belongsTo"))

partSI <- if_else(is_empty(partSI), "regular", "special")

h <- tibble(journal, year, volume, DOI, partSI, history)
