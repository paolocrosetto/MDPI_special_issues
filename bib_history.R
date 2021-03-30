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

SI <- html_text(html_nodes(pg, ".belongsTo"))

SI <- if_else(is_empty(partSI), 0, 1)

h <- tibble(journal, year, volume, DOI, SI, history)



# journal articles are stored on MDPI website at an url of the form
# root/ISSN/volume/issue/article
#
# for each volume there is one html page with the issues;
# for each issue there is one (very long) html page with all the articles.
#
# the preliminary step is to know how many volumes per journal. This is already present in the journals.csv database.
# the first step is to know how many issues per volume, and add this to the journal database
# the second step is for each issue, find out how many articles there are, and add this too
# for each article, we use the code above to scrape the page for relevant info

# step 1: issues for each volume
#
# this is tricky because MDPI changed the way their volume pages work; they follow one of two formats. 
# so the code will have to take care of the possibility of two formats. 

# "new" page format (pictures in a grid)
pg <- read_html("https://www.mdpi.com/1996-1073/14")

# match the last character of the last element of the vector of Issue titles
html_text(html_nodes(pg, "h4")) %>% tail(n=1) %>% str_extract("(?<=\\Iss. ).*")


# "old" page format with a simple list of links
# if the page is "old" the code above for the "new" page returns an empty character
# which is handy to make an if 
pg <- read_html("https://www.mdpi.com/1996-1073/8")

# match the issue number in the last element of the vector of Issue titles
html_text(html_nodes(pg, "#middle-column li")) %>% 
  tail(n=1) %>% 
  str_extract("[0-9]+?(?=\\ ())")

# step 2: articles per issue
#
pg <- read_html("https://www.mdpi.com/1996-1073/13/1")

numarticle <- html_text(html_node(pg, "#exportArticles .medium-12")) %>% 
  str_extract("[0-9]+?(?=\\n)")





