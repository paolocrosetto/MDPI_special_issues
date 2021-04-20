## scraping the publication history of articles at MDPI

# libraries
library(tidyverse)
library(rvest)
library(purrr)
library(stringr)
library(lubridate)
library(magrittr)


# journal articles are stored on MDPI website at an url of the form
# root/ISSN/volume/issue/article
#
# for each volume there is one html page with the issues;
# for each issue there is one (very long) html page with all the articles.
#
# the preliminary step is to know how many volumes per journal. This is already present in the journals.csv database.

journals <- read_csv("Special Issues/journals.csv")

# dataset for iterating on journals
iter_volumes <- tibble(journal = rep(journals$journal, journals$volumes),
                       ISSN = rep(journals$ISSN, journals$volumes)) %>%  
  group_by(journal) %>%
  mutate(volume = seq_along(journal))

# we do not care that much about older years, so let's cut this to the last 6 issues of each journal
iter_volumes <- iter_volumes %>% 
  arrange(volume) %>% 
  group_by(journal) %>% 
  top_n(6) %>% 
  arrange(journal)

# journal "separations" throws an error, so I just discard it for the time being
iter_volumes <- iter_volumes %>% 
  filter(journal != "separations")
  

# the first step is to know how many issues per volume, and add this to the journal database
# the second step is for each issue, find out how many articles there are, and add this too
# for each article, we use the code above to scrape the page for relevant info

# step 1: issues for each volume
#
# this is tricky because MDPI changed the way their volume pages work; they follow one of two formats. 
# so the code will have to take care of the possibility of two formats. 

issues <- map2_df(iter_volumes$ISSN, iter_volumes$volume, function(i,j){
  cat(i,j,'\n')
  pg <- read_html(sprintf("https://www.mdpi.com/%s/%s", i, j))
  
  if (!is_empty(html_text(html_nodes(pg, "h4")))) {
    # new page format -- grid of covers
    nissues <- html_text(html_nodes(pg, "h4")) %>% 
      str_extract("(?<=\\Iss. ).*") %>% 
      as.numeric() %>% 
      max()
  } else {
    # old page format -- list of URLs
    nissues <- html_text(html_nodes(pg, "#middle-column li")) %>% 
      str_extract("[0-9]+?(?=\\ ())") %>% 
      as.numeric() %>% 
      max()
  }
  p = data.frame(ISSN = i, 
                 volume = j,
                 nissues = nissues)
  p
})

# saving the number of issues to csv in order to save time for the next iterations
issues %>% 
  as_tibble() %>% 
  left_join(iter_volumes, by = c("ISSN", "volume")) %>% 
  select(journal, ISSN, volume, nissues) %>% 
  write_csv("issues_per_volume_per_journal.csv")


# step 2: articles per issue
#
iter_helper <- issues %>% 
  as_tibble() %>% 
  unite(isvol, ISSN, volume, sep = "__")
  
iter_issues <- tibble(isvol = rep(iter_helper$isvol, iter_helper$nissues)) %>%  
  group_by(isvol) %>%
  mutate(issue = seq_along(isvol)) %>% 
  separate(isvol, into = c("ISSN", "volume"), sep = "__")


# this takes a while, it downloads 3787 pages... and could fail at any of them!
articles <- pmap_dfr(iter_issues, function(ISSN,volume,issue,...) {
  cat(ISSN, volume, issue, "\n")
  pg <- read_html(sprintf("https://www.mdpi.com/%s/%s/%s",ISSN,volume,issue))
  numarticle <- html_text(html_node(pg, "#exportArticles .medium-12")) %>% 
    str_extract("[0-9]+?(?=\\n)")
  
  p = data.frame(ISSN = ISSN, 
                 volume = volume,
                 issue = issue,
                 narticles = numarticle)
  p
} )

articles <- articles %>% 
  as_tibble() %>% 
  mutate(across(.cols = -ISSN, .fns = as.integer)) %>% 
  left_join(iter_volumes, by = c("ISSN", "volume")) %>% 
  select(journal, everything()) 

articles %>% 
  write_csv("articles_per_issue_per_volume_per_journal.csv")


# step 3: relevant info for each article

iter_helper <- articles %>% 
  select(-journal) %>% 
  unite(iter, ISSN, volume, issue, sep = "__") %>% 
  filter(!is.na(narticles))


iter_articles <- 
  tibble(iter = rep(iter_helper$iter, iter_helper$narticles)) %>% 
  group_by(iter) %>%
  mutate(article = seq_along(iter)) %>% 
  separate(iter, into = c("ISSN", "volume", "issue"), sep = "__")
  

# PROBLEMS: 
# 1. articles up until 2014 use a different scheme for their URL, 
# using the number of page instead of the number of article. [solution: delete all instances of 2014]
iter_articles <- iter_articles %>%
  mutate(across(-ISSN, as.integer)) %>%
  group_by(ISSN) %>%
  mutate(minvol = min(volume)) %>%
  filter(volume != minvol) %>%
  select(-minvol)


# 2. article number is increasing for each volume, irrespective of the issue [solve with some clever sums]
iter_articles <- iter_articles %>%
  group_by(ISSN, volume) %>% 
  mutate(article = seq_along(issue))

# 3. merging with journals
iter_articles <- iter_articles %>% 
  left_join(journals)

# 4. cleaning off 2021
iter_articles <- iter_articles %>%
  filter(volume != volumes)

# basic scraping function
scrape <- function(ISSN, volume, issue, article, ...){
  
  cat(ISSN, volume, issue, article, "\n")
  
  pg <- read_html(sprintf("https://www.mdpi.com/%s/%s/%s/%s", ISSN, volume, issue, article))
  
  journal <- html_text(html_nodes(pg, ".bib-identity em:nth-child(1)"))
  
  year <- html_text(html_nodes(pg, ".bib-identity b")) %>% as.integer()
  
  #volume <-  html_text(html_nodes(pg, ".bib-identity b+ em")) %>% as.integer()
  
  DOI <- html_text(html_nodes(pg, ".bib-identity a"))
  
  history <- html_text(html_nodes(pg, ".pubhistory"))
  
  SI <- html_text(html_nodes(pg, ".belongsTo"))
  
  if (is_empty(SI)) {
    SI <- "Normal Issue"
  }
  
  h <- tibble(journal, year, volume, issue, DOI, SI, history)
  h
}

# wrapping the function around a "possibly" clause to catch error and move on
scrape2 <- possibly(scrape, otherwise = tibble(journal = NA, year = NA, volume = NA, issue = NA, DOI = NA, SI = NA, history = NA))


# function to scrape ONE journal and save it to csv
onejournal <- function(jo){
  
  df <- pmap_dfr(iter_articles %>% filter(journal == jo), scrape2)
  
  df <- df %>% 
    filter(!is.na(journal)) %>% 
    separate(history, into = c("A_1","A_2","A_3","A_4","A_5", "A_6", "A_7", "A_8"), sep = "/") %>% 
    pivot_longer(cols = starts_with("A"), names_to = "drop", values_to = "history") %>% 
    select(-drop) %>% 
    separate(history, into = c("event", "date"), sep = ":") %>% 
    filter(!is.na(event)) %>% 
    mutate(date = dmy(date))
  
  df %>% 
    write_csv(paste0(jo, ".csv"))
  
  df
}

iter_journals <- iter_articles %>% 
  group_by(journal) %>% 
  tally() %>% 
  arrange(n) %$% 
  journal

# run the scrape, one journal at a time -- this takes an AWFUL LOT of time
for (jo in iter_journals) {
  cat(jo, '\n')
  onejournal(jo)
}


# run the scrape via Rstudio jobs, to speed things up
# done, just waiting for it to finish

minjo <- 74
maxjo <- 74
rstudioapi::jobRunScript(path = "Editorial history/rstudio_jobs_scrape.R", importEnv = TRUE)
