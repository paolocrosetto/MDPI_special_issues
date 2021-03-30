## scraping the publication history of articles at MDPI

# libraries
library(tidyverse)
library(rvest)
library(purrr)
library(stringr)

# journal articles are stored on MDPI website at an url of the form
# root/ISSN/volume/issue/article
url_base <- "https://www.mdpi.com/%s/%s/%s/%s"
#
# for each volume there is one html page with the issues;
# for each issue there is one (very long) html page with all the articles.
#
# the preliminary step is to know how many volumes per journal. This is already present in the journals.csv database.

journals <- read_csv("journals.csv")

# dataset for iterating on journals
iter_volumes <- tibble(journal = rep(journals$journal, journals$volumes),
                       ISSN = rep(journals$ISSN, journals$volumes)) %>%  
  group_by(journal) %>%
  mutate(volume = seq_along(journal))

# we do not care that much about older years, so let's cut this to the last 7 issues of each journal
iter_volumes <- iter_volumes %>% 
  arrange(volume) %>% 
  group_by(journal) %>% 
  top_n(7) %>% 
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
    nissues = html_text(html_nodes(pg, "h4")) %>% str_extract("(?<=\\Iss. ).*") %>% max()
  } else {
    # old page format -- list of URLs
    nissues = html_text(html_nodes(pg, "#middle-column li")) %>% str_extract("[0-9]+?(?=\\ ())") %>% max()
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


# this takes a while, it downloads 3295 pages... and could fail at any of them!
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




# step 3: relevant info for each article [for a subset of journals]

journals %>% filter(journal == "sustainability")

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


