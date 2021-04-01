## scraping the publication history of articles at MDPI

# libraries
library(tidyverse)
library(rvest)
library(purrr)
library(stringr)
library(lubridate)

# journal articles are stored on MDPI website at an url of the form
# root/ISSN/volume/issue/article
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


articles <- read_csv("articles_per_issue_per_volume_per_journal.csv")


# step 3: relevant info for each article [for a subset of journals]

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


trial <- iter_articles %>% 
  head(5)

scrape <- function(ISSN, volume, issue, article, ...){
  
  cat(ISSN, volume, issue, article, "\n")
  
  pg <- read_html(sprintf("https://www.mdpi.com/%s/%s/%s/%s", ISSN, volume, issue, article))
  
  journal <- html_text(html_nodes(pg, ".bib-identity em:nth-child(1)"))
  
  year <- html_text(html_nodes(pg, ".bib-identity b")) %>% as.integer()
  
  #volume <-  html_text(html_nodes(pg, ".bib-identity b+ em")) %>% as.integer()
  
  DOI <- html_text(html_nodes(pg, ".bib-identity a"))
  
  history <- html_text(html_nodes(pg, ".pubhistory"))
  
  SI <- html_text(html_nodes(pg, ".belongsTo"))
  
  SI <- if_else(is_empty(SI), 0, 1)
  
  h <- tibble(journal, year, volume, issue, DOI, SI, history)
  h
}

# wrapping the function around a "possibly" clause to catch error and move on
scrape2 <- possibly(scrape, otherwise = tibble(journal = NA, year = NA, volume = NA, issue = NA, DOI = NA, SI = NA, history = NA))





# example: actuators
iter_articles %>% 
  left_join(journals) %>% 
  filter(journal == "actuators") -> trial

df <- pmap_dfr(trial, scrape2)

actuators <- df %>% 
  filter(!is.na(journal)) %>% 
  separate(history, into = c("A_1","A_2","A_3","A_4","A_5", "A_6", "A_7", "A_8"), sep = "/") %>% 
  pivot_longer(cols = starts_with("A"), names_to = "drop", values_to = "history") %>% 
  select(-drop) %>% 
  separate(history, into = c("event", "date"), sep = ":") %>% 
  filter(!is.na(event)) %>% 
  mutate(date = dmy(date))

#share of SI vs non-SI
actuators %>% 
  select(year, DOI, SI) %>% distinct() %>% 
  group_by(year, SI, .drop = F) %>% 
  tally() %>% 
  ggplot(aes(as.factor(year), n, fill = as.factor(SI)))+
  geom_col(position = position_dodge2())

# submission to acceptance
actuators %>% 
  filter(event == "\nReceived" | event == " Published") %>% 
  select(year, DOI, SI, event, date) %>% 
  group_by(year, DOI, SI) %>% 
  pivot_wider(names_from = event, values_from = date) %>% 
  mutate(lag = ` Published` - `\nReceived`) %>% 
  group_by(year, SI) %>% 
  ggplot(aes(as.factor(year), lag, fill = as.factor(SI)))+
  geom_boxplot()

# ok now let's beef it up and do nutrients
# bug: the nuber of issues goes up to 9 only, meaning that issues 10-12 are disregarded, each year
iter_articles %>% 
  left_join(journals) %>% 
  filter(journal == "nutrients") -> trial

df <- pmap_dfr(trial, scrape2)


df %>% 
  write_csv("nutrients.csv")


nutrients <- df %>% 
  filter(!is.na(journal)) %>% 
  separate(history, into = c("A_1","A_2","A_3","A_4","A_5", "A_6", "A_7", "A_8"), sep = "/") %>% 
  pivot_longer(cols = starts_with("A"), names_to = "drop", values_to = "history") %>% 
  select(-drop) %>% 
  separate(history, into = c("event", "date"), sep = ":") %>% 
  filter(!is.na(event)) %>% 
  mutate(date = dmy(date))

nutSI <- nutrients %>% 
  filter(year != 2015) %>% 
  filter(year != 2021) %>% 
  select(year, DOI, SI) %>% distinct() %>% 
  mutate(SI = as.factor(SI),
         SI = fct_recode(SI, "Special Issue" = "1", "Normal Issue" = "0")) %>% 
  group_by(year, SI, .drop = F) %>% 
  tally() %>% 
  ggplot(aes(as.factor(year), n, fill = SI, color = SI))+
  scale_fill_brewer(name = "", palette = "Set1", direction = -1)+
  scale_color_brewer(name = "", palette = "Set1", direction = -1)+
  geom_col(position = position_dodge2())+
  #geom_line(aes(group = SI), size = 0.8)+
  theme_ipsum_ps()+
  theme(legend.position = "bottom", 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank())+
  labs(x = "", y = "Number of articles", 
       title = "Articles")

nutLAG <- nutrients %>% 
  filter(year != 2015) %>% 
  filter(year != 2021) %>% 
  filter(event == "\nReceived" | event == " Published") %>% 
  mutate(SI = as.factor(SI),
         SI = fct_recode(SI, "Special Issue" = "1", "Normal Issue" = "0")) %>% 
  select(year, DOI, SI, event, date) %>% 
  group_by(year, DOI, SI) %>% 
  pivot_wider(names_from = event, values_from = date) %>% 
  mutate(lag = ` Published` - `\nReceived`) %>% 
  filter(lag < 200) %>% 
  group_by(year, SI) %>% 
  ggplot(aes(as.factor(year), lag, color = as.factor(SI)))+
  stat_summary(position = position_dodge(width = 0.2), show.legend = F)+
  scale_fill_brewer(name = "", palette = "Set1", direction = -1)+
  scale_color_brewer(name = "", palette = "Set1", direction = -1)+
  theme_ipsum_ps()+
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank())+
  labs(x = "", y = "Days", 
       title = "Submission to acceptance")

nutSI + nutLAG + 
  plot_annotation(title = "Editorial history of MDPI:Nutrients", 
                  subtitle = "For all years, issues 1-9", 
                  caption = "data: MDPI -- code: @paolocrosetto")+
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave("Situation_at_nutrients.png", width = 12, height = 7, units = "in", dpi = 300)


# number of articles with revisions vs not
nutrev <- nutrients %>% 
  group_by(DOI) %>% 
  mutate(eventno = n()) %>% 
  mutate(norevision = eventno == 3,
         norevision = as.factor(norevision),
         norevision = fct_recode(norevision, "No revisions" = "TRUE", "Revisions" = "FALSE")) %>% 
  select(year, DOI, SI, norevision) %>% 
  distinct()
  
nutrev %>% 
  filter(year != 2015) %>% 
  filter(year != 2021) %>% 
  group_by(year, SI, norevision) %>% 
  tally() %>% 
  mutate(SI = as.factor(SI),
         SI = fct_recode(SI, "Special Issue" = "1", "Normal Issue" = "0"),
         year = as_factor(year)) %>% 
  ggplot(aes(norevision, n, fill = SI))+
  geom_col(position = position_dodge())+
  facet_wrap(~year, nrow = 1)

nutrev %>% 
  filter(year != 2015) %>% 
  filter(year != 2021) %>% 
  group_by(year, SI, norevision) %>% 
  tally() %>% 
  mutate(SI = as.factor(SI),
         SI = fct_recode(SI, "Special Issue" = "1", "Normal Issue" = "0"),
         year = as_factor(year)) %>% 
  spread(norevision, n) %>% 
  mutate(share = `No revisions`/Revisions) %>% 
  ggplot(aes(year, share, fill = SI))+
  geom_col(position = position_dodge())
