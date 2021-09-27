#### Plotting the number of special issues
#### at MDPI journals
####

#### Paolo Crosetto
#### March 2021


#### This script imports two datasets
#### and generates a plot of SIs per year.
####
#### journals.csv       contains IF, number of articles, year founded, title and url key for each journal
#### SIs.csv            contains the special issues of each journal, by year and date

#libraries
library(tidyverse)
library(ggrepel)
library(ggbeeswarm)
library(hrbrthemes)

# getting to the right place
setwd("Special Issues/")

# import datasets
journals <- read_csv("../journals_sept21.csv")
SI <- read_csv("SIs_sept_21.csv")


# data cleaning: dates
SI <- SI %>% 
  mutate(d = dates) %>% 
  separate(d, into = c("day", "month", "year"))

# count SIs per year
SIcount <- SI %>% 
  group_by(journal, year) %>% 
  tally()

# excluding 2022 and eliminating NAs
SIcount <- SIcount %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(year)) %>% 
  filter(year <= 2021) %>% 
  filter(year >= 2016)

# merging journal and SI data
dfplot <- journals %>% left_join(SIcount)


# generating labels for the plot

dfplot <- dfplot %>% 
  filter(year >=2013) %>% 
  mutate(lab = paste(journal, n, sep = ": "))

dfplot$lab[dfplot$year != 2021] <- NA
dfplot$lab[dfplot$n<=240] <- NA

# computing number of SIs per year
dfplot <- dfplot %>% 
  group_by(year) %>% 
  mutate(N = sum(n))

# creating a label indicating the N of SIs per year
dfplot <- dfplot %>% 
  mutate(label = paste0(year, "\n(", N, ")"))

yearlabels <- dfplot %>% select(label) %>% distinct() %>% pull(label)

# plotting
p <- dfplot %>% 
  mutate(nd = cut(n, breaks = c(0,12,52,365,365*5,10000), labels = c("<1/month",">1/month",">1/week", ">1/day", ">5/day"))) %>% 
  ggplot(aes(label, n, group = journal))+
  geom_line(color = "grey70", alpha = 0.4)+
  geom_text_repel(aes(label = lab), nudge_x = 12, direction = "y", hjust = 1, segment.alpha = 0.6)+
  geom_quasirandom(aes(fill = nd), size = 4, pch = 21, color = "grey60", alpha = 0.7)+
  theme_ipsum()+
  scale_x_discrete(limits = c(yearlabels, "", ""))+
  scale_fill_brewer(name = "", palette = "RdYlGn", direction = -1)+
  scale_y_continuous(breaks = c(52, 365, 365*2, 365*3, 365*4, 365*5, 365*6, 365*7), 
                     labels = c("1 per week", "1 per day", "2 per day", 
                                "3 per day", "4 per day", "5 per day", 
                                "6 per day", "7 per day"))+
  labs(title = "Number of Special Issues at MDPI -- september 2021",
       subtitle = "85 journals with an Impact Factor",
       caption = "code @paolocrosetto -- data scraped from MDPI website",
       y = "", x = "")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom")


##saving the plot
ggsave(
  "MDPI_special_issues_2016-21_september2021.png", 
  plot = p,
  width = 9, height = 14, units = "in", dpi = 200
)


## finally, here I create a summary dataset with the info everyone wants
## number of SIs per journal per year. 
dfplot %>% 
  select(journal, year, n) %>% 
  spread(year, n, fill = 0) %>% 
  write_csv('summary_sept2021.csv')
