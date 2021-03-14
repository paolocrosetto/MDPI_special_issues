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

# import datasets
journals <- read_csv("journals.csv")
SI <- read_csv("SIs.csv")


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
  filter(year <= 2021)

# merging journal and SI data
dfplot <- journals %>% left_join(SIcount)


# generating labels for the plot

dfplot <- dfplot %>% 
  filter(year >=2013) %>% 
  mutate(lab = paste(journal, n, sep = ": "))

dfplot$lab[dfplot$year != 2021] <- NA
dfplot$lab[dfplot$n<=120] <- NA

# plotting
p <- dfplot %>% 
  mutate(nd = cut(n, breaks = c(0,12,60,120,600,10000), labels = c("<1/month",">1/month",">5/month", ">10/month", ">50/month"))) %>% 
  ggplot(aes(year, n, group = journal))+
  geom_line(color = "grey70", alpha = 0.4)+
  geom_text_repel(aes(label = lab), nudge_x = 12, direction = "y", hjust = 1, segment.alpha = 0.6)+
  geom_quasirandom(aes(fill = nd), size = 5, pch = 21, color = "grey60", alpha = 0.7)+
  theme_ipsum()+
  scale_x_continuous(breaks = seq(2013, 2021, 1), limits = c(2012.5, 2024))+
  scale_fill_brewer(name = "", palette = "RdYlGn", direction = -1)+
  labs(title = "Number of Special Issues at MDPI",
       subtitle = "74 journals with an Impact Factor",
       caption = "code @paolocrosetto -- data scraped from MDPI website",
       y = "", x = "")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom")


##saving the plot
ggsave(
  "MDPI_special_issues_2013-21.png", 
  plot = p,
  width = 9, height = 7, units = "in", dpi = 200
)


## finally, here I create a summary dataset with the info everyone wants
## number of SIs per journal per year. 
dfplot %>% 
  select(journal, year, n) %>% 
  spread(year, n, fill = 0) %>% 
  write_csv('summary.csv')