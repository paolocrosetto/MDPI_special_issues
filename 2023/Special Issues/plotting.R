#### Plotting the number of special issues
#### at MDPI journals
####

#### Paolo Crosetto
#### February 2023


#### pltting the results

#libraries
library(tidyverse)
library(ggrepel)
library(ggbeeswarm)
library(hrbrthemes)

# getting to the right place
setwd("2023/Special Issues/")

# import datasets
journals <- read_csv("Data/journals.csv")
SI <- read_csv("Data/SIs.csv")


# data cleaning: dates
SI <- SI %>% 
  mutate(d = dates) %>% 
  separate(d, into = c("day", "month", "year"))

# data cleaning: some problems here and there, NAs need to be removed
SI <- SI %>% 
  filter(!is.na(year))

# count SIs per year
SIcount <- SI %>% 
  group_by(journal, year) %>% 
  tally()

# excluding 2023 and eliminating NAs
SIcount <- SIcount %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(year)) %>% 
  filter(year <= 2023) %>% 
  filter(year >= 2017)

# merging journal and SI data
dfplot <- journals %>% left_join(SIcount)


# generating labels for the plot

dfplot <- dfplot %>% 
  filter(year >=2013) %>% 
  mutate(lab = paste(journal, n, sep = ": "))

dfplot$lab[dfplot$year != 2023] <- NA
dfplot$lab[dfplot$n<=365] <- NA

# computing number of SIs per year
dfplot <- dfplot %>% 
  group_by(year) %>% 
  mutate(N = sum(n))


dfplot <- dfplot %>% 
  mutate(label = paste0(year, "\n(", N, ")"))

yearlabels <- dfplot %>% select(label) %>% distinct() %>% pull(label)

## labels only for 2022
dfplot <- dfplot %>% 
  mutate(lab = if_else(year == 2023 | year == 2022, paste0(journal, " ", n), NA_character_))


# label
dfplot <- dfplot %>% 
  mutate(nd = cut(n, breaks = c(0,12,52,365,365*5,10000), labels = c("<1/month",">1/month",">1/week", ">1/day", ">5/day")))

yearlabels <- unique(dfplot$label[dfplot$year !="2023"])
# plotting
dfplot %>% 
  mutate(lab = if_else(n>365, lab, NA_character_)) %>% 
  filter(year != 2023) %>% 
  mutate(nd = cut(n, breaks = c(0,12,52,365,365*5,10000), labels = c("<1/month",">1/month",">1/week", ">1/day", ">5/day"))) %>% 
  ggplot(aes(label, n, group = journal))+
  geom_line(color = "grey70", alpha = 0.4)+
  geom_text_repel(aes(label = lab), nudge_x = 1, direction = "y", hjust = 1, segment.alpha = 0.3)+
  geom_quasirandom(aes(fill = nd), size = 4, pch = 21, color = "grey60", alpha = 0.7)+
  theme_ipsum()+
  scale_x_discrete(limits = c(yearlabels, ""))+
  scale_fill_brewer(name = "", palette = "RdYlGn", direction = -1)+
  scale_y_continuous(breaks = c(52, 365, 365*2, 365*3, 365*4, 365*5, 365*6, 365*7, 365*8, 365*9), 
                     labels = c("1 per week", "1 per day", "2 per day", 
                                "3 per day", "4 per day", "5 per day", 
                                "6 per day", "7 per day", "8 per day", "9 per day"))+
  labs(title = "Number of Special Issues at MDPI: 2017-22",
       subtitle = "98 journals with an Impact Factor",
       caption = "code @paolocrosetto -- data scraped from MDPI website",
       y = "", x = "")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        plot.title.position = "plot",
        plot.background = element_rect(color = "white", fill = "white"))


## %>% saving the plot
ggsave(
  "MDPI_special_issues_17_22.png", 
  width = 14/1.1, height = 9/1.1, units = "in", dpi = 300
)


## including 2023
yearlabels <- unique(dfplot$label)
# plotting
dfplot %>% 
  mutate(lab = if_else(n>365, lab, NA_character_)) %>% 
  mutate(lab = if_else(year == 2023, paste0(journal, " ", n), NA_character_)) %>% 
  mutate(nd = cut(n, breaks = c(0,12,52,365,365*5,10000), labels = c("<1/month",">1/month",">1/week", ">1/day", ">5/day"))) %>% 
  ggplot(aes(label, n, group = journal))+
  geom_line(color = "grey70", alpha = 0.4)+
  geom_text_repel(aes(label = lab), nudge_x = 1, direction = "y", hjust = 1, segment.alpha = 0.3)+
  geom_quasirandom(aes(fill = nd), size = 4, pch = 21, color = "grey60", alpha = 0.7)+
  theme_ipsum()+
  scale_x_discrete(limits = c(yearlabels, ""))+
  scale_fill_brewer(name = "", palette = "RdYlGn", direction = -1)+
  scale_y_continuous(breaks = c(52, 365, 365*2, 365*3, 365*4, 365*5, 365*6, 365*7, 365*8, 365*9), 
                     labels = c("1 per week", "1 per day", "2 per day", 
                                "3 per day", "4 per day", "5 per day", 
                                "6 per day", "7 per day", "8 per day", "9 per day"))+
  labs(title = "Number of Special Issues at MDPI: 2017-22",
       subtitle = "98 journals with an Impact Factor",
       caption = "code @paolocrosetto -- data scraped from MDPI website",
       y = "", x = "")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        plot.title.position = "plot",
        plot.background = element_rect(color = "white", fill = "white"))

## %>% saving the plot
ggsave(
  "MDPI_special_issues_17_23.png", 
  width = 14/1.1, height = 9/1.1, units = "in", dpi = 300
)


## finally, here I create a summary dataset with the info everyone wants
## number of SIs per journal per year. 
dfplot %>% 
  select(journal, year, n) %>% 
  spread(year, n, fill = 0) %>% 
  write_csv('summary_sept2022.csv')
