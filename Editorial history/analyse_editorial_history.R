## scraping the publication history of articles at MDPI

# libraries
library(tidyverse)
library(rvest)
library(purrr)
library(stringr)
library(lubridate)
library(magrittr)
library(hrbrthemes)
library(patchwork)
library(ggridges)
library(gghalves)

# getting to the right place
setwd("Editorial history/")

# function to create a plot of special issues + of editorial lags for each journal
analyse_journal <- function(data, jo) {
  df <- data
  SI <- df %>% 
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
  
  LAG <- df %>% 
    filter(year != 2015) %>% 
    filter(year != 2021) %>% 
    filter(event == "\nReceived" | event == "Accepted") %>% 
    mutate(SI = as.factor(SI),
           SI = fct_recode(SI, "Special Issue" = "1", "Normal Issue" = "0")) %>% 
    select(year, DOI, SI, event, date) %>% 
    group_by(year, DOI, SI) %>% 
    pivot_wider(names_from = event, values_from = date) %>% 
    mutate(lag = `Accepted` - `\nReceived`) %>% 
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
  
  pl <- SI + LAG + 
    plot_annotation(title = sprintf("Editorial history of MDPI:%s",jo), 
                    caption = "data: MDPI -- code: @paolocrosetto")+
    plot_layout(guides = "collect") & theme(legend.position = "bottom")
  ggsave(sprintf("Editorial_History_%s.png",jo), plot = pl, width = 12, height = 7, units = "in", dpi = 300)
  pl
}

# loop over .csv files scraped to create individual image files
makeplot <- function(jo, ti) {
  data = read_csv(file = paste0(jo, ".csv"))
  analyse_journal(data, ti)
}

for (i in journals$rowname %>% as.integer()) {
  ti <- journals$title[i]
  jo <- journals$journal[i]
  cat(jo,", ", ti, "\n")
  try(makeplot(jo, ti))
}


## getting all the data into one data frame for joint analysis

# all the data
tbl <-
  list.files(pattern = "*.csv") %>% 
  map_df(~read_csv(.))

# data cleaning

# cleaning the event data
tbl$event[tbl$event == "\n Received"] <- "Received"
tbl$event[tbl$event == "\nReceived"] <- "Received"
tbl$event[tbl$event == "\nAccepted"] <- "Accepted"

# counting how many events
tbl <- tbl %>% 
  group_by(DOI) %>% 
  mutate(nevents = n())

# creating a "revised" dummy
tbl <- tbl %>% 
  ungroup() %>% 
  mutate(revised = if_else(nevents == 4, "Revised", "Direct accept"))


tbl <- tbl %>% 
  filter(year != 2015) %>% 
  filter(year != 2021) %>% 
  filter(event == "Received" | event == "Accepted") %>% 
  mutate(SI = as.factor(SI),
         SI = fct_recode(SI, "Special Issue" = "1", "Normal Issue" = "0"))

tbl <- tbl %>% 
  select(-volume, -issue, -nevents)


# articles by SI, over all journals
tbl %>% 
  select(year, DOI, SI) %>% distinct() %>% 
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
       title = "Articles at MDPI - 74 journals with an IF",
       caption = "data: MDPI -- code: @paolocrosetto")
ggsave("overall_articles_SI.png", width = 10, height = 8, units = "in", dpi = 300)


# analysis of LAG between submission and acceptance
tbl_large <- tbl %>%
  group_by(journal, year, DOI, SI, revised) %>% 
  pivot_wider(names_from = event, values_from = date)

tbl_lag <- tbl_large %>% 
  group_by(journal) %>% 
  mutate(lag = Accepted - Received)


#line plot with error bars
by_journal <- tbl_lag %>% 
  group_by(journal, year, SI) %>% 
  summarise(mean = mean(lag, na.rm = T),
            sd = sd(lag, na.rm = T),
            se = sd/sqrt(n()),
            cil = mean-2*se, cih = mean + 2*se)

overall <- tbl_lag %>% 
  group_by(year, SI) %>% 
  summarise(mean = mean(lag, na.rm = T),
            sd = sd(lag, na.rm = T),
            se = sd/sqrt(n()),
            cil = mean-2*se, cih = mean + 2*se)

ggplot(by_journal, aes(as.factor(year), mean, color = as.factor(SI), group = journal))+
  geom_line(color = "grey70", alpha = 0.4)+
  geom_line(inherit.aes = F, 
            data = overall, 
            aes(as.factor(year), mean, color = as.factor(SI), group = SI),
            size=1.2)+
  scale_color_brewer(name = "", palette = "Set1", direction = -1)+
  geom_point(inherit.aes = F, 
             data = overall, 
             aes(as.factor(year), mean, color = as.factor(SI), group = SI),
             size = 2)+
  theme_ipsum_ps()+
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10))+
  facet_wrap(~SI)+
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank())+
  labs(x = "", y = "Days", 
       title = "Submission to acceptance at MDPI - 74 journals with an IF",
       subtitle = "One line is a journal -- bold line is the overall mean",
       caption = "data: MDPI -- code: @paolocrosetto")
ggsave("overall_lag.png", width = 12, height = 8, units = "in", dpi = 300)


# plotting uncertainty and the distribution
# take 1: violins
p1 <- tbl_lag %>% 
  filter(lag>0 & lag<150) %>% 
  ggplot(aes(as.factor(year), lag))+
  geom_half_violin(side = "r", position = position_nudge(.15), alpha = .3, fill = "#377eb8")+
  geom_jitter(aes(color = revised), width = 0.08, height = .5, alpha = .1, size = .2)+
  theme_ipsum_ps()+
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank())+
  facet_wrap(~revised, ncol = 1)+
  labs(x = "", y = "Days", 
       title = "Submission to acceptance at MDPI - 74 journals with an IF",
       subtitle = "Lag distribution",
       caption = "data: MDPI -- code: @paolocrosetto")
ggsave(plot = p1, "lag_distro.png", width = 12, height = 9, units = "in", dpi = 300)

## take 2: ridges
selected_jo <- tbl_lag %>% 
  filter(year == 2016) %>% 
  group_by(journal) %>% 
  mutate(nbyjour = n()) %>% 
  filter(nbyjour>50) %>% 
  select(journal) %>% 
  distinct() %$% 
  journal


tbl_lag %>% 
  filter(year == 2016 | year == 2020) %>% 
  filter(journal %in% selected_jo) %>% 
  filter(lag<150) %>% 
  mutate(year = as.factor(year), 
         fct_relevel(year, "2020")) %>% 
  ggplot(aes(lag, reorder(journal, lag), fill = reorder(year, lag)))+
  geom_density_ridges(color = "white", alpha = .9)+
  scale_fill_brewer(palette = "Set1", direction = 1, name = "")+
  facet_grid(.~year, shrink = T, scales = "free", space = "free")+
  theme_ipsum_rc()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none",
        plot.title.position = "plot")+
  labs(title = "Lag from submission to acceptance at top MDPI Journals", 
       x = "Days", y = "",
       caption = "data: MDPI -- code: @paolocrosetto")
ggsave("lag_ridge.png", width = 12, height = 12, units = "in", dpi = 300)

# take 3: heatmap
# heatmap of lag by journal
tbl_lag %>% 
  filter(year == 2016 | year == 2020) %>% 
  filter(journal %in% selected_jo) %>% 
  group_by(journal, year) %>% 
  mutate(lag_d = cut(as.integer(lag), 
                     breaks = c(-0.5, 0.5, 10.5, 20.5, 30.5, 40.5, 50.5,60.5, 71.5, 81.5, 91.5, 1000),
                     labels = c("0", "1-10", "11-20", "21-30", "31-40", "41-50", 
                                "51-60", "61-70", "71-80", "81-90", ">90"))) %>% 
  group_by(year, journal, lag_d) %>% 
  tally() %>% 
  mutate(share = 100*n/sum(n), 
         share = round(share, 1)) %>% 
  filter(lag_d != "NA") %>% 
  ggplot(aes(lag_d, journal, fill = share))+
  geom_tile(color = "white")+
  scale_fill_gradient(low = "white", high = "tomato")+
  theme_ipsum_rc()+
  facet_wrap(~year, nrow = 1)+
  theme(panel.grid.major = element_blank(), plot.title.position = "plot")+
  labs(title = "Lag from submission to acceptance at top MDPI Journals", 
       x = "Days", y = "",
       caption = "data: MDPI -- code: @paolocrosetto")
ggsave("lag_heatmap.png", width = 15, height = 12, units = "in", dpi = 300)

# individual distribution by journal
lag_journal <- function(jo){
  tbl_lag %>% 
    filter(lag>0 & lag<150) %>% 
    filter(journal == jo) %>% 
    ggplot(aes(as.factor(year), lag))+
    geom_half_violin(side = "r", position = position_nudge(.15), alpha = .3, fill = "#377eb8")+
    geom_jitter(width = 0.08, height = .5, alpha = .1, size = .8)+
    scale_y_continuous(breaks = seq(0,150,25))+
    theme_ipsum_ps()+
    theme(legend.position = "none", 
          panel.grid.minor = element_blank(), 
          panel.grid.major.x = element_blank())+
    geom_text(data = tbl_lag %>% 
                filter(lag>0 & lag<150) %>% 
                filter(journal == jo) %>% 
                group_by(year) %>% summarise(N = n()) %>% mutate(N = paste("N = ", N, sep = "")), 
              inherit.aes = F,
              aes(as.factor(year), 155, label = N), 
              hjust = 0.4)+
    labs(x = "", y = "Days", 
         title = sprintf("Submission to acceptance at MDPI:%s",jo),
         subtitle = "Each point is a paper",
         caption = "data: MDPI -- code: @paolocrosetto")
  ggsave(sprintf("lag_distro_%s.png",jo), width = 16, height = 9, units = "in", dpi = 300)
}

jo <- tbl %>% 
  select(journal) %>% 
  distinct() %$% 
  journal

for (i in jo) {
  cat(i, "\n")
  try(lag_journal(i))
}

## overall table with data (2020 only)
shares <- tbl_lag %>% 
  filter(year == 2016) %>% 
  group_by(journal) %>% 
  mutate(lag_d = cut(as.integer(lag), 
                     breaks = c(-0.5, 0.5, 10.5, 20.5, 30.5, 40.5, 50.5, 1000),
                     labels = c("0", "1-10", "11-20", "21-30", "31-40", "41-50", ">50"))) %>% 
  group_by(journal, lag_d) %>% 
  tally() %>% 
  mutate(share = 100*n/sum(n), 
         share = round(share, 1)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = lag_d, values_from = share, values_fill = 0) %>% 
  select(journal, `0`, `1-10`, `11-20`, `21-30`, everything())

stats <- tbl_lag %>% 
  filter(year == 2020) %>% 
  group_by(journal) %>% 
  summarise(N = n(),
            mean = mean(lag, na.rm = T),
            median = median(lag, na.rm = T)) %>% 
  mutate(across(-journal, ~round(as.numeric(.x), 2))) %>% 
  arrange(-N)

hux20 <- stats %>% 
  left_join(shares) %>% 
  huxtable()