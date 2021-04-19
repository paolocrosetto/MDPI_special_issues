## analysing the data from the editorial history scrape

## NOTE: this is REALLY MESSY, do not try this at home

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
library(gt)
library(kableExtra)
library(waffle)
library(ggtext)

# getting basic journal data
journals <- read_csv("journals.csv")

# getting to the right place
setwd("Editorial history/")

## 1. create a plot with the basic overview for each journal
# function to create a plot of special issues + of editorial lags for each journal
analyse_journal <- function(data, jo) {
  df <- data
  SI <- df %>% 
    filter(year != 2015) %>% 
    filter(year != 2021)

  
  # dealing with sections and collections
  SI$SI[str_detect(SI$SI, "Collection")] <- "Section & Collection"
  SI$SI[str_detect(SI$SI, "Section")] <- "Section & Collection"
  SI$SI[str_detect(SI$SI, "Special")] <- "Special Issue"
  
  cleandata <- SI
  
  cleandata %>% 
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
         title = "Articles")
  
  LAG <- cleandata %>% 
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

journals <- journals %>% rownames_to_column()

for (i in journals$rowname %>% as.integer()) {
  ti <- journals$title[i]
  jo <- journals$journal[i]
  cat(jo,", ", ti, "\n")
  try(makeplot(jo, ti))
}


## 2. overall analaysis

# getting all the data
setwd("data/")
tbl <-
  list.files(pattern = "*.csv") %>% 
  map_df(~read_csv(.))
setwd("..")
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

# filtering to the events we care about and getting rid of 2015 and 2021 (they aren't there but for errors, but better safe than sorry)
tbl <- tbl %>% 
  filter(year != 2015) %>% 
  filter(year != 2021) %>% 
  filter(event == "Received" | event == "Accepted") %>% 
  mutate(SI = as.factor(SI),
         SI = fct_recode(SI, "Special Issue" = "1", "Normal Issue" = "0"))

# cleaning
tbl <- tbl %>% 
  select(-volume, -issue, -nevents)


## PLOT: articles by SI, over all journals: bar version
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
       caption = "data: MDPI -- code: @paolocrosetto")+
  scale_y_continuous(breaks = c(0,50000,100000), labels = c("0", "50000", "100000"))
ggsave("overall_articles_SI.png", width = 10, height = 8, units = "in", dpi = 300)

## PLOT: articles by SI, over all journals: waffle version
df <- tbl %>% 
  select(year, DOI, SI) %>% distinct() %>% 
  group_by(year, SI, .drop = F) %>% 
  tally()

scaler <- 200
ggplot(df %>% mutate(n = n/scaler), aes(fill = SI, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, 
              flip = T, radius = unit(0.7, units = "mm")
  ) +
  facet_wrap(~year, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(breaks = seq(10, 80, 20), 
                     labels = function(x) format(x * 10*scaler, scientific = F),
                     expand = c(0,0)) +
  scale_fill_brewer(name=NULL, palette = "Set1", direction = -1) +
  coord_equal() +
  labs(
    title = "<span style = 'color:#377EB8;'>Normal</span> and <span style = 'color:#E41A1C;'>Special</span> Issue articles at MDPI, 2016-20",
    subtitle = sprintf("74 journals with an Impact Factor. One square = %s articles", scaler),
    x = "",
    y = "",
    caption = "Data:MDPI -- code: @paolocrosetto"
  ) +
  #theme_minimal(base_family = "Roboto Condensed") +
  theme_ipsum_rc()+
  theme(panel.grid = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.ticks.y = element_line(),
        plot.title = element_markdown(),
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE))
ggsave("overall_articles_SI_waffle.png", height = 9, width = 15, units = "in", dpi = 300)


## TABLE: growth and share of Special Issues for selected journals 

# selected journals
  selected_jo <- tbl %>% 
    filter(year == 2016) %>% 
    group_by(journal) %>% 
    mutate(nbyjour = n()) %>% 
    filter(nbyjour>50) %>% 
    select(journal) %>% 
    distinct() %$% 
    journal    
  
# table: share of SIs by year + groth of articles and SIs 
# With sparklines!

# prepare the data
data <- tbl %>% 
    select(year, journal, DOI, SI) %>% 
    distinct() %>% 
    group_by(journal, year, SI, .drop = F) %>% 
    tally() %>% 
    pivot_wider(names_from = SI, values_from = n) %>% 
    mutate(shareSI = `Special Issue`/(`Normal Issue` + `Special Issue`)) %>% 
    mutate(N = sum(`Normal Issue` + `Special Issue`)) %>% 
    select(journal, year, shareSI, N) 

# resolution of problem with journal names
data <- data %>% 
  mutate(journal = case_when(journal == "Appl. Sci." ~ "Applied Sciences",
                             journal == "Brain Sci." ~ "Brain Sciences",
                             journal == "Curr. Oncol." ~ "Current Oncology",
                             journal == "Brain Sci." ~ "Brain Sciences",
                             journal == "Remote Sens." ~ "Remote Sensing",
                             journal == "Mar. Drugs" ~ "Marine Drugs",
                             journal == "J. Mar. Sci. Eng." ~ "Journal of Marine Science and Engineering",
                             journal == "J. Fungi" ~ "Journal of Fungi",
                             journal == "J. Clin. Med." ~ "Journal of Clinical Medicine",
                             journal == "ISPRS Int. J. Geo-Inf." ~ "ISPRS International Journal of Geo-Information",
                             journal == "Int. J. Mol. Sci." ~ "International Journal of Molecular Sciences",
                             journal == "Int. J. Environ. Res. Public Health" ~ "International Journal of Environmental Research and Public Health",
                             TRUE ~ journal))

# more formatting
add_data <- data %>% 
  mutate(shareSI = round(100*shareSI, 2)) %>% 
  pivot_wider(names_from = year, values_from = c(shareSI, N)) %>% 
  select(journal, N_2016, N_2020, starts_with("share"))

# number of special issues
SI <- read_csv("../summary.csv")

# merge the correct names and find the number of SIs in 2016 and 2020
SIs <- journals %>% 
  select(title, journal) %>% 
  right_join(SI, by = "journal") %>% 
  select(journal = title, `2016`, `2020`)

# plot for the growth in SIs
add_si_plot <- journals %>% 
  select(title, journal) %>% 
  right_join(SI, by = "journal") %>% 
  select(-journal) %>% 
  select(journal = title, everything()) %>% 
  pivot_longer(-journal, names_to = "year", values_to = "SI") %>%
  mutate(year = as.integer(year)) %>% 
  filter(year >= 2016 & year <=2020) %>% 
  group_by(journal) %>% 
  summarise(d = list(SI)) %>% 
  mutate(
    plot = map(d, ~kableExtra::spec_plot(.x, same_lim = TRUE, width = 300, height = 70)),
    plot = map(plot, "svg_text"),
    plot = map(plot, gt::html)
  ) %>% 
  select(-d) %>% 
  left_join(SIs) %>% 
  select(journal, `2016`, SIplot = plot, `2020`)

SItable <- data %>% 
  group_by(journal) %>% 
  summarise(d = list(N))  %>% 
  mutate(
    plot = map(d, ~kableExtra::spec_plot(.x, same_lim = TRUE, width = 300, height = 70)),
    plot = map(plot, "svg_text"),
    plot = map(plot, gt::html)
  ) %>% 
  select(-d) %>% 
  left_join(add_data) %>% 
  left_join(add_si_plot) %>% 
  filter(journal %in% selected_jo) %>% 
  select(journal, N_2016, plot, N_2020, starts_with("share"), `2016`, SIplot, `2020`) %>% 
  gt() %>% 
  tab_header(
    title = md("**Journal growth and share of Special Issues at selected MDPI journals**")
  ) %>% 
  tab_source_note(
    source_note = "Source: MDPI website, crawled in April 2021."
  ) %>% 
  tab_spanner(label = "Number of articles", 
              columns = c(2:4)) %>% 
  tab_spanner(label = "Share of Special issues",
              columns = c(5:9)) %>% 
  tab_spanner(label = "Number of Special issues",
                columns = c(10:12)) %>%     
  cols_label(journal = "", 
             N_2016 = "'16",
             plot = "growth",
             N_2020 = "'20",
             shareSI_2016 = "'16",
             shareSI_2017 = "'17",
             shareSI_2018 = "'18",
             shareSI_2019 = "'19",
             shareSI_2020 = "'20",
             `2016` = "'16",
             SIplot = "growth",
             `2020` = "'20") %>% 
  data_color(columns = c(5:9),
             scales::col_numeric(domain = c(40,100), 
                                 palette = "Reds",
                                 reverse = F))
SItable %>% 
  gtsave("table_SI.png")

### 3. Analysis of turnaround times (LAG between submission and acceptance)

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
  geom_text(data = tbl_lag %>% 
              filter(lag>0 & lag<150) %>% 
              group_by(year, revised) %>% summarise(N = n()) %>% mutate(N = paste("N = ", N, sep = "")), 
            inherit.aes = F,
            aes(as.factor(year), 155, label = N), 
            hjust = 0.4)+
  facet_wrap(~revised, ncol = 1)+
  labs(x = "", y = "Days", 
       title = "Submission to acceptance at MDPI - 74 journals with an IF",
       subtitle = "Lag distribution",
       caption = "data: MDPI -- code: @paolocrosetto")
ggsave(plot = p1, "lag_distro.png", width = 12, height = 9, units = "in", dpi = 300)


# shares of observations excluded from the above plot
tbl_lag %>% 
  mutate(included = lag>0 & lag<150) %>% 
  group_by(year, included) %>% 
  tally() %>% 
  spread(included, n) %>% 
  mutate(share = 100*`FALSE`/ (`FALSE` + `TRUE`))

# st.dev of lag across years and SIs
tbl_lag %>% 
  group_by(year, SI) %>% 
  summarise(m = mean(lag, na.rm = T), sd = sd(lag, na.rm = T)) %>% 
  arrange(SI, year)

## take 2: distribution at selected journals in 2016 and 2020, with ggridges

tbl_lag %>% 
  filter(year == 2016 | year == 2020) %>% 
  filter(journal %in% selected_jo) %>% 
  filter(lag<150) %>% 
  mutate(year = as.factor(year), 
         fct_relevel(year, "2020")) %>% 
  ggplot(aes(lag, reorder(journal, lag), fill = reorder(year, lag)))+
  geom_density_ridges(color = "white", alpha = .9)+
  scale_fill_brewer(palette = "Set2", direction = 1, name = "")+
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



### Generating individual distribution plots by journal
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

## overall table with data on the distribution of lags (2020 only)
shares <- tbl_lag %>% 
  filter(journal %in% selected_jo) %>% 
  filter(year == 2020) %>% 
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
  filter(journal %in% selected_jo) %>% 
  filter(year == 2020) %>% 
  group_by(journal) %>% 
  summarise(N = n(),
            mean = mean(lag, na.rm = T),
            median = median(lag, na.rm = T)) %>% 
  mutate(across(-journal, ~round(as.numeric(.x), 2))) %>% 
  arrange(-N)

lagtable <- stats %>% 
  left_join(shares) %>% 
  select(-`NA`) %>% 
  gt() %>% 
  tab_header(
    title = md("**Turnaround statistics for selected MDPI journals, 2020**")
  ) %>% 
  tab_source_note(
    source_note = "Source: MDPI website, crawled in April 2021."
  ) %>% 
  tab_spanner(label = "Share of papers with turnaround in ... days",
              columns = c(5:11)) %>% 
  tab_spanner(label = "Days", 
              columns = c(3,4)) %>% 
  cols_label(journal = "") %>% 
  data_color(columns = c(5:11),
             scales::col_numeric(domain = c(0,50), 
                             palette = RColorBrewer::brewer.pal(11, "RdYlGn")[1:5], 
                             reverse = T))

lagtable %>% 
  gtsave("table_LAG.png")