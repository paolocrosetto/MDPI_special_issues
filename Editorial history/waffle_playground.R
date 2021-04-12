library(dplyr)
library(waffle)
library(hrbrthemes)
library(ggtext)

storms %>% 
  filter(year >= 2010) %>% 
  count(year, status) -> storms_df


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
ggsave("prova_waffle.png", height = 9, width = 15, units = "in", dpi = 300)


# how much of an increase for SI and non SI? 
df %>% 
  filter(year %in% c(2016, 2020)) %>% 
  spread(year, n) %>% 
  mutate(increase = (`2020` - `2016`) / `2016`)
# NI increased 2.64 times in 5 years
# SI increased 7.52 times in 5 years -> 3 times as fast

# overall share of SI in time
df %>% 
  spread(SI, n) %>% 
  mutate(shareSI = `Special Issue` / (`Normal Issue` + `Special Issue`))
# increased from 79 to 90 % (rounded to integer, else its 0.789 to 0.897)