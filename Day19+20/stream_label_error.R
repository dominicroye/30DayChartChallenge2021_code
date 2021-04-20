library(tidyverse)
library(ggstream)
library(showtext)

load("emdat.RData")

grord <- c("Climatological", "Meteorological", "Hydrological", 
           "Geophysical", "Biological", "Extra-terrestrial")

data_yr %>% 
  filter(year < 2019) %>%
  mutate(subgr = factor(subgr, grord)) %>%
  ggplot(aes(date, occr, fill = subgr)) +
  geom_stream(bw = 0.4, show.legend = FALSE) +
  geom_stream_label(aes(label = subgr))