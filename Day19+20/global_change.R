library(tidyverse)
library(ggstream)
library(showtext)

showtext_opts(dpi = 300)
showtext_auto()

grord <- c("Climatological", "Meteorological", "Hydrological", 
           "Geophysical", "Biological", "Extra-terrestrial")

data_yr %>% 
   filter(year < 2019) %>%
     mutate(subgr = factor(subgr, grord)) %>%
ggplot(aes(date, occr, fill = subgr)) +
  geom_stream(bw = 0.4, show.legend = FALSE) +
  scale_x_date(date_breaks = "10 year", date_labels = "%Y", 
               expand = expansion()) +
  scale_y_continuous(position = "right",
                     breaks = seq(-150, 150, 50)) +
  scale_fill_manual(values = c("#08519c", "#6baed6", "#c6dbef",
                               "#7f2704", "#006d2c", "black"))+
  labs(x = "", y = "", title = "Worldwide disasters", 
       subtitle = "1900 – 2018",
       caption = "Dominic Royé (@dr_xeo) | Data: EM-DAT") +
  theme_minimal(base_family = "Montserrat") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(colour = "white", size = 26, vjust = -10, hjust = .02),
        plot.subtitle =  element_text(colour = "white", size = 16, vjust = -1, hjust = .015),
        plot.caption = element_text(colour = "white"),
        axis.text.y =  element_text(colour = "white"),
        axis.text.x =  element_text(colour = "white"),
        axis.ticks.x = element_line(colour = "white"),
        axis.ticks.y.right = element_line(colour = "white"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black")) 

ggsave("disasters.pdf")