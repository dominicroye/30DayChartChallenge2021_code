# packages
library(tidyverse)
library(lubridate)
library(ragg)

# functions
source("lightning_download.R")

# download lightning data
rayos <- lightning_download("01/01/2015", "31/12/2020")

# save
save(rayos, file = "lightning_data.RData")

# set English time for my OS
Sys.setlocale("LC_TIME", "English")

# summarise by year and months
temp <- mutate(rayos, yr = year(date) %>% factor(2020:2015), 
                      mo = month(date, label = TRUE)) %>% 
        count(yr, mo)

# grid for x
grid_x <- tibble(x = unique(temp$mo), 
                 y = rep(0, 12), 
                 xend = levels(temp$mo), 
                 yend = rep(80000, 12))

# color ramp
col_neo <- colorRampPalette(c("#ffd300", "#FC6E22"))

# plot
m <-  ggplot(temp) + 
    geom_hline(yintercept = seq(0, 80000, 20000), colour = "white") +
    geom_segment(data = grid_x , 
                 aes(x = x, y = y, xend = xend, yend = yend), 
                 linetype = "dashed", col = "white", size = .2) +
    geom_col(aes(mo, n, fill = yr), alpha = .8, colour = "transparent") + 
    annotate(x = unique(temp$mo), y = 90000, 
             label = unique(temp$mo), 
             geom = "text", 
             colour = "white", 
             family = "Montserrat",
             fontface = "bold") +
    annotate(x = unique(temp$mo)[1], y = seq(20000, 80000, 20000), 
            label = str_c(seq(20, 80, 20), "k"), 
            geom = "text", 
            colour = "white", 
            vjust = 1.7,
            hjust = .35,
            family = "Montserrat", 
            fontface = "italic") +
    coord_polar(start = -0.26) +
  scale_fill_manual(values = col_neo(6)) +
  labs(title = "Number of lightning strikes in the north-west Spain",
       caption = "Dominic Royé (@dr_xeo) | Data: Meteogalicia") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        legend.margin = margin(0,10,0, -25),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(colour = "#ffd300", family = "Montserrat", size = 18, hjust = .5, vjust = -5),
        plot.caption = element_text(colour = "#ffd300", family = "Montserrat", vjust = 20, size = 10),
        legend.text = element_text(colour = "white", family = "Montserrat"),
        panel.grid.major = element_blank())

# export with ragg
agg_png("parttowhole.png", width = 7.9, height = 7.8, units = 'in', res = 300)
  
  print(m)
  
invisible(dev.off())