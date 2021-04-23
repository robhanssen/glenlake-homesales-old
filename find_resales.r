library(tidyverse)
library(lubridate)

homesales <- read_csv("data/homesales_processeddata.csv")

dupes <- homesales %>% 
            filter(duplicated(address) | duplicated(address, fromLast = TRUE)) %>% 
            arrange(address) %>% 
            group_by(address) %>%
            mutate(lagtime = listingdate - lag(saledate), lagtime=time_length(lagtime, "months")) %>%
            ungroup() %>%
            select(address:hometype, lagtime)

dupes %>%   filter(!is.na(lagtime)) %>% 
            arrange(hometype,address) %>% 
            ggplot  + aes(x=fct_reorder(address,lagtime), y=lagtime, fill=hometype) + 
            geom_col() + 
            labs(y="Time betweens sale and next listing (in months)", x="Address", fill="Type of home") + 
            scale_y_continuous(breaks=12*1:10) +
            coord_flip()

ggsave("graphs/home-resale-time.pdf", width=8, height=11)